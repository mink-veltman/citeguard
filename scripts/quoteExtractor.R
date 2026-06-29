options(shiny.maxRequestSize = 1000 * 1024^2)  # 1000 MB

library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(DT)
library(tibble)
library(httr2)
library(metacheck)
library(htmltools)

`%||%` <- function(x, y) if (is.null(x)) y else x

# --------------------------------------------------
# Helpers
# --------------------------------------------------

collapse_authors <- function(authors_obj) {
  if (is.null(authors_obj)) return(NA_character_)
  if (length(authors_obj) == 0) return(NA_character_)
  
  out <- tryCatch({
    if (is.data.frame(authors_obj)) {
      cols <- names(authors_obj)
      
      if (all(c("given", "family") %in% cols)) {
        names_vec <- trimws(paste(authors_obj$given, authors_obj$family))
        names_vec <- names_vec[nzchar(names_vec)]
        return(paste(names_vec, collapse = "; "))
      }
      
      if ("family" %in% cols) {
        names_vec <- trimws(authors_obj$family)
        names_vec <- names_vec[nzchar(names_vec)]
        return(paste(names_vec, collapse = "; "))
      }
      
      return(paste(apply(authors_obj, 1, paste, collapse = " "), collapse = "; "))
    }
    
    if (is.list(authors_obj)) {
      parts <- purrr::map_chr(authors_obj, function(a) {
        if (is.list(a)) {
          given <- a$given %||% ""
          family <- a$family %||% ""
          full <- trimws(paste(given, family))
          if (nzchar(full)) return(full)
          
          txt <- paste(unlist(a), collapse = " ")
          txt <- str_squish(txt)
          if (nzchar(txt)) return(txt)
        }
        
        txt <- paste(unlist(a), collapse = " ")
        str_squish(txt)
      })
      
      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) return(NA_character_)
      return(paste(parts, collapse = "; "))
    }
    
    txt <- paste(as.character(unlist(authors_obj)), collapse = "; ")
    txt <- str_squish(txt)
    if (!nzchar(txt)) NA_character_ else txt
  }, error = function(e) {
    txt <- paste(as.character(unlist(authors_obj)), collapse = "; ")
    txt <- str_squish(txt)
    if (!nzchar(txt)) NA_character_ else txt
  })
  
  out
}

extract_apa_like_authors <- function(citation_text) {
  if (is.na(citation_text) || !nzchar(citation_text)) return(NA_character_)
  
  txt <- citation_text |>
    str_replace_all("\\(|\\)|\\[|\\]", " ") |>
    str_replace_all("&", " and ") |>
    str_squish()
  
  txt <- str_replace_all(txt, "\\b(19|20)\\d{2}[a-z]?\\b", " ")
  txt <- str_replace_all(txt, "pp?\\.\\s*\\d+(-\\d+)?", " ")
  txt <- str_replace_all(txt, "\\bet al\\.?\\b", " ")
  txt <- str_replace_all(txt, ",", " ")
  txt <- str_replace_all(txt, ";", " ")
  txt <- str_squish(txt)
  
  tokens <- str_extract_all(txt, "\\b[A-Z][A-Za-z'\\-]+\\b")[[1]]
  if (length(tokens) == 0) return(NA_character_)
  
  paste(unique(tokens), collapse = "; ")
}

check_url <- function(url, timeout_sec = 20) {
  tryCatch({
    resp <- request(url) |>
      req_user_agent("shiny-metacheck-local") |>
      req_timeout(timeout_sec) |>
      req_perform()
    
    list(
      ok = TRUE,
      status = resp_status(resp),
      body = resp_body_string(resp)
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      status = NA_integer_,
      body = conditionMessage(e)
    )
  })
}

check_grobid_alive <- function(grobid_url) {
  grobid_url <- sub("/+$", "", grobid_url)
  check_url(paste0(grobid_url, "/api/isalive"))
}

safe_pdf2grobid <- function(pdf_path, grobid_url) {
  tryCatch({
    metacheck::pdf2grobid(
      filename = pdf_path,
      save_path = tempdir(),
      grobid_url = grobid_url
    )
  }, error = function(e) {
    stop(
      paste0(
        "pdf2grobid failed.\n",
        "GROBID URL: ", grobid_url, "\n",
        "File: ", basename(pdf_path), "\n",
        "Error: ", conditionMessage(e)
      ),
      call. = FALSE
    )
  })
}

safe_extract_zip <- function(zipfile, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  
  if (requireNamespace("archive", quietly = TRUE)) {
    ok <- tryCatch({
      archive::archive_extract(zipfile, dir = exdir)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(list(ok = TRUE, method = "archive"))
  }
  
  ok <- tryCatch({
    utils::unzip(zipfile, exdir = exdir)
    TRUE
  }, warning = function(w) FALSE,
  error = function(e) FALSE)
  if (ok) return(list(ok = TRUE, method = "utils::unzip"))
  
  ok <- tryCatch({
    system2(
      "unzip",
      args = c("-o", zipfile, "-d", exdir),
      stdout = TRUE,
      stderr = TRUE
    )
    TRUE
  }, error = function(e) FALSE)
  if (ok) return(list(ok = TRUE, method = "system unzip"))
  
  list(ok = FALSE, method = NA_character_)
}

collect_uploaded_pdfs <- function(upload_df) {
  if (is.null(upload_df) || nrow(upload_df) == 0) {
    return(list(
      pdfs = tibble(
        pdf_path = character(),
        pdf_name = character(),
        source_upload = character()
      ),
      messages = character()
    ))
  }
  
  out <- list()
  messages <- character()
  
  for (i in seq_len(nrow(upload_df))) {
    original_name <- upload_df$name[i]
    datapath <- upload_df$datapath[i]
    ext <- tolower(tools::file_ext(original_name))
    
    if (ext == "pdf") {
      out[[length(out) + 1]] <- tibble(
        pdf_path = datapath,
        pdf_name = original_name,
        source_upload = original_name
      )
      messages <- c(messages, paste0("Added PDF: ", original_name))
    } else if (ext == "zip") {
      unzip_dir <- file.path(
        tempdir(),
        paste0("unzipped_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", i)
      )
      
      extracted <- safe_extract_zip(datapath, unzip_dir)
      
      if (!isTRUE(extracted$ok)) {
        messages <- c(messages, paste0("Failed to extract ZIP: ", original_name))
        next
      }
      
      messages <- c(
        messages,
        paste0("Extracted ZIP with ", extracted$method, ": ", original_name)
      )
      
      pdfs <- list.files(
        unzip_dir,
        pattern = "\\.pdf$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      
      if (length(pdfs) == 0) {
        messages <- c(messages, paste0("No PDFs found inside ZIP: ", original_name))
      } else {
        out[[length(out) + 1]] <- tibble(
          pdf_path = pdfs,
          pdf_name = basename(pdfs),
          source_upload = original_name
        )
        messages <- c(messages, paste0("PDFs found in ZIP ", original_name, ": ", length(pdfs)))
      }
    } else {
      messages <- c(messages, paste0("Skipped unsupported file: ", original_name))
    }
  }
  
  pdf_tbl <- if (length(out) == 0) {
    tibble(
      pdf_path = character(),
      pdf_name = character(),
      source_upload = character()
    )
  } else {
    bind_rows(out) %>%
      distinct(pdf_path, .keep_all = TRUE)
  }
  
  list(
    pdfs = pdf_tbl,
    messages = messages
  )
}

highlight_citation_marker <- function(text, citation) {
  if (is.na(text) || !nzchar(text)) return("")
  safe_text <- htmlEscape(text)
  
  if (is.na(citation) || !nzchar(citation)) {
    return(safe_text)
  }
  
  citation_trim <- str_squish(citation)
  
  # Numeric-style markers: [1], [12], (1), superscript-like plain numbers are too risky
  is_numeric_style <- str_detect(citation_trim, "^\\s*\\[?[0-9]+([,;\\-–\\s]+[0-9]+)*\\]?\\s*$") ||
    str_detect(citation_trim, "^\\s*\\([0-9]+([,;\\-–\\s]+[0-9]+)*\\)\\s*$")
  
  escaped_citation <- htmlEscape(citation_trim)
  pattern <- stringr::regex(stringr::fixed(escaped_citation), ignore_case = FALSE)
  
  out <- safe_text
  
  # First try exact citation marker
  out2 <- tryCatch({
    str_replace_all(
      out,
      fixed(escaped_citation),
      "<span style='background-color:#ffef88; color:#b30000; font-weight:700; padding:1px 3px; border-radius:3px;'>\\0</span>"
    )
  }, error = function(e) out)
  
  if (!identical(out2, out)) return(out2)
  
  # If numeric style, try some bracket/spacing-normalized variants
  if (is_numeric_style) {
    digits <- str_extract_all(citation_trim, "[0-9]+")[[1]]
    if (length(digits) > 0) {
      variants <- unique(c(
        citation_trim,
        paste0("[", paste(digits, collapse = ","), "]"),
        paste0("[", paste(digits, collapse = ", "), "]"),
        paste0("(", paste(digits, collapse = ","), ")"),
        paste0("(", paste(digits, collapse = ", "), ")")
      ))
      
      for (v in variants) {
        escaped_v <- htmlEscape(v)
        candidate <- str_replace_all(
          out,
          fixed(escaped_v),
          "<span style='background-color:#ffef88; color:#b30000; font-weight:700; padding:1px 3px; border-radius:3px;'>\\0</span>"
        )
        if (!identical(candidate, out)) return(candidate)
      }
    }
  }
  
  out
}

extract_citation_contexts <- function(pdf_path,
                                      pdf_name,
                                      minus = 1,
                                      plus = 1,
                                      grobid_url) {
  xml_path <- safe_pdf2grobid(pdf_path, grobid_url = grobid_url)
  paper <- metacheck::read(xml_path)
  
  if (is.null(paper$xrefs) || nrow(paper$xrefs) == 0) {
    return(tibble(
      file = pdf_name,
      citation = NA_character_,
      citation_authors_guess = NA_character_,
      sentence = NA_character_,
      expanded_text = NA_character_,
      sentence_html = NA_character_,
      expanded_text_html = NA_character_,
      ref_title = NA_character_,
      ref_authors = NA_character_,
      ref_year = NA_character_,
      ref_doi = NA_character_,
      section = NA_character_,
      div = NA_integer_,
      p = NA_integer_,
      s = NA_integer_,
      note = "No xrefs found in parsed paper."
    ))
  }
  
  xrefs <- paper$xrefs %>%
    filter(type == "bibr")
  
  if (nrow(xrefs) == 0) {
    return(tibble(
      file = pdf_name,
      citation = NA_character_,
      citation_authors_guess = NA_character_,
      sentence = NA_character_,
      expanded_text = NA_character_,
      sentence_html = NA_character_,
      expanded_text_html = NA_character_,
      ref_title = NA_character_,
      ref_authors = NA_character_,
      ref_year = NA_character_,
      ref_doi = NA_character_,
      section = NA_character_,
      div = NA_integer_,
      p = NA_integer_,
      s = NA_integer_,
      note = "No bibliography citations found (type == 'bibr')."
    ))
  }
  
  bib_tbl <- if (!is.null(paper$bib) && nrow(paper$bib) > 0) {
    paper$bib %>%
      mutate(
        ref_authors = purrr::map_chr(authors, collapse_authors),
        ref_title   = as.character(title %||% NA_character_),
        ref_year    = as.character(year %||% NA),
        ref_doi     = as.character(doi %||% NA)
      ) %>%
      select(any_of(c("xref_id", "ref_title", "ref_authors", "ref_year", "ref_doi")))
  } else {
    tibble(
      xref_id = character(),
      ref_title = character(),
      ref_authors = character(),
      ref_year = character(),
      ref_doi = character()
    )
  }
  
  out <- xrefs %>%
    left_join(bib_tbl, by = "xref_id") %>%
    transmute(
      file = pdf_name,
      citation = as.character(contents %||% NA_character_),
      citation_authors_guess = purrr::map_chr(
        as.character(contents %||% NA_character_),
        extract_apa_like_authors
      ),
      sentence = as.character(text %||% NA_character_),
      expanded_text = as.character(text %||% NA_character_),
      ref_title,
      ref_authors,
      ref_year,
      ref_doi,
      section = as.character(section %||% NA_character_),
      div = suppressWarnings(as.integer(div)),
      p = suppressWarnings(as.integer(p)),
      s = suppressWarnings(as.integer(s)),
      note = NA_character_
    )
  
  expanded_out <- tryCatch({
    full_sentences <- metacheck::search_text(paper)
    
    citation_rows <- full_sentences %>%
      inner_join(
        xrefs %>% select(xref_id, contents, text, section, div, p, s),
        by = c("section", "div", "p", "s", "text")
      )
    
    if (nrow(citation_rows) == 0) {
      out %>%
        mutate(note = "Could not match xrefs back to search_text() output for expansion.")
    } else {
      expanded <- metacheck::expand_text(
        citation_rows,
        paper,
        minus = minus,
        plus = plus
      )
      
      expanded_tbl <- expanded %>%
        transmute(
          section = as.character(section %||% NA_character_),
          div = suppressWarnings(as.integer(div)),
          p = suppressWarnings(as.integer(p)),
          s = suppressWarnings(as.integer(s)),
          expanded_text_new = if ("expanded" %in% names(.)) as.character(expanded) else as.character(text)
        )
      
      out %>%
        left_join(expanded_tbl, by = c("section", "div", "p", "s")) %>%
        mutate(expanded_text = coalesce(expanded_text_new, expanded_text)) %>%
        select(-expanded_text_new)
    }
  }, error = function(e) {
    out %>%
      mutate(note = paste("Expansion failed:", conditionMessage(e)))
  })
  
  expanded_out %>%
    rowwise() %>%
    mutate(
      sentence_html = highlight_citation_marker(sentence, citation),
      expanded_text_html = highlight_citation_marker(expanded_text, citation)
    ) %>%
    ungroup()
}

# --------------------------------------------------
# UI
# --------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      table.dataTable tbody td {
        vertical-align: top;
      }
      .wide-text {
        white-space: normal !important;
        word-break: break-word;
      }
    "))
  ),
  
  titlePanel("Citation Context Extractor + Cited Author Search"),
  
  fluidRow(
    column(
      width = 3,
      wellPanel(
        fileInput(
          "files",
          "Upload PDF(s) or ZIP file(s)",
          multiple = TRUE,
          accept = c(".pdf", ".zip")
        ),
        
        textInput(
          "grobid_url",
          "GROBID URL",
          value = "http://localhost:8070"
        ),
        
        numericInput("minus", "Sentences before citation", value = 1, min = 0, max = 10),
        numericInput("plus", "Sentences after citation", value = 1, min = 0, max = 10),
        
        tags$hr(),
        
        textInput(
          "citation_filter",
          "Filter in-text citation string",
          placeholder = "e.g. Smith, [12], 2020"
        ),
        
        textInput(
          "author_filter",
          "Filter cited author",
          placeholder = "e.g. Kahneman"
        ),
        
        textInput(
          "title_filter",
          "Filter reference title",
          placeholder = "e.g. prospect theory"
        ),
        
        checkboxInput(
          "only_matches",
          "Show only rows with matched bibliography info",
          value = FALSE
        ),
        
        tags$hr(),
        
        actionButton("test_grobid", "Test GROBID"),
        actionButton("test_pdf2grobid", "Test first PDF"),
        br(), br(),
        actionButton("process", "Process files", class = "btn-primary"),
        br(), br(),
        downloadButton("download_csv", "Download CSV")
      )
    ),
    
    column(
      width = 9,
      fluidRow(
        column(3, wellPanel(strong("Uploaded items"), textOutput("stat_uploaded"))),
        column(3, wellPanel(strong("PDFs found"), textOutput("stat_pdfs"))),
        column(3, wellPanel(strong("Processed"), textOutput("stat_processed"))),
        column(3, wellPanel(strong("Success / Fail"), textOutput("stat_success_fail")))
      ),
      
      tabsetPanel(
        tabPanel("Results", DTOutput("results_table")),
        tabPanel("Live log", verbatimTextOutput("live_log")),
        tabPanel("Diagnostics", verbatimTextOutput("diagnostic_text")),
        tabPanel("Status", verbatimTextOutput("status_text"))
      )
    )
  )
)

# --------------------------------------------------
# Server
# --------------------------------------------------

server <- function(input, output, session) {
  diagnostics <- reactiveVal("No diagnostics run yet.")
  process_status <- reactiveVal("No processing run yet.")
  raw_data <- reactiveVal(tibble())
  live_log <- reactiveVal(character())
  stats <- reactiveValues(
    uploaded = 0,
    pdfs = 0,
    processed = 0,
    success = 0,
    fail = 0
  )
  
  append_log <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste(..., collapse = ""))
    live_log(c(live_log(), msg))
  }
  
  reset_log_and_stats <- function() {
    live_log(character())
    stats$uploaded <- 0
    stats$pdfs <- 0
    stats$processed <- 0
    stats$success <- 0
    stats$fail <- 0
  }
  
  observeEvent(input$test_grobid, {
    grobid_url <- trimws(input$grobid_url)
    res <- check_grobid_alive(grobid_url)
    
    msg <- paste(
      "GROBID TEST",
      "-----------",
      paste("URL:", paste0(sub("/+$", "", grobid_url), "/api/isalive")),
      paste("OK:", res$ok),
      paste("Status:", res$status %||% NA),
      "",
      "Response body:",
      substr(res$body %||% "", 1, 1000),
      sep = "\n"
    )
    
    diagnostics(msg)
  })
  
  observeEvent(input$test_pdf2grobid, {
    req(input$files)
    
    grobid_url <- trimws(input$grobid_url)
    collected <- collect_uploaded_pdfs(input$files)
    pdfs <- collected$pdfs
    req(nrow(pdfs) > 0)
    
    pdf_path <- pdfs$pdf_path[1]
    pdf_name <- pdfs$pdf_name[1]
    
    msg <- tryCatch({
      xml_path <- safe_pdf2grobid(pdf_path, grobid_url)
      
      paste(
        c(
          "PDF2GROBID TEST",
          "----------------",
          paste("File:", pdf_name),
          paste("GROBID URL:", grobid_url),
          "Result: SUCCESS",
          paste("XML path:", xml_path),
          "",
          "Upload scan messages:",
          collected$messages
        ),
        collapse = "\n"
      )
    }, error = function(e) {
      paste(
        c(
          "PDF2GROBID TEST",
          "----------------",
          paste("File:", pdf_name),
          paste("GROBID URL:", grobid_url),
          "Result: FAILED",
          "",
          conditionMessage(e),
          "",
          "Upload scan messages:",
          collected$messages
        ),
        collapse = "\n"
      )
    })
    
    diagnostics(msg)
  })
  
  observeEvent(input$process, {
    req(input$files)
    
    reset_log_and_stats()
    raw_data(tibble())
    process_status("Processing started...")
    
    append_log("Starting processing run")
    stats$uploaded <- nrow(input$files)
    
    collected <- collect_uploaded_pdfs(input$files)
    pdfs <- collected$pdfs
    grobid_url <- trimws(input$grobid_url)
    
    for (m in collected$messages) {
      append_log(m)
    }
    
    stats$pdfs <- nrow(pdfs)
    
    status_messages <- c(
      paste0("Using GROBID URL: ", grobid_url),
      paste0("Uploaded items: ", nrow(input$files)),
      collected$messages
    )
    
    if (nrow(pdfs) == 0) {
      append_log("No PDFs found after scanning uploads")
      status_messages <- c(status_messages, "No PDFs could be found or extracted from the uploaded files.")
      process_status(paste(status_messages, collapse = "\n"))
      return(NULL)
    }
    
    append_log("Found ", nrow(pdfs), " PDF(s) to process")
    status_messages <- c(status_messages, paste0("PDFs found: ", nrow(pdfs)))
    
    all_results <- vector("list", nrow(pdfs))
    
    withProgress(message = "Processing PDFs", value = 0, {
      for (i in seq_len(nrow(pdfs))) {
        pdf_path <- pdfs$pdf_path[i]
        pdf_name <- pdfs$pdf_name[i]
        source_upload <- pdfs$source_upload[i]
        
        detail_msg <- paste0("Processing ", i, "/", nrow(pdfs), ": ", pdf_name)
        append_log(detail_msg)
        incProgress(1 / nrow(pdfs), detail = detail_msg)
        
        status_messages <- c(
          status_messages,
          paste0("Processing: ", pdf_name, " (from ", source_upload, ")")
        )
        
        res <- tryCatch({
          out <- extract_citation_contexts(
            pdf_path = pdf_path,
            pdf_name = pdf_name,
            minus = input$minus,
            plus = input$plus,
            grobid_url = grobid_url
          ) %>%
            mutate(source_upload = source_upload, .before = file)
          
          stats$success <- stats$success + 1
          append_log("Success: ", pdf_name, " | rows extracted: ", nrow(out))
          out
        }, error = function(e) {
          stats$fail <- stats$fail + 1
          append_log("FAILED: ", pdf_name, " | ", conditionMessage(e))
          
          status_messages <<- c(
            status_messages,
            paste0("Failed: ", pdf_name, " -> ", conditionMessage(e))
          )
          
          tibble(
            source_upload = source_upload,
            file = pdf_name,
            citation = NA_character_,
            citation_authors_guess = NA_character_,
            sentence = NA_character_,
            expanded_text = NA_character_,
            sentence_html = NA_character_,
            expanded_text_html = NA_character_,
            ref_title = NA_character_,
            ref_authors = NA_character_,
            ref_year = NA_character_,
            ref_doi = NA_character_,
            section = NA_character_,
            div = NA_integer_,
            p = NA_integer_,
            s = NA_integer_,
            note = paste("ERROR:", conditionMessage(e))
          )
        })
        
        all_results[[i]] <- res
        stats$processed <- i
      }
    })
    
    raw_data(bind_rows(all_results))
    append_log("Processing finished")
    process_status(paste(status_messages, collapse = "\n"))
  })
  
  filtered_data <- reactive({
    dat <- raw_data()
    if (nrow(dat) == 0) return(dat)
    
    if (nzchar(trimws(input$citation_filter))) {
      dat <- dat %>%
        filter(str_detect(
          coalesce(citation, ""),
          regex(trimws(input$citation_filter), ignore_case = TRUE)
        ))
    }
    
    if (nzchar(trimws(input$author_filter))) {
      dat <- dat %>%
        filter(
          str_detect(
            coalesce(ref_authors, ""),
            regex(trimws(input$author_filter), ignore_case = TRUE)
          ) |
            str_detect(
              coalesce(citation_authors_guess, ""),
              regex(trimws(input$author_filter), ignore_case = TRUE)
            )
        )
    }
    
    if (nzchar(trimws(input$title_filter))) {
      dat <- dat %>%
        filter(str_detect(
          coalesce(ref_title, ""),
          regex(trimws(input$title_filter), ignore_case = TRUE)
        ))
    }
    
    if (isTRUE(input$only_matches)) {
      dat <- dat %>%
        filter(
          !is.na(ref_authors) | !is.na(ref_title) | !is.na(ref_year) | !is.na(ref_doi)
        )
    }
    
    dat
  })
  
  output$stat_uploaded <- renderText(stats$uploaded)
  output$stat_pdfs <- renderText(stats$pdfs)
  output$stat_processed <- renderText(paste0(stats$processed, "/", stats$pdfs))
  output$stat_success_fail <- renderText(paste0(stats$success, " / ", stats$fail))
  
  output$live_log <- renderText({
    paste(live_log(), collapse = "\n")
  })
  
  output$diagnostic_text <- renderText({
    diagnostics()
  })
  
  output$status_text <- renderText({
    process_status()
  })
  
  output$results_table <- renderDT({
    dat <- filtered_data()
    
    datatable(
      dat,
      escape = FALSE,
      extensions = c("Buttons"),
      filter = "top",
      rownames = FALSE,
      class = "cell-border stripe compact",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = list(
          list(
            extend = "colvis",
            text = "Show / hide columns"
          )
        ),
        columnDefs = list(
          list(
            targets = which(names(dat) %in% c("sentence_html", "expanded_text_html")) - 1,
            className = "wide-text"
          ),
          list(
            targets = which(names(dat) %in% c("sentence", "expanded_text")) - 1,
            visible = FALSE
          )
        )
      )
    )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("citation_contexts_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)