server <- function(input, output, session) {
  init_db_if_missing()

  diagnostics    <- reactiveVal("No diagnostics run yet.")
  process_status <- reactiveVal("No processing run yet.")
  llm_status     <- reactiveVal("")
  llm_cancel     <- reactiveVal(FALSE)
  raw_data       <- reactiveVal(tibble::tibble())
  live_log       <- reactiveVal(character())
  known_tbl_open    <- reactiveVal(FALSE)
  taxonomy_tbl_open <- reactiveVal(FALSE)

  stats <- reactiveValues(uploaded = 0, pdfs = 0, processed = 0, success = 0, fail = 0)

  reports_db_rv <- reactiveVal(read_reports_db())

  observe({
    if (isTRUE(known_tbl_open())) {
      shinyjs::show("known_tbl_body")
      updateActionLink(session, "toggle_known_tbl", label = "Hide")
    } else {
      shinyjs::hide("known_tbl_body")
      updateActionLink(session, "toggle_known_tbl", label = "Show")
    }
  })

  observeEvent(input$toggle_known_tbl, {
    known_tbl_open(!isTRUE(known_tbl_open()))
  })

  observe({
    if (isTRUE(taxonomy_tbl_open())) {
      shinyjs::show("taxonomy_tbl_body")
      updateActionLink(session, "toggle_taxonomy_tbl", label = "Hide")
    } else {
      shinyjs::hide("taxonomy_tbl_body")
      updateActionLink(session, "toggle_taxonomy_tbl", label = "Show")
    }
  })

  observeEvent(input$toggle_taxonomy_tbl, {
    taxonomy_tbl_open(!isTRUE(taxonomy_tbl_open()))
  })

  known_miscited_df <- reactive({
    summarise_known_miscitations(reports_db_rv())
  })

  output$known_miscited_table <- DT::renderDT({
    DT::datatable(
      known_miscited_df(),
      rownames = FALSE,
      filter   = "top",
      options  = list(pageLength = 8, scrollX = TRUE, dom = "tip")
    )
  })

  output$mistake_table_out <- renderTable({
    mistake_table
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

  append_log <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste(..., collapse = ""))
    live_log(c(live_log(), msg))
    message(msg)
  }

  reset_log_and_stats <- function() {
    live_log(character())
    stats$uploaded  <- 0
    stats$pdfs      <- 0
    stats$processed <- 0
    stats$success   <- 0
    stats$fail      <- 0
  }

  observeEvent(input$test_grobid, {
    grobid_url <- trimws(input$grobid_url)
    res <- check_grobid_alive(grobid_url)
    diagnostics(paste(
      "GROBID TEST", "-----------",
      paste("URL:", paste0(sub("/+$", "", grobid_url), "/api/isalive")),
      paste("OK:", res$ok),
      paste("Status:", res$status %||% NA),
      "", "Response body:",
      substr(res$body %||% "", 1, 1000),
      sep = "\n"
    ))
  })

  observeEvent(input$test_pdf2grobid, {
    req(input$files)
    grobid_url <- trimws(input$grobid_url)
    alive      <- check_grobid_alive(grobid_url)

    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      diagnostics(paste(
        "PDF2GROBID TEST", "----------------",
        paste("GROBID URL:", grobid_url),
        "Result: FAILED",
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "", "Response body:", substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      ))
      return()
    }

    collected <- collect_uploaded_pdfs(input$files)
    pdfs      <- collected$pdfs
    req(nrow(pdfs) > 0)

    msg <- tryCatch({
      xml_path <- safe_pdf2grobid(pdfs$pdf_path[1], grobid_url)
      paste(c(
        "PDF2GROBID TEST", "----------------",
        paste("File:", pdfs$pdf_name[1]),
        paste("GROBID URL:", grobid_url),
        "Result: SUCCESS",
        paste("XML path:", xml_path),
        "", "Upload scan messages:", collected$messages
      ), collapse = "\n")
    }, error = function(e) {
      paste(c(
        "PDF2GROBID TEST", "----------------",
        paste("File:", pdfs$pdf_name[1]),
        paste("GROBID URL:", grobid_url),
        "Result: FAILED", "",
        conditionMessage(e),
        "", "Upload scan messages:", collected$messages
      ), collapse = "\n")
    })
    diagnostics(msg)
  })

  observeEvent(input$process, {
    req(input$files)

    reset_log_and_stats()
    raw_data(tibble::tibble())
    process_status("Checking GROBID connection...")

    # Reload database so annotation uses the latest reports
    reports_db_rv(read_reports_db())

    grobid_url <- trimws(input$grobid_url)
    alive      <- check_grobid_alive(grobid_url)

    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      msg <- paste(
        "Processing stopped.",
        paste("GROBID URL:", grobid_url),
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "", "Response body:", substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      )
      process_status(msg)
      append_log(msg)
      return()
    }

    process_status("Processing started...")
    append_log("Starting processing run")
    stats$uploaded <- nrow(input$files)

    collected       <- collect_uploaded_pdfs(input$files)
    pdfs            <- collected$pdfs
    status_messages <- c(
      paste0("Using GROBID URL: ", grobid_url),
      paste0("Uploaded items: ", nrow(input$files)),
      collected$messages
    )

    for (m in collected$messages) append_log(m)
    stats$pdfs <- nrow(pdfs)

    if (nrow(pdfs) == 0) {
      append_log("No PDFs found after scanning uploads")
      process_status(paste(c(status_messages, "No PDFs could be found or extracted from the uploaded files."), collapse = "\n"))
      return(NULL)
    }

    append_log("Found ", nrow(pdfs), " PDF(s) to process")
    status_messages <- c(status_messages, paste0("PDFs found: ", nrow(pdfs)))

    all_results <- vector("list", nrow(pdfs))

    shiny::withProgress(message = "Processing PDFs", value = 0, {
      for (i in seq_len(nrow(pdfs))) {
        pdf_path      <- pdfs$pdf_path[i]
        pdf_name      <- pdfs$pdf_name[i]
        source_upload <- pdfs$source_upload[i]

        detail_msg <- paste0("Processing ", i, "/", nrow(pdfs), ": ", pdf_name)
        append_log(detail_msg)
        shiny::incProgress(1 / nrow(pdfs), detail = detail_msg)
        status_messages <- c(status_messages, paste0("Processing: ", pdf_name, " (from ", source_upload, ")"))

        res <- tryCatch({
          out <- extract_citation_contexts(
            pdf_path   = pdf_path,
            pdf_name   = pdf_name,
            grobid_url = grobid_url
          ) |> dplyr::mutate(source_upload = source_upload, .before = file)

          append_log("Rows extracted for ", pdf_name, ": ", nrow(out))
          append_log("Non-empty citations: ", sum(!is.na(out$citation) & nzchar(out$citation)))
          append_log("Non-empty sentences: ", sum(!is.na(out$sentence) & nzchar(out$sentence)))
          append_log("Non-empty ref titles: ", sum(!is.na(out$ref_title) & nzchar(out$ref_title)))

          stats$success <- stats$success + 1
          append_log("Success: ", pdf_name, " | rows extracted: ", nrow(out))
          out
        }, error = function(e) {
          stats$fail <- stats$fail + 1
          append_log("FAILED: ", pdf_name, " | ", conditionMessage(e))
          status_messages <<- c(status_messages, paste0("Failed: ", pdf_name, " -> ", conditionMessage(e)))

          tibble::tibble(
            source_upload = source_upload, file = pdf_name,
            citation = NA_character_, sentence = NA_character_, expanded_text = NA_character_,
            ref_title = NA_character_, ref_authors = NA_character_,
            ref_author_last_names = NA_character_, ref_author_key = NA_character_,
            ref_lead_author = NA_character_, ref_title_key = NA_character_,
            ref_year = NA_character_, ref_doi = NA_character_,
            citation_lead_author_guess = NA_character_, citation_year_guess = NA_character_,
            section = NA_character_, div = NA_integer_, p = NA_integer_, s = NA_integer_,
            note = paste("ERROR:", conditionMessage(e))
          )
        })

        all_results[[i]] <- res
        stats$processed  <- i
      }
    })

    combined <- dplyr::bind_rows(all_results)
    combined <- annotate_with_db_matches(combined, known_miscited_df())

    raw_data(combined)
    append_log("Processing finished")
    process_status(paste(status_messages, collapse = "\n"))

    shinyjs::hide("empty_state")
    shinyjs::show("results_area")
  })

  filtered_data <- reactive({
    dat <- raw_data()
    if (nrow(dat) == 0) return(dat)

    if ("db_known_miscited_paper" %in% names(dat)) {
      dat <- dat |> dplyr::filter(db_known_miscited_paper == "Yes")
    }

    dat
  })

  output$process_msg <- renderText({
    s <- process_status()
    if (s == "No processing run yet.") return("")
    if (startsWith(s, "Processing started") || startsWith(s, "Checking GROBID")) return("Analyzing…")
    n_flagged <- sum(!is.na(raw_data()$db_known_miscited_paper) &
                     raw_data()$db_known_miscited_paper == "Yes", na.rm = TRUE)
    n_total   <- nrow(raw_data())
    if (n_total == 0) return("No citations extracted.")
    paste0("Checked ", n_total, " citation", if (n_total != 1) "s" else "", ".")
  })

  output$results_summary <- renderText({
    n_flagged <- nrow(filtered_data())
    n_total   <- nrow(raw_data())
    if (n_total == 0) return("")
    if (n_flagged == 0)
      return(paste0("No known miscitations found across ", n_total,
                    " citation", if (n_total != 1) "s" else "", "."))
    paste0("Found ", n_flagged, " citation", if (n_flagged != 1) "s" else "",
           " to papers with known miscitations (out of ", n_total, " total).")
  })

  output$results_cards <- renderUI({
    dat     <- filtered_data()
    full_db <- reports_db_rv()

    if (nrow(dat) == 0) return(NULL)

    make_code_badge <- function(cd, tt) {
      paste0(
        "<span style='background:#c0392b;color:#fff;border-radius:3px;padding:1px 5px;",
        "font-size:0.78em;font-weight:bold;margin-right:3px'>",
        htmltools::htmlEscape(cd), "</span>",
        if (nzchar(tt)) paste0(
          "<span style='color:#555;font-size:0.83em;margin-right:8px'>",
          htmltools::htmlEscape(tt), "</span>"
        ) else ""
      )
    }

    cards_html <- paste(vapply(seq_len(nrow(dat)), function(i) {
      title_str   <- htmltools::htmlEscape(trimws(dplyr::coalesce(as.character(dat$ref_title[i]),   "")))
      authors_str <- htmltools::htmlEscape(trimws(dplyr::coalesce(as.character(dat$ref_authors[i]), "")))
      year_str    <- htmltools::htmlEscape(trimws(dplyr::coalesce(as.character(dat$ref_year[i]),    "")))
      file_str    <- htmltools::htmlEscape(trimws(dplyr::coalesce(as.character(dat$file[i]),        "")))
      fallback    <- htmltools::htmlEscape(trimws(dplyr::coalesce(as.character(dat$citation[i]),    "")))

      display_title <- if (nzchar(title_str)) title_str else fallback
      meta_parts    <- c(if (nzchar(authors_str)) authors_str else NULL,
                         if (nzchar(year_str))    year_str    else NULL)
      display_meta  <- paste(meta_parts, collapse = " · ")

      # highlighted in-text quote
      et      <- trimws(dplyr::coalesce(as.character(dat$expanded_text[i]), ""))
      ts      <- if ("target_sentence" %in% names(dat))
        trimws(dplyr::coalesce(as.character(dat$target_sentence[i]), "")) else ""
      et_html <- htmltools::htmlEscape(et)
      ts_html <- trimws(htmltools::htmlEscape(ts))

      quote_html <- if (!nzchar(et_html) && !nzchar(ts_html)) {
        ""
      } else if (!nzchar(ts_html)) {
        paste0("<div class='cg-quote'>", et_html, "</div>")
      } else {
        marked_ts <- paste0("<mark>", ts_html, "</mark>")
        if (!nzchar(et_html) || et_html == ts_html) {
          paste0("<div class='cg-quote'>", marked_ts, "</div>")
        } else {
          et_marked <- if (grepl(ts_html, et_html, fixed = TRUE))
            sub(ts_html, paste0("<mark>", ts_html, "</mark>"), et_html, fixed = TRUE)
          else et_html
          paste0(
            "<div class='cg-quote'>", marked_ts,
            "<details><summary style='color:#888;font-size:0.82em;cursor:pointer;padding-top:3px'>",
            "&#9660;&nbsp;full context</summary>",
            "<div style='margin-top:4px;color:#444;font-style:normal'>", et_marked, "</div>",
            "</details></div>"
          )
        }
      }

      # collect DB records for this citation
      n        <- as.integer(dplyr::coalesce(as.character(dat$db_report_count[i]), "0"))
      if (is.na(n)) n <- 0L
      rids_raw <- trimws(dplyr::coalesce(as.character(dat$db_report_ids[i]), ""))
      rid_vec  <- if (nzchar(rids_raw) && rids_raw != "NA")
        trimws(unlist(strsplit(rids_raw, ";"))) else character(0)
      records  <- if (length(rid_vec) > 0 && nrow(full_db) > 0)
        full_db[full_db$report_id %in% rid_vec, , drop = FALSE]
      else NULL

      if (!is.null(records) && nrow(records) > 0) {
        role_ord <- dplyr::case_when(
          records$reporter_role == "Author of the cited work" ~ 1L,
          records$reporter_role == "Co-author"                ~ 2L,
          TRUE                                                 ~ 3L
        )
        has_votes  <- "vote_count" %in% names(records)
        vote_score <- if (has_votes)
          -suppressWarnings(as.integer(records$vote_count))
        else rep(0L, nrow(records))
        vote_score[is.na(vote_score)] <- 0L
        records <- records[order(role_ord, vote_score), , drop = FALSE]
      }

      record_cards_html <- if (!is.null(records) && nrow(records) > 0) {
        has_votes <- "vote_count" %in% names(records)
        paste(vapply(seq_len(nrow(records)), function(r) {
          rec        <- records[r, , drop = FALSE]
          role       <- trimws(dplyr::coalesce(rec$reporter_role, ""))
          role_color <- dplyr::case_when(
            role == "Author of the cited work" ~ "#1a6e2e",
            role == "Co-author"                ~ "#155a8a",
            TRUE                               ~ "#666"
          )
          role_label <- dplyr::case_when(
            role == "Author of the cited work" ~ "Author",
            role == "Co-author"                ~ "Co-author",
            TRUE                               ~ "Reader"
          )
          role_badge <- paste0(
            "<span style='background:", role_color, ";color:#fff;border-radius:3px;",
            "padding:1px 5px;font-size:0.75em;font-weight:bold;margin-right:5px'>",
            role_label, "</span>"
          )
          vote_html <- if (has_votes) {
            vc <- suppressWarnings(as.integer(rec$vote_count))
            vc <- if (is.na(vc)) 0L else vc
            paste0("<span style='float:right;font-size:0.8em;color:#888'>&#9650;&nbsp;",
                   vc, " vote", if (vc != 1L) "s" else "", "</span>")
          } else ""

          codes_r  <- trimws(unlist(strsplit(dplyr::coalesce(rec$mistake_codes, ""), ";")))
          codes_r  <- codes_r[nzchar(codes_r) & codes_r != "NA"]
          titles_r <- mistake_table$mistake_title[match(codes_r, mistake_table$mistake_code)]
          badges_r <- if (length(codes_r) > 0)
            paste(vapply(seq_along(codes_r), function(j)
              make_code_badge(trimws(codes_r[j]), if (!is.na(titles_r[j])) titles_r[j] else ""),
              character(1)), collapse = "")
          else ""

          quot <- trimws(dplyr::coalesce(rec$quoted_or_paraphrased_text, ""))
          if (is.na(quot) || quot == "NA") quot <- ""
          why  <- trimws(dplyr::coalesce(rec$why_incorrect, ""))
          if (is.na(why)  || why  == "NA") why  <- ""
          corr <- trimws(dplyr::coalesce(rec$correct_citation, ""))
          if (is.na(corr) || corr == "NA") corr <- ""

          paste0(
            "<div style='border:1px solid #ddd;border-radius:4px;padding:8px 10px;margin:6px 0;background:#fafafa'>",
            "<div style='margin-bottom:5px'>", role_badge, vote_html,
            if (nzchar(badges_r)) paste0("<span>", badges_r, "</span>") else "",
            "<div style='clear:both'></div></div>",
            if (nzchar(quot)) paste0(
              "<div style='margin:4px 0;font-size:0.85em'><strong>Cited as:</strong> &ldquo;",
              htmltools::htmlEscape(quot), "&rdquo;</div>"
            ) else "",
            if (nzchar(why)) paste0(
              "<div style='border-left:3px solid #e67e22;padding:3px 8px;margin:4px 0;",
              "font-size:0.85em;color:#444'><strong>Why incorrect:</strong> ",
              htmltools::htmlEscape(why), "</div>"
            ) else "",
            if (nzchar(corr)) paste0(
              "<div style='border-left:3px solid #27ae60;padding:3px 8px;margin:4px 0;",
              "font-size:0.85em;color:#444'><strong>How to cite correctly:</strong> ",
              htmltools::htmlEscape(corr), "</div>"
            ) else "",
            "</div>"
          )
        }, character(1)), collapse = "")
      } else ""

      # summary-level code badges (union across all records)
      all_codes_raw <- trimws(dplyr::coalesce(as.character(dat$db_mistake_codes[i]), ""))
      if (is.na(all_codes_raw)) all_codes_raw <- ""
      all_codes  <- trimws(unlist(strsplit(all_codes_raw, ";")))
      all_codes  <- all_codes[nzchar(all_codes) & all_codes != "NA"]
      all_titles <- mistake_table$mistake_title[match(all_codes, mistake_table$mistake_code)]
      summary_badges <- if (length(all_codes) > 0)
        paste(vapply(seq_along(all_codes), function(j)
          make_code_badge(trimws(all_codes[j]), if (!is.na(all_titles[j])) all_titles[j] else ""),
          character(1)), collapse = "")
      else ""

      expandable <- if (nzchar(record_cards_html)) paste0(
        "<details><summary style='color:#555;font-size:0.83em;cursor:pointer;",
        "padding-top:6px;font-weight:500'>&#9660;&nbsp;View ", n,
        " known miscitation", if (n != 1L) "s" else "", "</summary>",
        "<div style='margin-top:6px'>", record_cards_html, "</div></details>"
      ) else ""

      paste0(
        "<div class='cg-card'>",
        "<div class='cg-card-header'>",
        "<div>",
        "<div class='cg-paper-title'>", display_title, "</div>",
        if (nzchar(display_meta)) paste0("<div class='cg-paper-meta'>", display_meta, "</div>") else "",
        "</div>",
        if (nzchar(file_str)) paste0("<span class='cg-file'>", file_str, "</span>") else "",
        "</div>",
        if (nzchar(quote_html)) "<div class='cg-cite-label'>Cited in your document</div>" else "",
        quote_html,
        "<div class='cg-warning-bar'>&#9888; Known miscited paper &mdash; ",
        n, " report", if (n != 1L) "s" else "", " in database</div>",
        if (nzchar(summary_badges))
          paste0("<div style='margin:6px 0'>", summary_badges, "</div>") else "",
        expandable,
        "</div>"
      )
    }, character(1)), collapse = "\n")

    HTML(paste0("<div class='cg-cards'>", cards_html, "</div>"))
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("citation_contexts_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )

  observeEvent(input$run_llm, {
    llm_status("")
    dat <- raw_data()

    if (is.null(dat) || nrow(dat) == 0) {
      llm_status("No data loaded. Process PDFs first.")
      return()
    }

    api_key <- trimws(input$groq_api_key)
    if (!nzchar(api_key)) api_key <- Sys.getenv("GROQ_API_KEY")
    if (!nzchar(api_key)) {
      llm_status("No Groq API key provided. Enter your key above or set GROQ_API_KEY in .Renviron.")
      return()
    }

    flagged_count <- sum(!is.na(dat$db_known_miscited_paper) & dat$db_known_miscited_paper == "Yes", na.rm = TRUE)
    if (flagged_count == 0) {
      llm_status("No rows matched the miscitation database. Nothing to check.")
      return()
    }

    selected_model <- input$llm_model
    llm_status(paste0("Running LLM check on ", flagged_count, " row(s)... please wait."))
    llm_cancel(FALSE)
    shinyjs::disable("run_llm")
    shinyjs::show("stop_llm")

    session$onFlushed(function() {
      llm_out <- tryCatch(
        run_llm_check(dat, model = selected_model, api_key = api_key,
                      status_fn = llm_status,
                      cancel_fn = function() isolate(llm_cancel())),
        error = function(e) {
          llm_status(paste("LLM error:", conditionMessage(e)))
          NULL
        }
      )

      if (!is.null(llm_out)) {
        result       <- llm_out$df
        raw_data(result)
        n_done       <- length(llm_out$flagged_idx)
        stopped_early <- isTRUE(llm_out$cancelled)
        verdict_lines <- if (n_done > 0) {
          paste(vapply(seq_along(llm_out$flagged_idx), function(j) {
            i         <- llm_out$flagged_idx[j]
            cit       <- trimws(result$citation[i] %||% paste("Row", i))
            cit_short <- if (nchar(cit) > 60) paste0(substr(cit, 1, 60), "...") else cit
            raw       <- llm_out$raw_answers[j]
            normalized <- result$llm_verdict[i]
            paste0("  • ", cit_short,
                   "\n    Raw: ", if (is.na(raw) || !nzchar(raw)) "(empty)" else raw,
                   "\n    Verdict: ", if (is.na(normalized) || !nzchar(normalized)) "(empty)" else normalized)
          }, character(1)), collapse = "\n")
        } else ""
        prefix <- if (stopped_early) paste0("Stopped early. LLM checked ", n_done, " row(s) (partial results kept).\n")
                  else paste0("Done. LLM checked ", n_done, " row(s).\n")
        llm_status(paste0(
          prefix, verdict_lines, "\n\n",
          "DEBUG: ", llm_out$debug_info, "\n\n",
          "Text sent to LLM:\n", paste(llm_out$texts, collapse = "\n---\n")
        ))
      }

      shinyjs::enable("run_llm")
      shinyjs::hide("stop_llm")
    }, once = TRUE)
  })

  observeEvent(input$stop_llm, {
    llm_cancel(TRUE)
    llm_status("Stopping after current request completes...")
  })

  output$llm_status <- renderText(llm_status())
}
