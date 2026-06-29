# download_pdfs.R
#
# Download PDFs from DOIs or a .bib file.
# Stage 1: Unpaywall (open access)
# Stage 2: Institutional access via university network IP
#
# SETUP: install.packages("httr2")
#        Set UNPAYWALL_EMAIL in .Renviron
#
# USAGE:
#   source("download_pdfs.R")
#   results <- download_pdfs(bib_path = "refs.bib",  email = "you@uni.nl")
#   results <- download_pdfs(dois = c("10.1037/..."), email = "you@uni.nl")
#
#   # Debug a specific failing DOI:
#   download_pdfs(dois = "10.1016/j.neucom.2025.130334",
#                 email = "you@uni.nl", verbose = TRUE)

library(httr2)

`%||%` <- function(x, y) if (is.null(x)) y else x

BROWSER_UA <- paste0(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)

# ---- BibTeX parsing -------------------------------------------------------

parse_bib_dois <- function(bib_path) {
  text <- paste(readLines(bib_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  hits <- regmatches(text, gregexpr('(?i)\\bdoi\\s*=\\s*[{"]([^}"\\n]+)[}"]', text, perl = TRUE))[[1]]
  dois <- sub('(?i)\\bdoi\\s*=\\s*[{"]([^}"\\n]+)[}"]', "\\1", hits, perl = TRUE)
  dois <- sub("^https?://(dx\\.)?doi\\.org/", "", trimws(dois), ignore.case = TRUE)
  unique(dois[nzchar(dois)])
}

# ---- Low-level helpers ----------------------------------------------------

is_pdf_bytes <- function(b) length(b) >= 4 && identical(b[1:4], as.raw(c(0x25,0x50,0x44,0x46)))

doi_to_filename <- function(doi) paste0(gsub("[^A-Za-z0-9._-]", "_", doi), ".pdf")

# A request pre-loaded with browser headers (errors never throw — always check status manually)
browser_req <- function(url, referer = NULL) {
  r <- request(url) |>
    req_user_agent(BROWSER_UA) |>
    req_headers(
      Accept            = "text/html,application/xhtml+xml,application/pdf,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9"
    ) |>
    req_timeout(20) |>
    req_error(is_error = \(r) FALSE)
  if (!is.null(referer)) r <- r |> req_headers(Referer = referer)
  r
}

# Resolve DOI → landing URL.
# Makes ONE request; if the final page is a meta-refresh stub (Elsevier linkinghub,
# APA doi.apa.org) follow one more hop. Never more than 2 requests total.
resolve_to_landing <- function(doi) {
  tryCatch({
    resp <- browser_req(paste0("https://doi.org/", doi)) |>
      req_timeout(15) |>
      req_perform()

    landing <- resp$url
    ct      <- tryCatch(resp_content_type(resp), error = \(e) "")

    if (grepl("html", ct, ignore.case = TRUE)) {
      html <- tryCatch(resp_body_string(resp), error = \(e) "")
      m    <- regexpr(
        '(?i)<meta[^>]+http-equiv=["\']refresh["\'][^>]+content=["\'][^"\']*url=([^"\'\\s;>]+)',
        html, perl = TRUE
      )
      if (m != -1) {
        hit      <- regmatches(html, m)
        next_url <- trimws(sub('(?i).*url=([^"\'\\s;>]+)', "\\1", hit, perl = TRUE))
        if (nzchar(next_url)) {
          if (!grepl("^https?://", next_url))
            next_url <- paste0(sub("(https?://[^/]+).*", "\\1", landing), next_url)
          landing <- next_url
        }
      }
    }

    landing
  }, error = function(e) paste0("https://doi.org/", doi))
}

# ---- Stage 1: Unpaywall ---------------------------------------------------

get_unpaywall_pdf_url <- function(doi, email) {
  tryCatch({
    resp <- request(
      paste0("https://api.unpaywall.org/v2/", utils::URLencode(doi, reserved = TRUE))
    ) |>
      req_url_query(email = email) |>
      req_user_agent("pdf-downloader-r/1.0") |>
      req_timeout(15) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()

    if (resp_status(resp) == 404) return(list(ok = FALSE, reason = "not in Unpaywall"))
    if (resp_status(resp) != 200) return(list(ok = FALSE, reason = paste("Unpaywall HTTP", resp_status(resp))))

    data      <- resp_body_json(resp)
    locations <- Filter(Negate(is.null), c(list(data$best_oa_location), data$oa_locations))
    for (loc in locations) {
      url <- loc$url_for_pdf %||% ""
      if (nzchar(url))
        return(list(ok = TRUE, url = url, via = paste0("unpaywall/", loc$host_type %||% "?")))
    }
    if (!isTRUE(data$is_oa)) return(list(ok = FALSE, reason = "not open access"))
    list(ok = FALSE, reason = "OA but no direct PDF URL")
  }, error = function(e) list(ok = FALSE, reason = conditionMessage(e)))
}

# ---- Stage 2: Institutional -----------------------------------------------

# Build a list of publisher-specific PDF URL candidates from the final landing URL
publisher_pdf_candidates <- function(doi, landing_url) {
  host <- tolower(sub("^https?://([^/]+).*", "\\1", landing_url))

  # Elsevier / ScienceDirect  (landing_url is already the ScienceDirect article page)
  if (grepl("sciencedirect\\.com", host)) {
    pii <- sub(".*/pii/([A-Z0-9]+).*", "\\1", landing_url, ignore.case = TRUE, perl = TRUE)
    if (nzchar(pii) && pii != landing_url)
      return(paste0("https://www.sciencedirect.com/science/article/pii/", pii,
                    "/pdfft?isDTMRedir=true&download=true"))
  }

  # Springer / Nature
  if (grepl("link\\.springer\\.com|nature\\.com", host))
    return(c(
      paste0("https://link.springer.com/content/pdf/", doi, ".pdf"),
      paste0("https://link.springer.com/article/", doi, "/fulltext.pdf")
    ))

  # Wiley
  if (grepl("wiley\\.com", host))
    return(c(
      paste0("https://onlinelibrary.wiley.com/doi/pdfdirect/", doi),
      paste0("https://onlinelibrary.wiley.com/doi/pdf/", doi)
    ))

  # Taylor & Francis
  if (grepl("tandfonline\\.com", host))
    return(paste0("https://www.tandfonline.com/doi/pdf/", doi, "?needAccess=true"))

  # SAGE
  if (grepl("sagepub\\.com", host))
    return(paste0("https://journals.sagepub.com/doi/pdf/", doi))

  # APA PsycNET — try fulltext path; often works on institutional network
  if (grepl("apa\\.org|psycnet\\.apa\\.org", host))
    return(c(
      paste0("https://doi.apa.org/fulltext/", doi, ".pdf"),
      paste0("https://psycnet.apa.org/fulltext/", doi, ".pdf")
    ))

  # Oxford University Press
  if (grepl("academic\\.oup\\.com|oxfordjournals\\.org", host)) {
    path <- sub("https?://[^/]+", "", landing_url)
    return(paste0("https://academic.oup.com", path, "/pdf"))
  }

  # IEEE Xplore
  if (grepl("ieeexplore\\.ieee\\.org", host)) {
    arnumber <- sub(".*/document/([0-9]+).*", "\\1", landing_url, perl = TRUE)
    if (nzchar(arnumber) && arnumber != landing_url)
      return(paste0("https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=", arnumber))
  }

  # ACM Digital Library
  if (grepl("dl\\.acm\\.org", host))
    return(paste0("https://dl.acm.org/doi/pdf/", doi))

  # Annual Reviews
  if (grepl("annualreviews\\.org", host))
    return(paste0("https://www.annualreviews.org/doi/pdf/", doi))

  # PLoS
  if (grepl("journals\\.plos\\.org", host)) {
    journal <- sub("https?://journals\\.plos\\.org/([^/]+).*", "\\1", landing_url)
    return(paste0("https://journals.plos.org/", journal, "/article/file?id=", doi, "&type=printable"))
  }

  # PubMed Central
  if (grepl("ncbi\\.nlm\\.nih\\.gov/pmc", host)) {
    pmc <- sub(".*/articles/(PMC[0-9]+).*", "\\1", landing_url)
    if (nzchar(pmc) && pmc != landing_url)
      return(paste0("https://www.ncbi.nlm.nih.gov/pmc/articles/", pmc, "/pdf/"))
  }

  character(0)
}

# All-in-one institutional download.
# Resolves DOI (one request + optional meta-refresh hop), establishes a browser
# session, scrapes meta tag and tries publisher patterns — all sharing one
# cookie jar that stays alive for the duration of this function.
download_institutional <- function(doi, dest_path, verbose = FALSE) {
  vcat <- function(...) if (verbose) cat("     DBG:", ..., "\n")

  # 1. Resolve DOI to real landing page (max 2 HTTP requests)
  landing <- resolve_to_landing(doi)
  vcat("Landing:", landing)

  # 2. Hit landing page once — this both establishes the session cookie AND
  #    gives us the HTML for the meta tag scrape.
  cookie_file <- tempfile(fileext = ".txt")
  on.exit(unlink(cookie_file), add = TRUE)

  landing_html <- tryCatch({
    resp <- browser_req(landing) |>
      req_cookie_preserve(cookie_file) |>
      req_timeout(20) |>
      req_perform()
    vcat("Landing status:", resp_status(resp))
    if (resp_status(resp) != 200) return("")
    tryCatch(resp_body_string(resp), error = \(e) "")
  }, error = function(e) { vcat("Landing error:", conditionMessage(e)); "" })

  # Inner helper: try downloading a URL with current session cookies
  try_pdf <- function(url, label) {
    vcat("Trying", label, ":", url)
    tryCatch({
      resp <- browser_req(url, referer = landing) |>
        req_cookie_preserve(cookie_file) |>
        req_options(connecttimeout = 10) |>
        req_timeout(30) |>
        req_perform()
      status <- resp_status(resp)
      ct     <- tryCatch(resp_content_type(resp), error = \(e) "?")
      vcat("  status:", status, "| ct:", ct)
      if (status != 200) return(NULL)
      body <- resp_body_raw(resp)
      if (!is_pdf_bytes(body)) { vcat("  not a PDF"); return(NULL) }
      writeBin(body, dest_path)
      list(ok = TRUE, size_kb = round(length(body) / 1024), via = label)
    }, error = function(e) { vcat("  error:", conditionMessage(e)); NULL })
  }

  # 3. citation_pdf_url meta tag (from the HTML we already have)
  pdf_url <- tryCatch({
    if (!nzchar(landing_html)) return(NULL)
    m <- regexpr(
      '(?i)<meta[^>]+name=["\']citation_pdf_url["\'][^>]+content=["\']([^"\']+)["\']|<meta[^>]+content=["\']([^"\']+)["\'][^>]+name=["\']citation_pdf_url["\']',
      landing_html, perl = TRUE
    )
    if (m == -1) { vcat("No citation_pdf_url meta tag"); return(NULL) }
    url <- trimws(sub('.*content=["\']([^"\']+)["\'].*', "\\1",
                      regmatches(landing_html, m), perl = TRUE))
    if (nzchar(url)) url else NULL
  }, error = function(e) NULL)

  vcat("Meta tag URL:", if (is.null(pdf_url)) "none" else pdf_url)
  if (!is.null(pdf_url)) {
    result <- try_pdf(pdf_url, "meta tag")
    if (!is.null(result)) return(result)
  }

  # 4. Publisher-specific URL patterns
  candidates <- publisher_pdf_candidates(doi, landing)
  vcat("Publisher candidates:", length(candidates))
  for (url in candidates) {
    result <- try_pdf(url, paste0("pattern(", sub("^https?://([^/]+).*", "\\1", url), ")"))
    if (!is.null(result)) return(result)
  }

  list(ok = FALSE, reason = paste0("no PDF found (landing: ", landing, ")"))
}

# ---- Main -----------------------------------------------------------------

#' @param dois              Character vector of DOIs
#' @param bib_path          Path to a .bib file
#' @param doi_file          Plain-text file, one DOI per line
#' @param output_dir        Folder to save PDFs (created if missing)
#' @param email             Email for Unpaywall API
#' @param use_institutional Try institutional network access as fallback
#' @param delay_sec         Pause between DOIs (be polite)
#' @param overwrite         Re-download files that already exist?
#' @param verbose           Print debug info for every attempt
#' @return Invisibly: data frame doi|status|file|url|via|reason
download_pdfs <- function(
  dois              = NULL,
  bib_path          = NULL,
  doi_file          = NULL,
  output_dir        = "downloaded_pdfs",
  email             = Sys.getenv("UNPAYWALL_EMAIL"),
  use_institutional = TRUE,
  delay_sec         = 1,
  overwrite         = FALSE,
  verbose           = FALSE
) {
  if (!nzchar(trimws(email)))
    stop("Provide email= or set UNPAYWALL_EMAIL in .Renviron.", call. = FALSE)

  all_dois <- character(0)
  if (!is.null(bib_path)) {
    message("Parsing .bib: ", bib_path)
    bd <- parse_bib_dois(bib_path)
    message("  ", length(bd), " DOI(s) found")
    all_dois <- c(all_dois, bd)
  }
  if (!is.null(doi_file)) {
    fd <- trimws(readLines(doi_file, warn = FALSE))
    fd <- fd[nzchar(fd) & !startsWith(fd, "#")]
    fd <- sub("^https?://(dx\\.)?doi\\.org/", "", fd, ignore.case = TRUE)
    message("Reading ", doi_file, ": ", length(fd), " DOI(s)")
    all_dois <- c(all_dois, fd)
  }
  if (!is.null(dois)) {
    dois <- trimws(sub("^https?://(dx\\.)?doi\\.org/", "", dois, ignore.case = TRUE))
    all_dois <- c(all_dois, dois[nzchar(dois)])
  }

  all_dois <- unique(all_dois[nzchar(all_dois)])
  if (length(all_dois) == 0) stop("No DOIs found.", call. = FALSE)
  message("\nTotal unique DOIs: ", length(all_dois), "\n")

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  results <- data.frame(
    doi    = all_dois,
    status = NA_character_,
    file   = NA_character_,
    url    = NA_character_,
    via    = NA_character_,
    reason = NA_character_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(all_dois)) {
    doi  <- all_dois[i]
    dest <- file.path(output_dir, doi_to_filename(doi))

    cat(sprintf("[%d/%d] %s\n", i, length(all_dois), doi))

    if (!overwrite && file.exists(dest)) {
      cat("  -> already exists, skipping\n")
      results$status[i] <- "skipped (exists)"
      results$file[i]   <- dest
      next
    }

    tried_reasons <- character(0)

    # Stage 1: Unpaywall
    cat("  [1] Unpaywall ... ")
    oa <- get_unpaywall_pdf_url(doi, email)

    if (isTRUE(oa$ok)) {
      cat("found\n  -> downloading ... ")
      # Try with a simple browser request first, no session needed for true OA
      dl <- tryCatch({
        resp <- browser_req(oa$url) |>
          req_options(connecttimeout = 10) |>
          req_timeout(30) |>
          req_perform()
        if (resp_status(resp) != 200)
          return(list(ok = FALSE, reason = paste("HTTP", resp_status(resp))))
        body <- resp_body_raw(resp)
        if (!is_pdf_bytes(body))
          return(list(ok = FALSE, reason = paste0("not a PDF (", tryCatch(resp_content_type(resp), error=\(e)"?"), ")")))
        writeBin(body, dest)
        list(ok = TRUE, size_kb = round(length(body)/1024))
      }, error = function(e) list(ok = FALSE, reason = conditionMessage(e)))

      if (isTRUE(dl$ok)) {
        cat(dl$size_kb, "KB\n")
        results$status[i] <- "downloaded"
        results$file[i]   <- dest
        results$url[i]    <- oa$url
        results$via[i]    <- oa$via
        Sys.sleep(delay_sec)
        next
      }
      cat(dl$reason, " (trying institutional)\n", sep = "")
      tried_reasons <- c(tried_reasons, paste0("unpaywall failed: ", dl$reason))
    } else {
      cat(oa$reason, "\n")
      tried_reasons <- c(tried_reasons, oa$reason)
    }

    # Stage 2: Institutional
    if (!use_institutional) {
      results$status[i] <- "not found"
      results$reason[i] <- paste(tried_reasons, collapse = "; ")
      Sys.sleep(delay_sec)
      next
    }

    cat("  [2] institutional ... ")
    inst <- download_institutional(doi, dest, verbose = verbose)

    if (isTRUE(inst$ok)) {
      cat(inst$size_kb, "KB (", inst$via, ")\n", sep = "")
      results$status[i] <- "downloaded"
      results$file[i]   <- dest
      results$via[i]    <- inst$via
    } else {
      cat(inst$reason, "\n")
      tried_reasons <- c(tried_reasons, inst$reason %||% "")
      results$status[i] <- "not found"
      results$reason[i] <- paste(tried_reasons, collapse = "; ")
    }

    Sys.sleep(delay_sec)
  }

  cat("\n=== Summary ===\n")
  tbl <- sort(table(results$status), decreasing = TRUE)
  for (nm in names(tbl)) cat(sprintf("  %-28s %d\n", nm, tbl[[nm]]))

  missing <- results$doi[!is.na(results$status) & results$status %in% c("not found", "download failed")]
  if (length(missing) > 0) {
    cat("\nStill missing:\n")
    cat(paste0("  ", missing, "\n"), sep = "")
  }

  invisible(results)
}
