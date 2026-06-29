#' @noRd
# Normalises heterogeneous author objects returned by metacheck::read() — which
# can arrive as a data frame, a nested list, or a plain vector — into a
# semicolon-separated "Given Family; Given Family" string.
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
          given  <- a$given  %||% ""
          family <- a$family %||% ""
          full   <- trimws(paste(given, family))
          if (nzchar(full)) return(full)

          txt <- paste(unlist(a), collapse = " ")
          txt <- stringr::str_squish(txt)
          if (nzchar(txt)) return(txt)
        }
        txt <- paste(unlist(a), collapse = " ")
        stringr::str_squish(txt)
      })

      parts <- parts[nzchar(parts)]
      if (length(parts) == 0) return(NA_character_)
      return(paste(parts, collapse = "; "))
    }

    txt <- paste(as.character(unlist(authors_obj)), collapse = "; ")
    txt <- stringr::str_squish(txt)
    if (!nzchar(txt)) NA_character_ else txt
  }, error = function(e) {
    txt <- paste(as.character(unlist(authors_obj)), collapse = "; ")
    txt <- stringr::str_squish(txt)
    if (!nzchar(txt)) NA_character_ else txt
  })

  out
}

#' @noRd
# Waterfall extractor: tries each closure in order and returns the first
# non-NA, non-empty string. Used to handle the variability of Crossref API
# response shapes across different record types.
try_extract <- function(...) {
  fns <- list(...)
  for (f in fns) {
    val <- tryCatch({
      v <- f()
      if (is.null(v) || length(v) == 0) return(NA_character_)
      v <- as.character(v[[1]])
      if (is.na(v) || !nzchar(trimws(v)) || v == "NULL") return(NA_character_)
      v
    }, error = function(e) NA_character_)
    if (!is.na(val)) return(val)
  }
  NA_character_
}

#' @noRd
# Wraps metacheck::doi_clean() + metacheck::crossref_doi() and normalises their
# output into a flat list with fields: ok, title, authors, year.
fetch_doi_meta <- function(doi) {
  doi <- metacheck::doi_clean(doi)
  if (!nzchar(doi)) return(list(ok = FALSE, error = "No DOI provided."))
  tryCatch({
    meta <- metacheck::crossref_doi(doi)
    if (is.null(meta) || length(meta) == 0)
      return(list(ok = FALSE, error = "No data returned from Crossref."))

    if (is.data.frame(meta) && nrow(meta) == 1) {
      meta <- as.list(meta[1, , drop = FALSE])
      meta <- lapply(meta, function(x) if (is.list(x) && length(x) == 1) x[[1]] else x)
    }

    title <- try_extract(
      function() meta$title[[1]],
      function() meta$title[1],
      function() unlist(meta$title)[1]
    )

    authors_raw <- tryCatch(meta$author, error = function(e) NULL)
    authors_str <- tryCatch(collapse_authors(authors_raw), error = function(e) NA_character_)

    extract_year_from_date_field <- function(field) {
      dp <- field
      if (is.null(dp)) return(NA_character_)
      for (i in 1:4) {
        if (is.list(dp) && "date-parts" %in% names(dp)) dp <- dp[["date-parts"]]
        else break
      }
      while (is.list(dp) && !is.numeric(dp) && !is.integer(dp) && length(dp) > 0)
        dp <- dp[[1]]
      if (is.null(dp) || length(dp) == 0) return(NA_character_)
      v <- suppressWarnings(as.integer(dp[1]))
      if (!is.na(v) && v > 1000) as.character(v) else NA_character_
    }

    year <- try_extract(
      function() extract_year_from_date_field(meta$published),
      function() extract_year_from_date_field(meta[["published-print"]]),
      function() extract_year_from_date_field(meta[["issued"]]),
      function() extract_year_from_date_field(meta[["published-online"]]),
      function() as.character(meta$year),
      function() substr(as.character(meta$created[["date-time"]]), 1, 4)
    )

    list(
      ok      = TRUE,
      title   = if (is.na(title)) "" else title,
      authors = if (is.na(authors_str)) "" else authors_str,
      year    = if (is.na(year)) "" else year
    )
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

#' @noRd
check_url <- function(url, timeout_sec = 20) {
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_user_agent("shiny-metacheck-local") |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_perform()

    list(
      ok     = TRUE,
      status = httr2::resp_status(resp),
      body   = httr2::resp_body_string(resp)
    )
  }, error = function(e) {
    list(ok = FALSE, status = NA_integer_, body = conditionMessage(e))
  })
}

#' @noRd
check_grobid_alive <- function(grobid_url) {
  grobid_url <- sub("/+$", "", grobid_url)
  check_url(paste0(grobid_url, "/api/isalive"))
}

#' @noRd
# Wraps metacheck::pdf2grobid() with a richer stop() message that includes
# the GROBID URL and the filename to help diagnose connection issues.
safe_pdf2grobid <- function(pdf_path, grobid_url) {
  tryCatch({
    metacheck::pdf2grobid(
      filename   = pdf_path,
      save_path  = tempdir(),
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

#' @noRd
# Tries three extraction strategies in order for cross-platform compatibility:
# archive::archive_extract (preferred), utils::unzip, then system unzip.
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
  }, warning = function(w) FALSE, error = function(e) FALSE)
  if (ok) return(list(ok = TRUE, method = "utils::unzip"))

  ok <- tryCatch({
    system2("unzip", args = c("-o", zipfile, "-d", exdir), stdout = TRUE, stderr = TRUE)
    TRUE
  }, error = function(e) FALSE)
  if (ok) return(list(ok = TRUE, method = "system unzip"))

  list(ok = FALSE, method = NA_character_)
}

#' @noRd
# Shiny fileInput helper only. Converts a fileInput data frame (with $name and
# $datapath columns) into a normalised tibble of PDF paths, extracting ZIPs as needed.
collect_uploaded_pdfs <- function(upload_df) {
  if (is.null(upload_df) || nrow(upload_df) == 0) {
    return(list(
      pdfs = tibble::tibble(pdf_path = character(), pdf_name = character(), source_upload = character()),
      messages = character()
    ))
  }

  out      <- list()
  messages <- character()

  for (i in seq_len(nrow(upload_df))) {
    original_name <- upload_df$name[i]
    datapath      <- upload_df$datapath[i]
    ext           <- tolower(tools::file_ext(original_name))

    if (ext == "pdf") {
      out[[length(out) + 1]] <- tibble::tibble(
        pdf_path      = datapath,
        pdf_name      = original_name,
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
      messages <- c(messages, paste0("Extracted ZIP with ", extracted$method, ": ", original_name))

      pdfs <- list.files(unzip_dir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(pdfs) == 0) {
        messages <- c(messages, paste0("No PDFs found inside ZIP: ", original_name))
      } else {
        out[[length(out) + 1]] <- tibble::tibble(
          pdf_path      = pdfs,
          pdf_name      = basename(pdfs),
          source_upload = original_name
        )
        messages <- c(messages, paste0("PDFs found in ZIP ", original_name, ": ", length(pdfs)))
      }
    } else {
      messages <- c(messages, paste0("Skipped unsupported file: ", original_name))
    }
  }

  pdf_tbl <- if (length(out) == 0) {
    tibble::tibble(pdf_path = character(), pdf_name = character(), source_upload = character())
  } else {
    dplyr::bind_rows(out) |> dplyr::distinct(pdf_path, .keep_all = TRUE)
  }

  list(pdfs = pdf_tbl, messages = messages)
}

#' Extract citation contexts from an already-parsed paper object
#'
#' Constructs a tibble of all bibliography citations from a \code{paper} object
#' as returned by \code{metacheck::read()}. Each row is one citation occurrence.
#' The \code{expanded_text} column contains a window of surrounding sentences
#' located using \code{metacheck::search_text()}.
#'
#' @param paper A paper object as returned by \code{metacheck::read()}.
#' @param pdf_name Character. Display name used as the \code{file} column.
#' @param window Integer. Number of sentences before and after the citation
#'   sentence to include in \code{expanded_text}. Default \code{10L}.
#'
#' @return A tibble with one row per citation occurrence. See
#'   \code{\link{extract_citation_contexts}} for column descriptions.
#'
#' @seealso \code{\link{extract_citation_contexts}}, \code{\link{annotate_with_db_matches}}
#' @export
contexts_from_paper <- function(paper, pdf_name, window = 10L) {
  xrefs_all <- paper$xrefs
  bib_all   <- paper$bib

  empty_ctx <- function(note) tibble::tibble(
    file = pdf_name,
    citation = NA_character_, sentence = NA_character_, expanded_text = NA_character_,
    ref_title = NA_character_, ref_authors = NA_character_,
    ref_author_last_names = NA_character_, ref_author_key = NA_character_,
    ref_lead_author = NA_character_, ref_title_key = NA_character_,
    ref_year = NA_character_, ref_doi = NA_character_,
    citation_lead_author_guess = NA_character_, citation_year_guess = NA_character_,
    section = NA_character_, div = NA_integer_, p = NA_integer_, s = NA_integer_,
    note = note
  )

  if (is.null(xrefs_all) || nrow(xrefs_all) == 0)
    return(empty_ctx("No xrefs found in parsed paper."))

  xrefs <- xrefs_all |> dplyr::filter(type == "bibr")

  if (nrow(xrefs) == 0)
    return(empty_ctx(paste0(
      "No bibliography citations found (type == 'bibr'). xref types seen: ",
      paste(unique(head(xrefs_all$type, 10)), collapse = ", ")
    )))

  bib_tbl <- if (!is.null(bib_all) && nrow(bib_all) > 0) {
    bib_all |>
      dplyr::mutate(
        ref_authors = purrr::map_chr(authors, collapse_authors),
        ref_title   = as.character(title %||% NA_character_),
        ref_year    = as.character(year  %||% NA),
        ref_doi     = as.character(doi   %||% NA)
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        ref_author_last_names = paste(extract_last_names_from_ref_authors(ref_authors), collapse = "; "),
        ref_author_key        = make_author_key(extract_last_names_from_ref_authors(ref_authors)),
        ref_lead_author = {
          tmp <- extract_last_names_from_ref_authors(ref_authors)
          if (length(tmp) > 0) tmp[1] else NA_character_
        },
        ref_title_key = make_title_key(ref_title)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::any_of(c(
        "xref_id", "ref_title", "ref_authors", "ref_author_last_names",
        "ref_author_key", "ref_lead_author", "ref_title_key", "ref_year", "ref_doi"
      )))
  } else {
    tibble::tibble(
      xref_id = character(), ref_title = character(), ref_authors = character(),
      ref_author_last_names = character(), ref_author_key = character(),
      ref_lead_author = character(), ref_title_key = character(),
      ref_year = character(), ref_doi = character()
    )
  }

  citation_vec <- as.character(xrefs$contents %||% NA_character_)

  out <- xrefs |>
    dplyr::left_join(bib_tbl, by = "xref_id") |>
    dplyr::transmute(
      file    = pdf_name,
      citation = citation_vec,
      sentence = as.character(text %||% NA_character_),
      expanded_text = as.character(text %||% NA_character_),
      ref_title, ref_authors, ref_author_last_names, ref_author_key,
      ref_lead_author, ref_title_key, ref_year, ref_doi,
      citation_lead_author_guess = extract_lead_author_from_citation_vec(citation_vec),
      citation_year_guess        = extract_year_guess_vec(citation_vec),
      section = as.character(section %||% NA_character_),
      div = suppressWarnings(as.integer(div)),
      p   = suppressWarnings(as.integer(p)),
      s   = suppressWarnings(as.integer(s)),
      note = NA_character_
    )

  # metacheck::search_text() returns all document sentences in order; row index
  # is used for the ±window expansion. Returns NULL when the paper has no
  # parseable sentence boundaries, in which case expansion is skipped gracefully.
  sent_list <- tryCatch({
    fs <- metacheck::search_text(paper)
    if (is.null(fs) || nrow(fs) == 0 || !"text" %in% names(fs)) {
      NULL
    } else {
      sents <- as.character(fs$text)
      sents[!is.na(sents) & nzchar(sents)]
    }
  }, error = function(e) NULL)

  # A 60-char normalised prefix is used as a fingerprint to locate each citation
  # sentence in the search_text() output, because GROBID's <s> element text and
  # search_text() text can differ slightly (whitespace/encoding). Falls back to
  # 30 chars before giving up and returning the raw sentence unchanged.
  find_and_expand <- function(sentence_text, sents, w) {
    st <- trimws(dplyr::coalesce(as.character(sentence_text), ""))
    if (!nzchar(st)) return(list(expanded = NA_character_, target = NA_character_))
    if (is.null(sents) || length(sents) == 0) return(list(expanded = st, target = st))

    norm <- function(x) tolower(trimws(gsub("\\s+", " ", normalize_text_ascii(x))))
    fp         <- substr(norm(st), 1, 60)
    sents_norm <- vapply(sents, norm, character(1), USE.NAMES = FALSE)

    idx <- which(grepl(fp, sents_norm, fixed = TRUE))
    if (length(idx) == 0) {
      fp2 <- substr(fp, 1, 30)
      idx <- which(grepl(fp2, sents_norm, fixed = TRUE))
    }

    if (length(idx) == 0) return(list(expanded = st, target = st))

    i            <- idx[1]
    window_sents <- sents[max(1L, i - w):min(length(sents), i + w)]
    list(expanded = paste(window_sents, collapse = " "), target = trimws(sents[i]))
  }

  expanded_out <- tryCatch({
    results <- lapply(out$sentence, find_and_expand, sents = sent_list, w = window)
    out |> dplyr::mutate(
      expanded_text   = vapply(results, `[[`, character(1), "expanded"),
      target_sentence = vapply(results, `[[`, character(1), "target")
    )
  }, error = function(e) {
    out |> dplyr::mutate(
      note = dplyr::coalesce(note, paste("Expansion failed:", conditionMessage(e))),
      target_sentence = NA_character_
    )
  })

  # Assemble a human-readable citation label. Author and year are appended only
  # if they are not already present in the raw in-text marker to avoid duplication.
  expanded_out |>
    dplyr::mutate(
      citation = {
        cit_raw  <- dplyr::coalesce(as.character(citation), "")
        auth     <- dplyr::coalesce(as.character(ref_lead_author), "")
        yr       <- dplyr::coalesce(as.character(ref_year), "")
        cit_lbl  <- ifelse(nzchar(cit_raw), paste0("[", cit_raw, "]"), "")
        auth_lbl <- ifelse(nzchar(auth) & !grepl(auth, cit_raw, ignore.case = TRUE, fixed = TRUE),
                           paste0(" — ", auth), "")
        yr_lbl   <- ifelse(nzchar(yr) & !grepl(yr, cit_raw, fixed = TRUE), paste0(", ", yr), "")
        trimws(paste0(cit_lbl, auth_lbl, yr_lbl))
      }
    )
}

#' Extract citation contexts from a PDF
#'
#' Sends a PDF to a running GROBID instance, parses the resulting TEI XML via
#' \code{metacheck::read()}, and delegates to \code{\link{contexts_from_paper}}.
#'
#' @param pdf_path Character. Absolute path to the local PDF file.
#' @param pdf_name Character. Display name used as the \code{file} column.
#' @param grobid_url Character. Base URL of the GROBID REST service.
#' @param window Integer. Context window size. Default \code{10L}.
#'
#' @return See \code{\link{contexts_from_paper}}.
#' @seealso \code{\link{contexts_from_paper}}, \code{\link{annotate_with_db_matches}}
#' @export
extract_citation_contexts <- function(pdf_path, pdf_name, grobid_url, window = 10L) {
  xml_path <- safe_pdf2grobid(pdf_path, grobid_url = grobid_url)
  paper    <- metacheck::read(xml_path)
  contexts_from_paper(paper, pdf_name, window)
}

#' Annotate citation contexts with known-miscitation database matches
#'
#' For each row in \code{citation_df} (output of
#' \code{\link{extract_citation_contexts}}), fuzzy-matches against
#' \code{known_df} (a summarised database of previously reported miscitations)
#' and appends match metadata columns. Matching uses a composite score across
#' author key equality, fuzzy author overlap, lead-author edit distance, year
#' proximity, and title token overlap.
#'
#' @param citation_df A tibble as returned by
#'   \code{\link{extract_citation_contexts}}.
#' @param known_df A tibble of known miscitations, e.g. as returned by
#'   \code{summarise_known_miscitations()}. Required columns:
#'   \code{target_author_key}, \code{target_lead_author},
#'   \code{target_author_last_names}, \code{target_publication_year},
#'   \code{target_title_key}, \code{report_count}, \code{report_ids},
#'   \code{mistake_codes}, \code{mistake_titles}, \code{why_incorrect},
#'   \code{quoted_or_paraphrased_text}. Pass \code{NULL} or a zero-row tibble
#'   to get the annotation columns with all-default values (no matches).
#'
#' @return The input tibble with additional columns:
#'   \code{db_known_miscited_paper} (\code{"Yes"}/\code{"No"}),
#'   \code{db_match_score} (integer composite score),
#'   \code{db_match_reason} (pipe-separated list of match signals),
#'   \code{db_report_count}, \code{db_report_ids},
#'   \code{db_target_author_last_names}, \code{db_mistake_codes},
#'   \code{db_mistake_titles}, \code{db_why_incorrect},
#'   \code{db_quoted_text}, \code{db_correct_citation}. Rows are sorted with
#'   \code{"Yes"} matches first, then by descending score and report count.
#'
#' @seealso \code{\link{extract_citation_contexts}}, \code{\link{analyze_citations}}
#' @export
normalize_doi <- function(x) tolower(trimws(sub("^https?://doi\\.org/", "", x %||% "")))

annotate_with_db_matches <- function(citation_df, known_df) {
  if (is.null(citation_df) || nrow(citation_df) == 0) return(citation_df)

  if (is.null(known_df) || nrow(known_df) == 0) {
    return(citation_df |>
             dplyr::mutate(
               db_known_miscited_paper     = "No",
               db_match_score              = 0L,
               db_match_reason             = NA_character_,
               db_report_count             = 0L,
               db_report_ids               = NA_character_,
               db_target_author_last_names = NA_character_,
               db_mistake_codes            = NA_character_,
               db_mistake_titles           = NA_character_,
               db_correct_citation         = NA_character_
             ))
  }

  out_rows <- vector("list", nrow(citation_df))

  for (i in seq_len(nrow(citation_df))) {
    row       <- citation_df[i, , drop = FALSE]
    ref_names <- unlist(strsplit(row$ref_author_last_names %||% "", ";\\s*"))
    ref_names <- ref_names[nzchar(ref_names)]

    # Score each candidate for ranking only — not used as a flag threshold.
    candidates <- known_df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        target_names_vec  = list(unlist(strsplit(target_author_last_names %||% "", ";\\s*"))),
        exact_key_match   = !is.na(row$ref_author_key) && !is.na(target_author_key) &&
                              row$ref_author_key == target_author_key,
        fuzzy_overlap     = fuzzy_author_overlap(ref_names, target_names_vec[[1]], max_compare = 3),
        lead_author_match = (!is.na(row$ref_lead_author) && !is.na(target_lead_author) &&
                               author_name_distance_ok(row$ref_lead_author, target_lead_author)) ||
                            (!is.na(row$citation_lead_author_guess) && !is.na(target_lead_author) &&
                               author_name_distance_ok(row$citation_lead_author_guess, target_lead_author)),
        year_score        = max(year_close_score(row$ref_year, target_publication_year),
                                year_close_score(row$citation_year_guess, target_publication_year)),
        title_overlap     = count_title_overlap(row$ref_title_key, target_title_key),
        score             = as.integer(exact_key_match) * 3L +
                            fuzzy_overlap +
                            as.integer(lead_author_match) +
                            year_score +
                            pmin(title_overlap, 2L)
      ) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(score), dplyr::desc(report_count))

    best <- candidates[1, , drop = FALSE]

    # Flag decision:
    #   author_match  : exact key match OR ≥2 fuzzy name overlaps
    #   lead_confirmed: lead author matches AND both year AND title agree independently
    #                   (handles cases where GROBID only linked the in-text lead author,
    #                    not the full reference list entry — requires all 3 signals to
    #                    avoid false positives from common single-author surnames)
    #   year_positive : year is within 1 of the target
    #   title_positive: ≥1 title token shared with the target
    # A veto fires only when a signal is resolvable on both sides and contradicts.
    author_match   <- isTRUE(best$exact_key_match) || isTRUE(best$fuzzy_overlap >= 2)
    year_positive  <- isTRUE(best$year_score > 0)
    title_positive <- isTRUE(best$title_overlap > 0)
    lead_confirmed <- isTRUE(best$lead_author_match) && year_positive && title_positive

    year_veto <- {
      ref_year_known    <- (!is.na(row$ref_year)              && nzchar(row$ref_year)) ||
                           (!is.na(row$citation_year_guess)   && nzchar(row$citation_year_guess))
      target_year_known <- !is.na(best$target_publication_year) && nzchar(best$target_publication_year)
      ref_year_known && target_year_known && !year_positive
    }
    title_veto <- {
      ref_title_known    <- !is.na(row$ref_title_key)       && nzchar(row$ref_title_key)
      target_title_known <- !is.na(best$target_title_key)   && nzchar(best$target_title_key)
      ref_title_known && target_title_known && !title_positive
    }

    flagged <- (author_match || lead_confirmed) &&
               (year_positive || title_positive) &&
               !year_veto && !title_veto

    reason_bits <- character(0)
    if (isTRUE(best$exact_key_match))         reason_bits <- c(reason_bits, "author key")
    else if (isTRUE(best$fuzzy_overlap >= 2)) reason_bits <- c(reason_bits, paste0("fuzzy overlap=", best$fuzzy_overlap))
    else if (lead_confirmed)                  reason_bits <- c(reason_bits, "lead author+year+title")
    if (isTRUE(best$lead_author_match))       reason_bits <- c(reason_bits, "lead author")
    if (year_positive)                        reason_bits <- c(reason_bits, "year")
    if (title_positive)                       reason_bits <- c(reason_bits, "title")

    match_score <- if (nrow(best) == 0 || is.na(best$score)) 0L else as.integer(best$score)

    out_rows[[i]] <- row |>
      dplyr::mutate(
        db_known_miscited_paper     = if (flagged) "Yes" else "No",
        db_match_score              = match_score,
        db_match_reason             = if (length(reason_bits)) paste(reason_bits, collapse = " + ") else NA_character_,
        db_report_count             = if (flagged) best$report_count else 0L,
        db_report_ids               = if (flagged) best$report_ids %||% NA_character_ else NA_character_,
        db_target_author_last_names = if (flagged) best$target_author_last_names else NA_character_,
        db_mistake_codes            = if (flagged) best$mistake_codes else NA_character_,
        db_mistake_titles           = if (flagged) best$mistake_titles else NA_character_,
        db_why_incorrect            = if (flagged) best$why_incorrect %||% NA_character_ else NA_character_,
        db_quoted_text              = if (flagged) best$quoted_or_paraphrased_text %||% NA_character_ else NA_character_,
        db_correct_citation         = if (flagged) best$correct_citation %||% NA_character_ else NA_character_
      )
  }

  dplyr::bind_rows(out_rows) |>
    dplyr::arrange(dplyr::desc(db_known_miscited_paper == "Yes"), dplyr::desc(db_match_score), dplyr::desc(db_report_count), file, p, s)
}
