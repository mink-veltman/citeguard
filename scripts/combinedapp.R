options(shiny.maxRequestSize = 1000 * 1024^2)

library(shiny)
library(shinyjs)
library(dplyr)
library(purrr)
library(stringr)
library(DT)
library(tibble)
library(httr2)
library(metacheck)
library(digest)
library(base64enc)

`%||%` <- function(x, y) if (is.null(x)) y else x

DB_DIR    <- "data"
DB_PATH   <- file.path(DB_DIR, "miscitation_reports.csv")
LOCK_PATH <- paste0(DB_PATH, ".lock")

GH_OWNER   <- "mink-veltman"
GH_REPO    <- "citeguard-data"
GH_FILE    <- "miscitation_reports.csv"
GH_BRANCH  <- "master"
GH_API_URL <- paste0("https://api.github.com/repos/", GH_OWNER, "/", GH_REPO, "/contents/", GH_FILE)
GH_RAW_URL <- paste0("https://raw.githubusercontent.com/", GH_OWNER, "/", GH_REPO, "/", GH_BRANCH, "/", GH_FILE)

dir.create(DB_DIR, showWarnings = FALSE, recursive = TRUE)

mistake_table <- data.frame(
  mistake_code = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10"),
  mistake_title = c(
    "Direct contradiction of finding",
    "Non-existent finding",
    "Non-significant finding",
    "Overgeneralization of population",
    "Misrepresentation of experimental conditions",
    "Causal relation distorted or confused",
    "Independent variable distortion",
    "Dependent variable distortion",
    "Measure-to-construct inflation",
    "Non-contextualized citation"
  ),
  mistake_description = c(
    "The findings of the cited paper are directly contradicted in the citation.",
    "Neither the cited finding nor the measures exist in the cited paper.",
    "A non-significant finding is cited.",
    "The cited finding is overgeneralized to a larger population without justification.",
    "The experimental conditions are misrepresented.",
    "The causal claim is reversed or overstated.",
    "The cited independent variable is distorted.",
    "The cited dependent variable is distorted.",
    "A measure is inflated to a broader construct without justification.",
    "Citation relevance is not apparent."
  ),
  stringsAsFactors = FALSE
)

mistake_choices <- stats::setNames(
  mistake_table$mistake_code,
  paste0(mistake_table$mistake_code, " — ", mistake_table$mistake_title)
)

db_cols <- c(
  "report_id",
  "submitted_at",
  "reporter_role",
  "reporter_orcid",
  "own_work_doi",
  "citing_work_doi",
  "target_author_last_names",
  "target_lead_author",
  "target_author_key",
  "target_publication_year",
  "target_title_clue",
  "target_title_key",
  "mistake_codes",
  "mistake_titles",
  "quoted_or_paraphrased_text",
  "why_incorrect"
)

empty_db_frame <- function() {
  as.data.frame(
    setNames(replicate(length(db_cols), character(0), simplify = FALSE), db_cols),
    stringsAsFactors = FALSE
  )
}

required_msg <- function(x) {
  if (is.null(x)) return(TRUE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(TRUE)
  !any(nzchar(trimws(x)))
}

make_report_id <- function(orcid, own_doi, citing_doi) {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  h <- digest(paste(orcid, own_doi, citing_doi, runif(1), sep = "|"), algo = "sha1")
  paste0(stamp, "_", substr(h, 1, 10))
}

get_mistake_titles <- function(codes) {
  if (is.null(codes) || length(codes) == 0) return(character(0))
  mistake_table$mistake_title[match(codes, mistake_table$mistake_code)]
}

normalize_text_ascii <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- enc2utf8(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  x
}

normalize_last_name <- function(x) {
  if (is.null(x)) return(character(0))
  x <- normalize_text_ascii(x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z'-]", "", x, perl = TRUE)
  x[nchar(x) == 0] <- NA_character_
  x
}

make_author_key <- function(last_names, max_n = 5) {
  if (is.null(last_names) || length(last_names) == 0) return(NA_character_)
  last_names <- normalize_last_name(last_names)
  last_names <- last_names[!is.na(last_names)]
  last_names <- unique(last_names)
  if (length(last_names) == 0) return(NA_character_)
  paste(last_names[seq_len(min(length(last_names), max_n))], collapse = "|")
}

normalize_title_tokens <- function(x, max_tokens = 6) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- normalize_text_ascii(x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  if (!nzchar(x)) return(character(0))
  
  toks <- unlist(strsplit(x, "\\s+"))
  toks <- toks[nchar(toks) >= 4]
  toks <- toks[!toks %in% c(
    "with", "from", "that", "this", "these", "those", "their", "there",
    "about", "among", "into", "using", "effects", "effect", "study",
    "analysis", "research", "evidence", "paper"
  )]
  toks <- unique(toks)
  
  if (length(toks) == 0) return(character(0))
  toks[seq_len(min(length(toks), max_tokens))]
}

make_title_key <- function(x, max_tokens = 6) {
  toks <- normalize_title_tokens(x, max_tokens = max_tokens)
  if (length(toks) == 0) return(NA_character_)
  paste(toks, collapse = "|")
}

extract_year_guess_one <- function(x) {
  if (length(x) != 1 || is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  m <- stringr::str_extract(as.character(x), "\\b(19|20)\\d{2}[a-z]?\\b")
  ifelse(is.na(m), NA_character_, m)
}

extract_year_guess_vec <- function(x) {
  purrr::map_chr(as.character(x), extract_year_guess_one)
}

extract_lead_author_from_citation_one <- function(citation_text) {
  if (length(citation_text) != 1 || is.null(citation_text) || is.na(citation_text) || !nzchar(trimws(citation_text))) {
    return(NA_character_)
  }
  
  txt <- normalize_text_ascii(citation_text)
  txt <- gsub("\\bet al\\.?\\b", "", txt, ignore.case = TRUE)
  txt <- trimws(txt)
  
  tokens <- stringr::str_extract_all(txt, "\\b[A-Za-z][A-Za-z'\\-]+\\b")[[1]]
  if (length(tokens) == 0) return(NA_character_)
  
  bad <- c("and", "the", "for", "see", "also")
  tokens <- tokens[!(tolower(tokens) %in% bad)]
  if (length(tokens) == 0) return(NA_character_)
  
  out <- normalize_last_name(tokens[1])
  if (length(out) == 0) NA_character_ else out
}

extract_lead_author_from_citation_vec <- function(x) {
  purrr::map_chr(as.character(x), extract_lead_author_from_citation_one)
}

extract_last_names_from_ref_authors <- function(ref_authors, max_n = 5) {
  if (is.null(ref_authors) || is.na(ref_authors) || !nzchar(trimws(ref_authors))) {
    return(character(0))
  }
  
  authors <- unlist(strsplit(ref_authors, ";"))
  authors <- trimws(authors)
  authors <- authors[nzchar(authors)]
  
  out <- character(0)
  for (a in authors) {
    a2 <- normalize_text_ascii(a)
    a2 <- gsub(",", " ", a2)
    a2 <- str_squish(a2)
    toks <- unlist(strsplit(a2, "\\s+"))
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) next
    last <- toks[length(toks)]
    last <- normalize_last_name(last)
    if (length(last) > 0 && !is.na(last)) out <- c(out, last)
  }
  
  out <- unique(out)
  if (length(out) == 0) return(character(0))
  out[seq_len(min(length(out), max_n))]
}

count_title_overlap <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(0L)
  ta <- unique(unlist(strsplit(a, "\\|")))
  tb <- unique(unlist(strsplit(b, "\\|")))
  length(intersect(ta, tb))
}

author_name_distance_ok <- function(a, b) {
  if (is.na(a) || is.na(b) || !nzchar(a) || !nzchar(b)) return(FALSE)
  
  a <- normalize_last_name(a)
  b <- normalize_last_name(b)
  
  if (length(a) == 0 || length(b) == 0 || is.na(a) || is.na(b)) return(FALSE)
  if (a == b) return(TRUE)
  
  d <- utils::adist(a, b)[1]
  if (max(nchar(a), nchar(b)) >= 8) d <= 2 else d <= 1
}

fuzzy_author_overlap <- function(ref_names, target_names, max_compare = 3) {
  ref_names <- normalize_last_name(ref_names)
  target_names <- normalize_last_name(target_names)
  
  ref_names <- unique(ref_names[!is.na(ref_names)])
  target_names <- unique(target_names[!is.na(target_names)])
  
  if (length(ref_names) == 0 || length(target_names) == 0) return(0L)
  
  ref_names <- ref_names[seq_len(min(length(ref_names), max_compare))]
  target_names <- target_names[seq_len(min(length(target_names), max_compare))]
  
  matched <- 0L
  used_target <- rep(FALSE, length(target_names))
  
  for (r in ref_names) {
    for (j in seq_along(target_names)) {
      if (!used_target[j] && author_name_distance_ok(r, target_names[j])) {
        matched <- matched + 1L
        used_target[j] <- TRUE
        break
      }
    }
  }
  
  matched
}

year_close_score <- function(ref_year, target_year) {
  if (is.na(ref_year) || is.na(target_year) || !nzchar(ref_year) || !nzchar(target_year)) return(0L)
  
  ref_num <- suppressWarnings(as.integer(stringr::str_extract(ref_year, "(19|20)\\d{2}")))
  tar_num <- suppressWarnings(as.integer(stringr::str_extract(target_year, "(19|20)\\d{2}")))
  
  if (is.na(ref_num) || is.na(tar_num)) return(0L)
  if (ref_num == tar_num) return(2L)
  if (abs(ref_num - tar_num) == 1) return(1L)
  0L
}

init_db_if_missing <- function() {
  if (!file.exists(DB_PATH)) {
    write.csv(empty_db_frame(), DB_PATH, row.names = FALSE, na = "")
  }
}

read_reports_db <- function() {
  init_db_if_missing()
  
  out <- tryCatch(
    read.csv(DB_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE),
    error = function(e) empty_db_frame()
  )
  
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  
  for (cc in db_cols) {
    if (!cc %in% names(out)) out[[cc]] <- NA_character_
  }
  
  out <- out[, db_cols, drop = FALSE]
  for (cc in db_cols) out[[cc]] <- as.character(out[[cc]])
  out
}

acquire_lock <- function(timeout_sec = 10) {
  start <- Sys.time()
  while (file.exists(LOCK_PATH)) {
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_sec) {
      return(FALSE)
    }
    Sys.sleep(0.1)
  }
  file.create(LOCK_PATH)
}

release_lock <- function() {
  if (file.exists(LOCK_PATH)) unlink(LOCK_PATH)
}

merge_semicolon_field <- function(existing_val, new_val) {
  existing_parts <- trimws(unlist(strsplit(as.character(existing_val), ";\\s*")))
  new_parts      <- trimws(unlist(strsplit(as.character(new_val),      ";\\s*")))
  all_parts <- unique(c(
    existing_parts[nzchar(existing_parts) & existing_parts != "NA"],
    new_parts[nzchar(new_parts) & new_parts != "NA"]
  ))
  paste(all_parts, collapse = "; ")
}

merge_freetext_field <- function(existing_val, new_val) {
  e <- trimws(as.character(existing_val))
  n <- trimws(as.character(new_val))
  e[e == "NA"] <- ""
  n[n == "NA"] <- ""
  if (!nzchar(n) || n == e) return(e)
  if (!nzchar(e)) return(n)
  paste(e, n, sep = " || ")
}

append_row_safely <- function(row_df) {
  init_db_if_missing()

  if (!acquire_lock()) stop("Database is busy. Please try again.")
  on.exit(release_lock(), add = TRUE)

  existing <- read_reports_db()
  row_df <- as.data.frame(row_df, stringsAsFactors = FALSE)

  for (cc in db_cols) {
    if (!cc %in% names(row_df)) row_df[[cc]] <- NA_character_
  }

  row_df <- row_df[, db_cols, drop = FALSE]
  for (cc in db_cols) row_df[[cc]] <- as.character(row_df[[cc]])

  new_own    <- trimws(row_df$own_work_doi[1])
  new_citing <- trimws(row_df$citing_work_doi[1])

  match_idx <- which(
    trimws(existing$own_work_doi)    == new_own &
    trimws(existing$citing_work_doi) == new_citing &
    nzchar(new_own) & nzchar(new_citing)
  )

  if (length(match_idx) > 0) {
    idx <- match_idx[1]
    existing$mistake_codes[idx]              <- merge_semicolon_field(existing$mistake_codes[idx],              row_df$mistake_codes[1])
    existing$mistake_titles[idx]             <- merge_semicolon_field(existing$mistake_titles[idx],             row_df$mistake_titles[1])
    existing$why_incorrect[idx]              <- merge_freetext_field(existing$why_incorrect[idx],               row_df$why_incorrect[1])
    existing$quoted_or_paraphrased_text[idx] <- merge_freetext_field(existing$quoted_or_paraphrased_text[idx],  row_df$quoted_or_paraphrased_text[1])
    existing$submitted_at[idx]               <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    out <- existing
  } else {
    out <- dplyr::bind_rows(existing, row_df)
  }

  out <- out[, db_cols, drop = FALSE]
  write.csv(out, DB_PATH, row.names = FALSE, na = "")
}

sync_db_from_github <- function() {
  tryCatch({
    df <- read.csv(GH_RAW_URL, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE)
    write.csv(df, DB_PATH, row.names = FALSE, na = "")
    message("DB synced from GitHub (", nrow(df), " rows)")
  }, error = function(e) {
    message("GitHub DB read failed (using local copy): ", conditionMessage(e))
  })
}

push_db_to_github <- function(df, commit_msg) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (!nzchar(pat)) return("GitHub sync skipped: GITHUB_PAT not set")

  tryCatch({
    sha_resp <- request(GH_API_URL) |>
      req_headers(Authorization = paste("token", pat),
                  Accept        = "application/vnd.github.v3+json") |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    sha <- if (resp_status(sha_resp) == 200) resp_body_json(sha_resp)$sha else NULL

    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    write.csv(df, tmp, row.names = FALSE, na = "")
    encoded <- base64encode(readBin(tmp, "raw", file.info(tmp)$size))

    body <- list(message = commit_msg, content = encoded, branch = GH_BRANCH)
    if (!is.null(sha)) body$sha <- sha

    put_resp <- request(GH_API_URL) |>
      req_headers(Authorization = paste("token", pat),
                  Accept        = "application/vnd.github.v3+json") |>
      req_body_json(body) |>
      req_method("PUT") |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()

    if (resp_status(put_resp) %in% c(200L, 201L)) {
      "GitHub synced"
    } else {
      paste0("GitHub sync failed (HTTP ", resp_status(put_resp), "): ",
             resp_body_string(put_resp))
    }
  }, error = function(e) {
    paste0("GitHub sync error: ", conditionMessage(e))
  })
}

erase_database_safely <- function() {
  init_db_if_missing()
  
  if (!acquire_lock()) stop("Database is busy. Please try again.")
  on.exit(release_lock(), add = TRUE)
  
  backup_path <- file.path(
    DB_DIR,
    paste0("miscitation_reports_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  
  if (file.exists(DB_PATH)) {
    file.copy(DB_PATH, backup_path, overwrite = TRUE)
  }
  
  write.csv(empty_db_frame(), DB_PATH, row.names = FALSE, na = "")

  backup_path
}

summarise_known_miscitations <- function(db) {
  if (is.null(db) || nrow(db) == 0) {
    return(tibble(
      target_author_key = character(),
      target_lead_author = character(),
      target_publication_year = character(),
      target_title_key = character(),
      target_author_last_names = character(),
      report_count = integer(),
      mistake_codes = character(),
      mistake_titles = character()
    ))
  }
  
  db2 <- db %>%
    mutate(
      target_author_key = ifelse(is.na(target_author_key) | !nzchar(trimws(target_author_key)), NA_character_, as.character(target_author_key)),
      target_lead_author = normalize_last_name(target_lead_author),
      target_publication_year = as.character(target_publication_year),
      target_title_key = ifelse(is.na(target_title_key) | !nzchar(trimws(target_title_key)), NA_character_, as.character(target_title_key))
    )
  
  db2 %>%
    filter(
      !is.na(target_author_key) |
        !is.na(target_lead_author) |
        !is.na(target_title_key)
    ) %>%
    group_by(
      target_author_key,
      target_lead_author,
      target_publication_year,
      target_title_key
    ) %>%
    summarise(
      target_author_last_names = first(target_author_last_names),
      report_count = n(),
      report_ids = paste(unique(na.omit(report_id)), collapse = "; "),
      mistake_codes = paste(unique(na.omit(unlist(strsplit(paste(mistake_codes, collapse = "; "), ";\\s*")))), collapse = "; "),
      mistake_titles = paste(unique(na.omit(unlist(strsplit(paste(mistake_titles, collapse = "; "), ";\\s*")))), collapse = "; "),
      why_incorrect = {
        parts <- trimws(unlist(strsplit(paste(why_incorrect, collapse = " || "), "\\s*\\|\\|\\s*")))
        parts <- unique(parts[nzchar(parts) & parts != "NA"])
        paste(parts, collapse = " || ")
      },
      .groups = "drop"
    ) %>%
    arrange(desc(report_count), target_author_last_names)
}

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

normalize_orcid <- function(orcid) {
  orcid <- trimws(orcid)
  orcid <- sub("^https?://orcid\\.org/", "", orcid, ignore.case = TRUE, perl = TRUE)
  trimws(orcid)
}

fetch_orcid_meta <- function(orcid) {
  orcid <- normalize_orcid(orcid)
  if (!nzchar(orcid)) return(list(ok = FALSE, error = "No ORCID provided."))
  if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", orcid))
    return(list(ok = FALSE, error = "Invalid format. Expected: 0000-0000-0000-0000"))
  tryCatch({
    resp <- request(paste0("https://pub.orcid.org/v3.0/", orcid, "/person")) |>
      req_headers(Accept = "application/json") |>
      req_user_agent("miscitation-reporter") |>
      req_timeout(15) |>
      req_perform()
    if (resp_status(resp) != 200)
      return(list(ok = FALSE, error = paste0("Not found (HTTP ", resp_status(resp), ")")))
    data <- resp_body_json(resp)
    given  <- tryCatch(data$name$`given-names`$value,  error = function(e) NULL)
    family <- tryCatch(data$name$`family-name`$value,  error = function(e) NULL)
    name <- trimws(paste(c(given, family), collapse = " "))
    list(ok = TRUE, mode = "verify",
         name = if (nzchar(name)) name else "(name not public)", orcid = orcid)
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

search_orcid_by_name <- function(query, max_results = 6) {
  query <- trimws(query)
  if (!nzchar(query)) return(list(ok = FALSE, error = "No name entered."))
  tryCatch({
    resp <- request("https://pub.orcid.org/v3.0/expanded-search") |>
      req_url_query(q = query, rows = max_results) |>
      req_headers(Accept = "application/json") |>
      req_user_agent("miscitation-reporter") |>
      req_timeout(15) |>
      req_perform()
    if (resp_status(resp) != 200)
      return(list(ok = FALSE, error = paste0("Search failed (HTTP ", resp_status(resp), ")")))
    data  <- resp_body_json(resp)
    items <- data$`expanded-result`
    if (is.null(items) || length(items) == 0)
      return(list(ok = TRUE, mode = "search", results = list()))
    records <- lapply(items, function(r) {
      orcid  <- r$`orcid-id`     %||% ""
      given  <- r$`given-names`  %||% ""
      family <- r$`family-names` %||% ""
      credit <- r$`credit-name`  %||% ""
      name   <- trimws(paste(c(given, family), collapse = " "))
      if (!nzchar(name)) name <- if (nzchar(credit)) credit else "(name not public)"
      inst   <- if (length(r$`institution-name`) > 0) r$`institution-name`[[1]] %||% "" else ""
      list(orcid = orcid, name = name, institution = inst)
    })
    list(ok = TRUE, mode = "search", results = records)
  }, error = function(e) {
    list(ok = FALSE, error = conditionMessage(e))
  })
}

normalize_doi <- function(doi) {
  doi <- trimws(doi)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE, perl = TRUE)
  doi <- sub("^doi:\\s*", "", doi, ignore.case = TRUE)
  trimws(doi)
}

# Safe single-value extractor: tries each accessor in turn, returns first non-empty string
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

fetch_doi_meta <- function(doi) {
  doi <- normalize_doi(doi)
  if (!nzchar(doi)) return(list(ok = FALSE, error = "No DOI provided."))
  tryCatch({
    meta <- metacheck::crossref_doi(doi)
    if (is.null(meta) || length(meta) == 0)
      return(list(ok = FALSE, error = "No data returned from Crossref."))

    # metacheck::crossref_doi may return a single-row tibble or a plain list.
    # Unwrap a data-frame row to a plain list so field access is uniform.
    if (is.data.frame(meta) && nrow(meta) == 1) {
      meta <- as.list(meta[1, , drop = FALSE])
      # list-columns come through as single-element lists — unwrap one level
      meta <- lapply(meta, function(x) if (is.list(x) && length(x) == 1) x[[1]] else x)
    }

    title <- try_extract(
      function() meta$title[[1]],
      function() meta$title[1],
      function() unlist(meta$title)[1]
    )

    authors_raw <- tryCatch(meta$author, error = function(e) NULL)
    authors_str <- tryCatch(collapse_authors(authors_raw), error = function(e) NA_character_)

    # Crossref date-parts can be nested at varying depths depending on how the
    # tibble row was unwrapped. Walk down any list layers until we hit a number.
    extract_year_from_date_field <- function(field) {
      dp <- field
      if (is.null(dp)) return(NA_character_)
      # Descend through named "date-parts" keys
      for (i in 1:4) {
        if (is.list(dp) && "date-parts" %in% names(dp)) dp <- dp[["date-parts"]]
        else break
      }
      # Unwrap anonymous list layers
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
  }, warning = function(w) FALSE, error = function(e) FALSE)
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
      
      messages <- c(messages, paste0("Extracted ZIP with ", extracted$method, ": ", original_name))
      
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
  
  list(pdfs = pdf_tbl, messages = messages)
}

extract_citation_contexts <- function(pdf_path, pdf_name, grobid_url, window = 3L) {
  xml_path <- safe_pdf2grobid(pdf_path, grobid_url = grobid_url)
  paper <- metacheck::read(xml_path)

  xrefs_all <- paper$xrefs
  bib_all <- paper$bib

  empty_ctx <- function(note) tibble(
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

  xrefs <- xrefs_all %>% filter(type == "bibr")

  if (nrow(xrefs) == 0)
    return(empty_ctx(paste0(
      "No bibliography citations found (type == 'bibr'). xref types seen: ",
      paste(unique(head(xrefs_all$type, 10)), collapse = ", ")
    )))
  
  bib_tbl <- if (!is.null(bib_all) && nrow(bib_all) > 0) {
    bib_all %>%
      mutate(
        ref_authors = purrr::map_chr(authors, collapse_authors),
        ref_title = as.character(title %||% NA_character_),
        ref_year = as.character(year %||% NA),
        ref_doi = as.character(doi %||% NA)
      ) %>%
      rowwise() %>%
      mutate(
        ref_author_last_names = paste(extract_last_names_from_ref_authors(ref_authors), collapse = "; "),
        ref_author_key = make_author_key(extract_last_names_from_ref_authors(ref_authors)),
        ref_lead_author = {
          tmp <- extract_last_names_from_ref_authors(ref_authors)
          if (length(tmp) > 0) tmp[1] else NA_character_
        },
        ref_title_key = make_title_key(ref_title)
      ) %>%
      ungroup() %>%
      select(any_of(c(
        "xref_id", "ref_title", "ref_authors", "ref_author_last_names",
        "ref_author_key", "ref_lead_author", "ref_title_key", "ref_year", "ref_doi"
      )))
  } else {
    tibble(
      xref_id = character(),
      ref_title = character(),
      ref_authors = character(),
      ref_author_last_names = character(),
      ref_author_key = character(),
      ref_lead_author = character(),
      ref_title_key = character(),
      ref_year = character(),
      ref_doi = character()
    )
  }
  
  citation_vec <- as.character(xrefs$contents %||% NA_character_)
  
  out <- xrefs %>%
    left_join(bib_tbl, by = "xref_id") %>%
    transmute(
      file = pdf_name,
      citation = citation_vec,
      sentence = as.character(text %||% NA_character_),
      expanded_text = as.character(text %||% NA_character_),
      ref_title,
      ref_authors,
      ref_author_last_names,
      ref_author_key,
      ref_lead_author,
      ref_title_key,
      ref_year,
      ref_doi,
      citation_lead_author_guess = extract_lead_author_from_citation_vec(citation_vec),
      citation_year_guess = extract_year_guess_vec(citation_vec),
      section = as.character(section %||% NA_character_),
      div = suppressWarnings(as.integer(div)),
      p = suppressWarnings(as.integer(p)),
      s = suppressWarnings(as.integer(s)),
      note = NA_character_
    )
  
  # Build a flat ordered sentence list from the paper.
  # search_text() returns sentences in document order; we keep that order so
  # row indices can be used for ±window expansion.
  sent_list <- tryCatch({
    fs <- metacheck::search_text(paper)
    if (is.null(fs) || nrow(fs) == 0 || !"text" %in% names(fs)) {
      NULL
    } else {
      sents <- as.character(fs$text)
      sents[!is.na(sents) & nzchar(sents)]
    }
  }, error = function(e) NULL)

  # For a given sentence (from xrefs$text, which comes directly from GROBID's
  # <s> element and is semantically correct), find it in the ordered sentence
  # list by text fingerprint, then expand by row index.
  find_and_expand <- function(sentence_text, sents, w) {
    st <- trimws(coalesce(as.character(sentence_text), ""))
    if (!nzchar(st)) return(list(expanded = NA_character_, target = NA_character_))
    if (is.null(sents) || length(sents) == 0) return(list(expanded = st, target = st))

    # Normalise for matching (strip accents, lowercase, collapse whitespace)
    norm <- function(x) tolower(trimws(gsub("\\s+", " ", normalize_text_ascii(x))))
    fp <- substr(norm(st), 1, 60)
    sents_norm <- vapply(sents, norm, character(1), USE.NAMES = FALSE)

    idx <- which(grepl(fp, sents_norm, fixed = TRUE))
    if (length(idx) == 0) {
      # shorter fingerprint fallback
      fp2 <- substr(fp, 1, 30)
      idx <- which(grepl(fp2, sents_norm, fixed = TRUE))
    }

    if (length(idx) == 0) return(list(expanded = st, target = st))

    i <- idx[1]
    window_sents <- sents[max(1L, i - w):min(length(sents), i + w)]
    expanded <- paste(window_sents, collapse = " ")
    target <- trimws(sents[i])
    list(expanded = expanded, target = target)
  }

  expanded_out <- tryCatch({
    results <- lapply(out$sentence, find_and_expand,
                      sents = sent_list, w = window)
    out %>% mutate(
      expanded_text   = vapply(results, `[[`, character(1), "expanded"),
      target_sentence = vapply(results, `[[`, character(1), "target")
    )
  }, error = function(e) {
    out %>% mutate(
      note = coalesce(note, paste("Expansion failed:", conditionMessage(e))),
      target_sentence = NA_character_
    )
  })
  
  expanded_out %>%
    mutate(
      citation = {
        cit_raw  <- coalesce(as.character(citation), "")
        auth     <- coalesce(as.character(ref_lead_author), "")
        yr       <- coalesce(as.character(ref_year), "")
        cit_lbl  <- ifelse(nzchar(cit_raw), paste0("[", cit_raw, "]"), "")
        # only append author/year if they are not already visible in the raw marker
        auth_lbl <- ifelse(nzchar(auth) & !grepl(auth, cit_raw, ignore.case = TRUE, fixed = TRUE),
                           paste0(" \u2014 ", auth), "")
        yr_lbl   <- ifelse(nzchar(yr) & !grepl(yr, cit_raw, fixed = TRUE),
                           paste0(", ", yr), "")
        trimws(paste0(cit_lbl, auth_lbl, yr_lbl))
      }
    )
}

annotate_with_db_matches <- function(citation_df, known_df) {
  if (is.null(citation_df) || nrow(citation_df) == 0) return(citation_df)
  
  if (is.null(known_df) || nrow(known_df) == 0) {
    return(citation_df %>%
             mutate(
               db_known_miscited_paper = "No",
               db_match_score = 0L,
               db_match_reason = NA_character_,
               db_report_count = 0L,
               db_report_ids = NA_character_,
               db_target_author_last_names = NA_character_,
               db_mistake_codes = NA_character_,
               db_mistake_titles = NA_character_,
               future_verdict = "Not implemented"
             ))
  }
  
  out_rows <- vector("list", nrow(citation_df))
  
  for (i in seq_len(nrow(citation_df))) {
    row <- citation_df[i, , drop = FALSE]
    ref_names <- unlist(strsplit(row$ref_author_last_names %||% "", ";\\s*"))
    ref_names <- ref_names[nzchar(ref_names)]
    
    candidates <- known_df %>%
      rowwise() %>%
      mutate(
        target_names_vec = list(unlist(strsplit(target_author_last_names %||% "", ";\\s*"))),
        fuzzy_overlap = fuzzy_author_overlap(ref_names, target_names_vec[[1]], max_compare = 3),
        lead_author_match = ifelse(
          !is.na(row$ref_lead_author) && !is.na(target_lead_author) &&
            author_name_distance_ok(row$ref_lead_author, target_lead_author),
          1L, 0L
        ),
        intext_lead_match = ifelse(
          !is.na(row$citation_lead_author_guess) && !is.na(target_lead_author) &&
            author_name_distance_ok(row$citation_lead_author_guess, target_lead_author),
          1L, 0L
        ),
        year_score = year_close_score(row$ref_year, target_publication_year) +
          year_close_score(row$citation_year_guess, target_publication_year),
        title_overlap = count_title_overlap(row$ref_title_key, target_title_key),
        score =
          ifelse(!is.na(row$ref_author_key) && !is.na(target_author_key) && row$ref_author_key == target_author_key, 6L, 0L) +
          fuzzy_overlap * 2L +
          lead_author_match * 2L +
          intext_lead_match * 1L +
          year_score +
          ifelse(title_overlap >= 2, 2L, ifelse(title_overlap == 1, 1L, 0L))
      ) %>%
      ungroup() %>%
      arrange(desc(score), desc(report_count))
    
    best <- candidates[1, , drop = FALSE]
    score <- if (nrow(best) == 0 || is.na(best$score)) 0L else as.integer(best$score)
    
    reason_bits <- c()
    if (!is.na(best$fuzzy_overlap) && best$fuzzy_overlap >= 2) reason_bits <- c(reason_bits, paste0("author overlap=", best$fuzzy_overlap))
    if (!is.na(best$lead_author_match) && best$lead_author_match == 1) reason_bits <- c(reason_bits, "lead author")
    if (!is.na(best$intext_lead_match) && best$intext_lead_match == 1) reason_bits <- c(reason_bits, "in-text author")
    if (!is.na(best$year_score) && best$year_score >= 2) reason_bits <- c(reason_bits, "year")
    if (!is.na(best$title_overlap) && best$title_overlap >= 1) reason_bits <- c(reason_bits, "title")
    
    out_rows[[i]] <- row %>%
      mutate(
        db_known_miscited_paper = ifelse(
          score >= 5 & (
            is.na(best$target_title_key) |   # no title in DB → rely on score alone
            best$title_overlap >= 1          # title in DB → must have ≥1 token overlap
          ),
          "Yes", "No"
        ),
        db_match_score = score,
        db_match_reason = if (length(reason_bits)) paste(reason_bits, collapse = " + ") else NA_character_,
        db_report_count = ifelse(score >= 5, best$report_count, 0L),
        db_report_ids = ifelse(score >= 5, best$report_ids %||% NA_character_, NA_character_),
        db_target_author_last_names = ifelse(score >= 5, best$target_author_last_names, NA_character_),
        db_mistake_codes = ifelse(score >= 5, best$mistake_codes, NA_character_),
        db_mistake_titles = ifelse(score >= 5, best$mistake_titles, NA_character_),
        db_why_incorrect = ifelse(score >= 5, best$why_incorrect %||% NA_character_, NA_character_),
        db_quoted_text = ifelse(score >= 5, best$quoted_or_paraphrased_text %||% NA_character_, NA_character_),
        future_verdict = "Not implemented"
      )
  }
  
  bind_rows(out_rows) %>%
    arrange(desc(db_known_miscited_paper == "Yes"), desc(db_match_score), desc(db_report_count), file, p, s)
}

VALID_MISTAKE_CODES <- mistake_table$mistake_code  # M1 .. M10

# Build the taxonomy block once, used in the LLM system prompt
TAXONOMY_BLOCK <- paste(
  paste0(
    mistake_table$mistake_code, " \u2013 ",
    mistake_table$mistake_title
  ),
  collapse = "\n"
)

# Expand a raw LLM verdict into display HTML.
# Pure code(s) like "M1" or "M1; M3" get badge + description injected from the
# taxonomy table.  Free text / No / Uncertain pass through as plain text.
confidence_badge <- function(conf) {
  conf <- tolower(trimws(conf))
  switch(conf,
    high   = "<span style='background:#5cb85c;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>high</span>",
    medium = "<span style='background:#f0ad4e;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>medium</span>",
    low    = "<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>low</span>",
    paste0("<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>",
           htmltools::htmlEscape(conf), "</span>")
  )
}

format_llm_verdict_html <- function(verdict) {
  if (is.na(verdict) || !nzchar(trimws(verdict))) return("")

  v <- trimws(verdict)

  if (grepl("^no(\\.?|\\s+known\\s+miscitation\\s+found\\.?)$", v, ignore.case = TRUE))
    return("<span style='color:#5cb85c;font-weight:bold'>No known miscitation found</span>")
  if (grepl("^uncertain\\.?$", v, ignore.case = TRUE))
    return("<span style='color:#f0ad4e;font-weight:bold'>Uncertain</span>")

  # Parse structured lines: "M1 (high): reason" or "Other (low): reason"
  lines <- trimws(unlist(strsplit(v, "\n")))
  lines <- lines[nzchar(lines)]

  # Pattern: CODE (confidence): reason
  pat <- "^(M\\d+|Other)\\s*\\((high|medium|low)\\):\\s*(.+)$"
  parsed <- lapply(lines, function(line) {
    m <- regmatches(line, regexec(pat, line, ignore.case = TRUE))[[1]]
    if (length(m) == 4) list(code = m[2], conf = m[3], reason = m[4])
    else NULL
  })
  parsed <- Filter(Negate(is.null), parsed)

  if (length(parsed) > 0) {
    parts <- vapply(parsed, function(p) {
      code <- toupper(p$code)
      row  <- mistake_table[mistake_table$mistake_code == code, ]
      code_badge <- paste0(
        "<span style='background:#d9534f;color:#fff;border-radius:3px;",
        "padding:1px 5px;font-size:0.85em;font-weight:bold;margin-right:4px'>",
        htmltools::htmlEscape(code), "</span>"
      )
      title_span <- if (nrow(row) > 0)
        paste0("<span style='color:#333;font-weight:bold'>", htmltools::htmlEscape(row$mistake_title), "</span>")
      else ""
      paste0(
        code_badge, confidence_badge(p$conf), title_span,
        "<br><span style='color:#555;font-size:0.9em'>", htmltools::htmlEscape(p$reason), "</span>"
      )
    }, character(1))
    paste(parts, collapse = "<br><br>")
  } else {
    # Fallback: unrecognised format, show as-is
    paste0("<span style='color:#555'><em>", htmltools::htmlEscape(v), "</em></span>")
  }
}

run_llm_check <- function(df, model = "llama-3.1-8b-instant", api_key = Sys.getenv("GROQ_API_KEY"), status_fn = NULL, cancel_fn = NULL) {
  flagged_idx <- which(
    !is.na(df$db_known_miscited_paper) &
    df$db_known_miscited_paper == "Yes" &
    !is.na(df$expanded_text) &
    nzchar(trimws(df$expanded_text))
  )

  df$llm_verdict <- NA_character_
  if (length(flagged_idx) == 0) return(df)

  clean_field <- function(x) {
    v <- trimws(x %||% "")
    if (length(v) == 0 || is.na(v) || !nzchar(v) || v == "NA") "" else v
  }

  # Rows with no known mistake details in the DB cannot be checked — mark them
  # immediately so we don't waste an API call or let the LLM invent mistakes.
  has_db_info <- vapply(flagged_idx, function(i) {
    row <- df[i, , drop = FALSE]
    nzchar(clean_field(row$db_mistake_codes)) ||
    nzchar(clean_field(row$db_why_incorrect)) ||
    nzchar(clean_field(row$db_quoted_text))
  }, logical(1))

  no_info_idx <- flagged_idx[!has_db_info]
  df$llm_verdict[no_info_idx] <- "No known miscitation found"

  flagged_idx <- flagged_idx[has_db_info]
  if (length(flagged_idx) == 0) return(df)

  meta <- lapply(flagged_idx, function(i) {
    row <- df[i, , drop = FALSE]
    list(
      why       = clean_field(row$db_why_incorrect),
      db_quote  = clean_field(row$db_quoted_text),
      db_codes  = clean_field(row$db_mistake_codes),
      db_titles = clean_field(row$db_mistake_titles),
      citation  = trimws(row$expanded_text)
    )
  })

  system_prompt <- paste0(
    "A paper has been reported as commonly miscited in a specific way. ",
    "Your job: decide whether a new citation repeats that exact same wrong claim — not whether it covers the same general topic.\n\n",
    "Steps:\n",
    "1. EXTRACT THE WRONG VARIABLE: From the WRONG CLAIM and HOW TO DETECT sections, identify the specific variable, ",
    "measure, or construct that is wrongly attributed. Write it down.\n",
    "2. CHECK PRESENCE: Does the new citation explicitly name or unambiguously refer to that same specific variable? ",
    "If the variable is absent from the new citation — even if the topic is related — stop here and do NOT flag.\n",
    "3. CHECK THE CLAIM: If the variable IS present, does the new citation make the same wrong claim about it ",
    "as described in WRONG CLAIM? Only flag if both the variable AND the wrong claim match.\n\n",
    "Critical rules:\n",
    "- A citation mentioning related-but-different variables is NOT a match — only the specific wrong variable counts.\n",
    "- A citation that merely mentions the same paper in a broad list is NOT a match.\n",
    "- When in doubt, output 'Uncertain' rather than guessing either way.\n\n",
    "Always respond using EXACTLY these four lines:\n",
    "VARIABLE IDENTIFIED: [the specific wrong variable from the KNOWN MISCITATION]\n",
    "PRESENT IN CITATION: yes/no — [exact short quote from new citation, or 'not found']\n",
    "CLAIM MATCHES: yes/no — [one sentence explaining why]\n",
    "VERDICT: [your verdict line below]\n\n",
    "Verdict if flagging — one line per error:\n",
    "  CODE (confidence): quote the exact phrase from the new citation that names the wrong variable and wrong claim\n",
    "  confidence = high (variable and claim both explicit) / medium (variable present, claim implied) / low (uncertain)\n\n",
    "Verdict if not flagging:\n",
    "  No known miscitation found\n\n",
    "Verdict if genuinely ambiguous:\n",
    "  Uncertain\n\n",
    "Output nothing else."
  )

  # Call Groq API directly via httr2 for reliable error reporting
  call_groq <- function(user_text, system_override = NULL) {
    active_system <- if (!is.null(system_override)) system_override else system_prompt
    repeat {
      resp <- httr2::request("https://api.groq.com/openai/v1/chat/completions") |>
        httr2::req_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(list(
          model       = model,
          messages    = list(
            list(role = "system", content = active_system),
            list(role = "user",   content = user_text)
          ),
          max_tokens  = if (!is.null(system_override)) 600L else 500L,
          temperature = 0.1
        )) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_timeout(60) |>
        httr2::req_perform()

      status <- httr2::resp_status(resp)
      body   <- httr2::resp_body_json(resp)

      if (status == 429) {
        wait_secs <- suppressWarnings(
          as.numeric(httr2::resp_header(resp, "retry-after") %||% "60")
        )
        if (is.na(wait_secs) || wait_secs <= 0) wait_secs <- 60
        wait_secs <- ceiling(wait_secs) + 2L
        cancelled <- FALSE
        for (s in seq_len(wait_secs)) {
          remaining <- wait_secs - s + 1L
          msg <- sprintf(
            "Groq rate limit reached. Waiting %d seconds before retrying... (press Stop to keep results so far)", remaining
          )
          if (!is.null(status_fn)) status_fn(msg)
          Sys.sleep(1)
          later::run_now()
          if (!is.null(cancel_fn) && isTRUE(cancel_fn())) {
            cancelled <- TRUE
            break
          }
        }
        if (cancelled) stop("__CANCELLED__")
        next
      }

      if (status != 200) {
        err <- body$error$message %||% paste("HTTP", status)
        stop(err)
      }
      content <- body$choices[[1]]$message$content %||% ""
      if (!nzchar(trimws(content))) {
        finish <- body$choices[[1]]$finish_reason %||% "?"
        stop(paste0("Empty response from model (finish_reason=", finish,
                    ", model_used=", body$model %||% "?", ")"))
      }
      return(trimws(content))
    }
  }

  # Expand each unique db entry's mistake description once, then build texts
  expand_prompt <- paste0(
    "A paper has been reported as commonly miscited in a specific way. ",
    "Expand the description below into a precise account for use in automated detection.\n\n",
    "Write exactly three sections:\n\n",
    "ACTUAL FINDING: What the cited paper actually found. Be specific about the variable, direction, and conditions.\n\n",
    "WRONG CLAIM: The specific wrong claim researchers make. Name the exact variable(s) involved. ",
    "List only close synonyms for that variable (e.g. for pitch: vocal pitch, fundamental frequency, F0, voice pitch). ",
    "Do NOT expand to related but different variables (e.g. do not list speech rate or intensity when the wrong variable is pitch).\n\n",
    "HOW TO DETECT:\n",
    "- TRIGGER (flag if present): list the specific words or phrases a new citation must contain to be flagged. ",
    "Be narrow — only the wrong variable and its close synonyms.\n",
    "- DO NOT TRIGGER (do not flag even if present): list related variables or claims that look similar but are NOT the same mistake.\n\n",
    "Output only these three sections. No preamble."
  )

  expansion_cache <- list()
  get_expansion <- function(key, raw_text) {
    if (!is.null(expansion_cache[[key]])) return(expansion_cache[[key]])
    if (!is.null(status_fn)) status_fn("Expanding mistake description...")
    expanded <- tryCatch(call_groq(raw_text, system_override = expand_prompt),
                         error = function(e) NULL)
    if (is.null(expanded) || !nzchar(trimws(expanded))) return("__EXPANSION_FAILED__")
    expansion_cache[[key]] <<- expanded
    expanded
  }

  expansion_failed <- logical(length(meta))
  texts <- vapply(seq_along(meta), function(j) {
    m <- meta[[j]]
    raw_desc <- paste0(
      if (nzchar(m$db_quote)) paste0("Prototypical wrong claim: ", m$db_quote, "\n") else "",
      if (nzchar(m$why))      paste0("Why it is wrong: ", m$why, "\n") else ""
    )
    cache_key <- paste0(m$db_codes, "|", substr(raw_desc, 1, 120))
    expanded  <- get_expansion(cache_key, raw_desc)

    if (identical(expanded, "__EXPANSION_FAILED__")) {
      expansion_failed[j] <<- TRUE
      return("")
    }

    known_mistake <- paste0(
      "KNOWN MISCITATION\n",
      if (nzchar(m$db_codes)) paste0("Error code: ", m$db_codes,
        if (nzchar(m$db_titles)) paste0(" (", m$db_titles, ")") else "", "\n") else "",
      expanded
    )
    paste0(known_mistake, "\nNEW CITATION TO CHECK\n", m$citation)
  }, character(1))

  raw_answers <- character(length(texts))
  cancelled_early <- FALSE
  for (ci in seq_along(texts)) {
    if (expansion_failed[ci]) {
      raw_answers[[ci]] <- "Uncertain (expansion failed)"
      next
    }
    result_i <- tryCatch(
      call_groq(texts[[ci]]),
      error = function(e) {
        if (conditionMessage(e) == "__CANCELLED__") {
          cancelled_early <<- TRUE
          NA_character_
        } else {
          paste0("ERROR: ", conditionMessage(e))
        }
      }
    )
    raw_answers[[ci]] <- if (is.na(result_i)) "" else result_i
    if (cancelled_early) break
  }

  completed <- if (cancelled_early) ci - 1L else length(raw_answers)
  debug_info <- paste0("model=", model, " | n=", completed, "/", length(raw_answers),
                       " | raw: ", paste(raw_answers[seq_len(completed)], collapse=" | "))

  # Extract only the VERDICT line from chain-of-thought responses
  extract_verdict <- function(raw) {
    if (!nzchar(trimws(raw))) return(raw)
    lines <- trimws(unlist(strsplit(raw, "\n")))
    verdict_lines <- grep("^VERDICT:\\s*", lines, value = TRUE)
    if (length(verdict_lines) > 0)
      return(trimws(sub("^VERDICT:\\s*", "", verdict_lines[1])))
    raw  # fallback: model didn't use the template
  }

  done_idx <- flagged_idx[seq_len(completed)]
  df$llm_verdict[done_idx] <- vapply(raw_answers[seq_len(completed)], extract_verdict, character(1))
  list(df = df, raw_answers = raw_answers[seq_len(completed)], flagged_idx = done_idx,
       debug_info = debug_info, texts = texts, cancelled = cancelled_early)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      table.dataTable tbody td { vertical-align: top; }
      .wide-text { white-space: normal !important; word-break: break-word; }
      .small-note { color:#666; font-size:0.95em; }
      .danger-box { border:1px solid #d9534f; padding:12px; border-radius:8px; background:#fff5f5; }
      .section-card { border:1px solid #ddd; border-radius:8px; padding:10px 14px; margin-bottom:12px; }
      .section-header { display:flex; justify-content:space-between; align-items:center; cursor:pointer; }
      .collapsed-body { display:none; margin-top:12px; }
    "))
  ),
  
  titlePanel("Miscitation database + citation analyzer"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Analyze citations",
      br(),
      fluidRow(
        column(
          width = 3,
          wellPanel(
            fileInput("files", "Upload PDF(s) or ZIP file(s)", multiple = TRUE, accept = c(".pdf", ".zip")),
            textInput("grobid_url", "GROBID URL", value = "http://localhost:8070"),
            
            hr(),
            
            textInput("citation_filter", "Filter citation string", placeholder = "e.g. Smith, [12], 2020"),
            textInput("author_filter", "Filter cited author", placeholder = "e.g. Kahneman"),
            checkboxInput("only_known_miscited", "Only show citations to papers in the database", value = FALSE),
            
            hr(),
            
            actionButton("test_grobid", "Test GROBID"),
            actionButton("test_pdf2grobid", "Test first PDF"),
            br(), br(),
            actionButton("process", "Process files", class = "btn-primary"),
            br(), br(),
            downloadButton("download_csv", "Download CSV"),

            hr(),

            strong("LLM miscitation check"),
            helpText(class = "small-note",
                     "Checks flagged rows (matched in miscitation database) using an LLM via Groq."),
            passwordInput("groq_api_key", "Groq API key",
                          placeholder = "gsk_... (or set GROQ_API_KEY in .Renviron)"),
            selectInput("llm_model", "Model",
              choices = c(
                "llama-3.1-8b-instant (fast, cheap)" = "llama-3.1-8b-instant",
                "llama3-8b-8192 (stable)" = "llama3-8b-8192",
                "llama-3.3-70b-versatile (more capable)" = "llama-3.3-70b-versatile",
                "llama-4-scout-17b (balanced)" = "meta-llama/llama-4-scout-17b-16e-instruct"
              ),
              selected = "llama-3.3-70b-versatile"
            ),
            actionButton("run_llm", "Run LLM check", class = "btn-warning"),
            shinyjs::hidden(actionButton("stop_llm", "Stop & keep results", class = "btn-danger")),
            br(), br(),
            verbatimTextOutput("llm_status")
          )
        ),
        
        column(
          width = 9,
          fluidRow(
            column(3, wellPanel(strong("Uploaded"), textOutput("stat_uploaded"))),
            column(3, wellPanel(strong("PDFs found"), textOutput("stat_pdfs"))),
            column(3, wellPanel(strong("Processed"), textOutput("stat_processed"))),
            column(3, wellPanel(strong("Success / Fail"), textOutput("stat_success_fail")))
          ),
          
          div(
            class = "section-card",
            div(
              class = "section-header",
              tags$div(
                tags$strong("Known miscited entries in database"),
                tags$div(class = "small-note", "Collapsed by default.")
              ),
              actionLink("toggle_known_tbl", label = "Show")
            ),
            div(
              id = "known_tbl_body",
              class = "collapsed-body",
              DTOutput("known_miscited_table")
            )
          ),
          
          tabsetPanel(
            tabPanel("Results", DTOutput("results_table")),
            tabPanel("Live log", verbatimTextOutput("live_log")),
            tabPanel("Diagnostics", verbatimTextOutput("diagnostic_text")),
            tabPanel("Status", verbatimTextOutput("status_text"))
          )
        )
      )
    ),
    
    tabPanel(
      "Report a miscitation",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "reporter_role",
            "You are submitting as",
            choices = c("Author of the cited work", "Co-author", "Reader", "Other"),
            selected = "Author of the cited work"
          ),
          tags$label("Your ORCID (optional)"),
          fluidRow(
            column(8, textInput("reporter_orcid", label = NULL,
                                placeholder = "ORCID iD or search by name")),
            column(4, actionButton("lookup_orcid", "Search", class = "btn-sm btn-default",
                                   style = "margin-top:0"))
          ),
          helpText(class = "small-note",
                   "Enter your ORCID iD to verify, or type your name to search."),
          uiOutput("orcid_meta_box"),
          helpText(
            class = "small-note",
            "Providing your ORCID increases the credibility of your report, ",
            "especially if you are an author of the cited work. ",
            "Anonymous reports are accepted but will be flagged for extra scrutiny."
          ),

          hr(),

          tags$label("DOI of the cited work *"),
          fluidRow(
            column(8, textInput("own_work_doi", label = NULL, placeholder = "10.xxxx/xxxxx")),
            column(4, actionButton("lookup_doi", "Look up", class = "btn-sm btn-default", style = "margin-top:0"))
          ),
          uiOutput("doi_meta_box"),

          tags$label("DOI of the citing paper (optional)"),
          helpText(class = "small-note",
                   "Leave blank to submit a general warning about how your work is commonly miscited."),
          textInput("citing_work_doi", label = NULL, placeholder = "10.xxxx/xxxxx"),

          hr(),

          textAreaInput(
            "quoted_or_paraphrased_text",
            "Miscited text (optional)",
            placeholder = "Paste the exact sentence(s) from the citing paper, or a paraphrase of the claim.",
            rows = 4
          ),

          textAreaInput(
            "why_incorrect",
            "Explanation *",
            placeholder = "Explain what is wrong and what the original work actually shows. You may also include a recommendation on how to correctly cite the work.",
            rows = 5
          ),

          selectizeInput(
            "mistake_codes",
            "Miscitation type(s) (optional)",
            choices = mistake_choices,
            multiple = TRUE,
            options = list(
              placeholder = "Select one or more miscitation types",
              plugins = list("remove_button")
            )
          ),

          actionButton("submit_report", "Submit report", class = "btn-primary"),
          br(), br(),
          verbatimTextOutput("report_status"),

          hr(),

          div(
            class = "danger-box",
            strong("Danger zone"),
            br(), br(),
            checkboxInput("confirm_erase_db", "I understand this will erase the miscitation database", value = FALSE),
            actionButton("erase_db", "Erase database", class = "btn-danger"),
            br(), br(),
            verbatimTextOutput("erase_status")
          )
        ),

        mainPanel(
          h4("Miscitation categories"),
          tableOutput("mistake_table_out")
        )
      )
    )
  )
)

spinner_tag <- tags$span(
  style = paste0(
    "display:inline-block; width:10px; height:10px; border:2px solid #aaa;",
    "border-top-color:#555; border-radius:50%;",
    "animation:spin 0.7s linear infinite; margin-right:6px;",
    "vertical-align:middle"
  )
)

sync_db_from_github()

server <- function(input, output, session) {
  init_db_if_missing()
  
  diagnostics <- reactiveVal("No diagnostics run yet.")
  process_status <- reactiveVal("No processing run yet.")
  report_status <- reactiveVal("")
  erase_status <- reactiveVal("")
  llm_status <- reactiveVal("")
  llm_cancel <- reactiveVal(FALSE)
  raw_data <- reactiveVal(tibble())
  live_log <- reactiveVal(character())
  known_tbl_open <- reactiveVal(FALSE)
  
  stats <- reactiveValues(
    uploaded = 0,
    pdfs = 0,
    processed = 0,
    success = 0,
    fail = 0
  )
  
  reports_db_rv <- reactiveVal(read_reports_db())
  crossref_meta <- reactiveVal(NULL)
  orcid_meta    <- reactiveVal(NULL)

  orcid_search_results <- reactiveVal(list())

  observeEvent(input$lookup_orcid, {
    query <- trimws(input$reporter_orcid)
    if (!nzchar(query)) {
      orcid_meta(list(ok = FALSE, error = "Please enter a name or ORCID iD."))
      return()
    }

    # Detect mode: looks like an ORCID iD after normalization?
    query_norm <- normalize_orcid(query)
    is_orcid   <- grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", query_norm)

    if (is_orcid && query_norm != query)
      updateTextInput(session, "reporter_orcid", value = query_norm)

    orcid_meta(list(ok = FALSE, loading = TRUE))
    shinyjs::disable("lookup_orcid")
    updateActionButton(session, "lookup_orcid", label = "Searching\u2026")

    session$onFlushed(function() {
      meta <- if (is_orcid) fetch_orcid_meta(query_norm) else search_orcid_by_name(query)
      if (isTRUE(meta$ok) && meta$mode == "search")
        orcid_search_results(meta$results)
      orcid_meta(meta)
      shinyjs::enable("lookup_orcid")
      updateActionButton(session, "lookup_orcid", label = "Search")
    }, once = TRUE)
  })

  # When the user clicks one of the search result buttons, fill in the ORCID field
  observeEvent(input$orcid_select, {
    results <- orcid_search_results()
    idx     <- as.integer(input$orcid_select)
    req(idx >= 1, idx <= length(results))
    chosen <- results[[idx]]
    updateTextInput(session, "reporter_orcid", value = chosen$orcid)
    orcid_meta(list(ok = TRUE, mode = "verify", name = chosen$name, orcid = chosen$orcid))
  })

  output$orcid_meta_box <- renderUI({
    m <- orcid_meta()
    if (is.null(m)) return(NULL)

    if (isTRUE(m$loading))
      return(div(style = "color:#888; font-size:0.9em; margin-bottom:8px", spinner_tag, "Searching\u2026"))

    if (!isTRUE(m$ok))
      return(div(class = "small-note", style = "color:#c00; margin-bottom:8px", m$error))

    if (m$mode == "search") {
      results <- orcid_search_results()
      if (length(results) == 0)
        return(div(class = "small-note", style = "margin-bottom:8px", "No results found."))

      rows <- lapply(seq_along(results), function(i) {
        r <- results[[i]]
        tags$div(
          style = "padding:5px 0; border-bottom:1px solid #eee; cursor:pointer",
          onclick = sprintf("Shiny.setInputValue('orcid_select', %d, {priority: 'event'})", i),
          tags$b(r$name),
          tags$span(class = "small-note", style = "margin-left:6px", r$orcid),
          if (nzchar(r$institution))
            tags$span(class = "small-note", style = "margin-left:6px; color:#888",
                      paste0("\u2014 ", r$institution))
          else NULL
        )
      })
      return(div(
        style = "background:#f5f5f5; border-radius:4px; padding:6px 10px; margin-bottom:8px; font-size:0.92em",
        tags$p(class = "small-note", style = "margin:0 0 4px",
               "Click a result to select:"),
        tagList(rows)
      ))
    }

    # Verify mode
    div(
      style = "background:#f5f5f5; border-radius:4px; padding:8px 10px; margin-bottom:8px; font-size:0.92em",
      tags$b(m$name), br(),
      tags$span(class = "small-note", m$orcid)
    )
  })

  observeEvent(input$lookup_doi, {
    doi_raw <- trimws(input$own_work_doi)
    if (!nzchar(doi_raw)) {
      crossref_meta(list(ok = FALSE, error = "Please enter a DOI first."))
      return()
    }
    doi_clean <- normalize_doi(doi_raw)
    if (doi_clean != doi_raw) updateTextInput(session, "own_work_doi", value = doi_clean)

    # Show loading state and disable button before the blocking fetch.
    # session$onFlushed ensures the browser receives these updates *before*
    # fetch_doi_meta blocks the R session.
    crossref_meta(list(ok = FALSE, loading = TRUE))
    shinyjs::disable("lookup_doi")
    updateActionButton(session, "lookup_doi", label = "Looking up\u2026")

    session$onFlushed(function() {
      meta <- fetch_doi_meta(doi_clean)
      crossref_meta(meta)
      shinyjs::enable("lookup_doi")
      updateActionButton(session, "lookup_doi", label = "Look up")
    }, once = TRUE)
  })

  output$doi_meta_box <- renderUI({
    m <- crossref_meta()
    if (is.null(m)) return(NULL)
    if (isTRUE(m$loading)) {
      return(div(
        style = "color:#888; font-size:0.9em; margin-bottom:8px",
        spinner_tag,
        "Looking up\u2026",
        tags$style("@keyframes spin { to { transform: rotate(360deg); } }")
      ))
    }
    if (!isTRUE(m$ok)) {
      return(div(class = "small-note", style = "color:#c00; margin-bottom:8px", m$error))
    }
    div(
      style = "background:#f5f5f5; border-radius:4px; padding:8px 10px; margin-bottom:8px; font-size:0.92em",
      tags$b(m$title), br(),
      tags$span(class = "small-note", m$authors),
      if (nzchar(m$year)) tags$span(class = "small-note", paste0(" (", m$year, ")")) else NULL
    )
  })

  append_log <- function(...) {
    msg <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", paste(..., collapse = ""))
    live_log(c(live_log(), msg))
    message(msg)
  }
  
  reset_log_and_stats <- function() {
    live_log(character())
    stats$uploaded <- 0
    stats$pdfs <- 0
    stats$processed <- 0
    stats$success <- 0
    stats$fail <- 0
  }
  
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
  
  known_miscited_df <- reactive({
    summarise_known_miscitations(reports_db_rv())
  })
  
  output$known_miscited_table <- renderDT({
    datatable(
      known_miscited_df(),
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 8, scrollX = TRUE, dom = "tip")
    )
  })
  
  output$mistake_table_out <- renderTable({
    mistake_table
  }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")
  
  output$erase_status <- renderText({
    erase_status()
  })
  
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
    alive <- check_grobid_alive(grobid_url)
    
    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      diagnostics(paste(
        "PDF2GROBID TEST",
        "----------------",
        paste("GROBID URL:", grobid_url),
        "Result: FAILED",
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "",
        "Response body:",
        substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      ))
      return()
    }
    
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
    process_status("Checking GROBID connection...")
    
    grobid_url <- trimws(input$grobid_url)
    alive <- check_grobid_alive(grobid_url)
    
    if (!isTRUE(alive$ok) || is.na(alive$status) || alive$status < 200 || alive$status >= 300) {
      msg <- paste(
        "Processing stopped.",
        paste("GROBID URL:", grobid_url),
        "Reason: GROBID is not reachable or not healthy.",
        paste("HTTP status:", alive$status %||% NA),
        "",
        "Response body:",
        substr(alive$body %||% "", 1, 1000),
        sep = "\n"
      )
      process_status(msg)
      append_log(msg)
      return()
    }
    
    process_status("Processing started...")
    append_log("Starting processing run")
    stats$uploaded <- nrow(input$files)
    
    collected <- collect_uploaded_pdfs(input$files)
    pdfs <- collected$pdfs
    
    for (m in collected$messages) append_log(m)
    
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
            grobid_url = grobid_url
          ) %>%
            mutate(source_upload = source_upload, .before = file)
          
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
          
          status_messages <<- c(
            status_messages,
            paste0("Failed: ", pdf_name, " -> ", conditionMessage(e))
          )
          
          tibble(
            source_upload = source_upload,
            file = pdf_name,
            citation = NA_character_,
            sentence = NA_character_,
            expanded_text = NA_character_,
            ref_title = NA_character_,
            ref_authors = NA_character_,
            ref_author_last_names = NA_character_,
            ref_author_key = NA_character_,
            ref_lead_author = NA_character_,
            ref_title_key = NA_character_,
            ref_year = NA_character_,
            ref_doi = NA_character_,
            citation_lead_author_guess = NA_character_,
            citation_year_guess = NA_character_,
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
    
    combined <- bind_rows(all_results)
    combined <- annotate_with_db_matches(combined, known_miscited_df())
    
    raw_data(combined)
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
          str_detect(coalesce(ref_authors, ""), regex(trimws(input$author_filter), ignore_case = TRUE)) |
            str_detect(coalesce(ref_author_last_names, ""), regex(trimws(input$author_filter), ignore_case = TRUE))
        )
    }
    
    if (isTRUE(input$only_known_miscited)) {
      dat <- dat %>%
        filter(db_known_miscited_paper == "Yes")
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

    # Build HTML for expanded_text:
    #   - show only the target (citing) sentence wrapped in <mark> by default
    #   - full context hidden behind a native <details> toggle
    if ("target_sentence" %in% names(dat) && "expanded_text" %in% names(dat)) {
      dat$expanded_text <- mapply(function(et, ts) {
        et <- trimws(coalesce(as.character(et), ""))
        ts <- trimws(coalesce(as.character(ts), ""))
        et_html <- htmltools::htmlEscape(et)
        ts_html <- trimws(htmltools::htmlEscape(ts))

        if (!nzchar(ts_html)) return(et_html)

        # Mark the target sentence within the full context
        et_marked <- if (nzchar(et_html) && grepl(ts_html, et_html, fixed = TRUE)) {
          sub(ts_html, paste0("<mark>", ts_html, "</mark>"), et_html, fixed = TRUE)
        } else {
          et_html
        }

        marked_ts <- paste0("<mark>", ts_html, "</mark>")

        if (!nzchar(et_html) || et_html == ts_html) {
          # No surrounding context — just show the marked sentence
          marked_ts
        } else {
          paste0(
            marked_ts,
            "<details><summary style='color:#888;font-size:0.82em;",
            "cursor:pointer;padding-top:3px'>&#9660;&nbsp;context</summary>",
            "<div style='margin-top:4px;color:#444'>", et_marked, "</div>",
            "</details>"
          )
        }
      }, dat$expanded_text, dat$target_sentence, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    }

    # Clear match details for non-flagged rows
    if ("db_known_miscited_paper" %in% names(dat)) {
      is_flagged <- !is.na(dat$db_known_miscited_paper) & dat$db_known_miscited_paper == "Yes"
      if ("db_match_reason" %in% names(dat))
        dat$db_match_reason[!is_flagged] <- NA_character_
    }

    default_visible <- c(
      "file",
      "citation",
      "expanded_text",
      if ("llm_verdict" %in% names(dat) && any(!is.na(dat$llm_verdict))) "llm_verdict" else NULL
    )

    hide_targets <- which(!(names(dat) %in% default_visible)) - 1

    # Format llm_verdict into display HTML, appending matched case ID(s)
    if ("llm_verdict" %in% names(dat)) {
      has_rids <- "db_report_ids" %in% names(dat)
      has_db_codes <- "db_mistake_codes" %in% names(dat)
      dat$llm_verdict <- vapply(seq_len(nrow(dat)), function(i) {
        v <- format_llm_verdict_html(dat$llm_verdict[i])
        if (!nzchar(v)) return(v)

        # Don't append DB details when no miscitation was found
        raw_v <- trimws(dat$llm_verdict[i] %||% "")
        if (grepl("^no(\\.?|\\s+known\\s+miscitation\\s+found\\.?)$", raw_v, ignore.case = TRUE))
          return(v)

        # "Reported as" line from the database
        db_codes <- if (has_db_codes) trimws(dat$db_mistake_codes[i] %||% "") else ""
        if (is.na(db_codes)) db_codes <- ""
        reported_line <- if (nzchar(db_codes) && db_codes != "NA") {
          codes <- trimws(unlist(strsplit(db_codes, ";")))
          code_spans <- paste(vapply(codes, function(cd) {
            paste0("<span style='background:#888;color:#fff;border-radius:3px;",
                   "padding:1px 4px;font-size:0.78em;font-weight:bold;margin-right:2px'>",
                   htmltools::htmlEscape(cd), "</span>")
          }, character(1)), collapse = "")
          paste0("<span style='color:#aaa;font-size:0.8em'>Reported as: ", code_spans, "</span>")
        } else ""

        # Case ID line
        rids <- if (has_rids) dat$db_report_ids[i] else NA_character_
        case_line <- if (!is.na(rids) && nzchar(trimws(rids)) && trimws(rids) != "NA") {
          ids <- trimws(unlist(strsplit(rids, ";")))
          id_spans <- paste(vapply(ids, function(id) {
            paste0("<code style='font-size:0.78em;color:#aaa'>", htmltools::htmlEscape(trimws(id)), "</code>")
          }, character(1)), collapse = " ")
          paste0("<span style='color:#aaa;font-size:0.8em'>Case: ", id_spans, "</span>")
        } else ""

        footer <- paste(c(reported_line, case_line)[nzchar(c(reported_line, case_line))], collapse = "<br>")
        if (nzchar(footer)) paste0(v, "<br>", footer) else v
      }, character(1))
    }

    # Escape all columns except those containing safe HTML
    html_cols <- intersect(c("expanded_text", "llm_verdict"), names(dat))
    escape_cols <- setdiff(seq_along(names(dat)), which(names(dat) %in% html_cols))

    col_defs <- list(
      list(targets = which(names(dat) == "file") - 1,        width = "12%"),
      list(targets = which(names(dat) == "citation") - 1,    width = "14%"),
      list(targets = which(names(dat) == "expanded_text") - 1, width = "60%",
           className = "wide-text"),
      list(targets = hide_targets, visible = FALSE)
    )
    if ("llm_verdict" %in% names(dat)) {
      col_defs <- c(col_defs, list(
        list(targets = which(names(dat) == "llm_verdict") - 1, width = "14%",
             className = "wide-text")
      ))
    }

    datatable(
      dat,
      escape = escape_cols,
      extensions = c("Buttons"),
      filter = "top",
      rownames = FALSE,
      class = "cell-border stripe compact",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = "Bfrtip",
        buttons = list(
          list(extend = "colvis", text = "Show / hide columns")
        ),
        columnDefs = col_defs
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
  
  output$report_status <- renderText({
    report_status()
  })
  
  observeEvent(input$submit_report, {
    errs <- c()

    own_doi <- normalize_doi(input$own_work_doi %||% "")
    if (!nzchar(own_doi)) errs <- c(errs, "DOI of the cited work is required.")
    if (required_msg(input$why_incorrect)) errs <- c(errs, "An explanation is required.")

    if (length(errs) > 0) {
      report_status(paste("Please fix:\n- ", paste(errs, collapse = "\n- ")))
      return()
    }

    # Use Crossref-fetched metadata if available, otherwise fall back to empty
    meta <- crossref_meta()
    meta_ok <- isTRUE(meta$ok)

    fetched_authors_str  <- if (meta_ok) meta$authors %||% "" else ""
    fetched_year         <- if (meta_ok) meta$year    %||% "" else ""
    fetched_title        <- if (meta_ok) meta$title   %||% "" else ""

    submitted_last_names <- extract_last_names_from_ref_authors(fetched_authors_str)
    author_key           <- make_author_key(submitted_last_names)
    lead_author          <- if (length(submitted_last_names) > 0) submitted_last_names[1] else NA_character_
    title_key            <- make_title_key(fetched_title)

    selected_codes  <- input$mistake_codes %||% character(0)
    selected_titles <- get_mistake_titles(selected_codes)

    rid <- make_report_id(
      orcid      = trimws(input$reporter_orcid %||% ""),
      own_doi    = own_doi,
      citing_doi = trimws(input$citing_work_doi %||% "")
    )

    row <- data.frame(
      report_id                 = rid,
      submitted_at              = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      reporter_role             = as.character(input$reporter_role %||% ""),
      reporter_orcid            = as.character(input$reporter_orcid %||% ""),
      own_work_doi              = own_doi,
      citing_work_doi           = as.character(input$citing_work_doi %||% ""),
      target_author_last_names  = paste(submitted_last_names, collapse = "; "),
      target_lead_author        = lead_author %||% NA_character_,
      target_author_key         = author_key %||% NA_character_,
      target_publication_year   = fetched_year,
      target_title_clue         = substr(fetched_title, 1, 80),
      target_title_key          = title_key %||% NA_character_,
      mistake_codes             = paste(selected_codes, collapse = "; "),
      mistake_titles            = paste(selected_titles, collapse = "; "),
      quoted_or_paraphrased_text = as.character(input$quoted_or_paraphrased_text %||% ""),
      why_incorrect             = as.character(input$why_incorrect %||% ""),
      stringsAsFactors = FALSE
    )

    tryCatch({
      append_row_safely(row)

      refreshed_db <- read_reports_db()
      reports_db_rv(refreshed_db)

      gh_status <- push_db_to_github(refreshed_db, paste0("report: ", rid))

      report_status(paste0(
        "Submitted \u2713\n",
        "Report ID: ", rid, "\n",
        "Saved to: ", DB_PATH, "\n",
        if (meta_ok) paste0("Cited work: ", fetched_title, " (", fetched_year, ")\n") else "",
        if (length(selected_codes) > 0) paste0("Miscitation type(s): ", paste(selected_codes, collapse = ", "), "\n") else "",
        gh_status, "\n"
      ))

      updateTextInput(session, "own_work_doi", value = "")
      updateTextInput(session, "citing_work_doi", value = "")
      updateTextInput(session, "reporter_orcid", value = "")
      updateSelectizeInput(session, "mistake_codes", selected = character(0))
      updateTextAreaInput(session, "quoted_or_paraphrased_text", value = "")
      updateTextAreaInput(session, "why_incorrect", value = "")
      crossref_meta(NULL)
      orcid_meta(NULL)
      orcid_search_results(list())

      tryCatch({
        current_results <- raw_data()
        if (!is.null(current_results) && nrow(current_results) > 0) {
          refreshed_known <- summarise_known_miscitations(refreshed_db)
          raw_data(annotate_with_db_matches(current_results, refreshed_known))
        }
      }, error = function(e) {
        message("Post-submit analyzer refresh failed: ", conditionMessage(e))
      })

    }, error = function(e) {
      report_status(paste("Error:", conditionMessage(e)))
    })
  })
  
  observeEvent(input$erase_db, {
    erase_status("")

    if (!isTRUE(input$confirm_erase_db)) {
      erase_status("Tick the confirmation box before erasing the database.")
      return()
    }

    tryCatch({
      backup_path <- erase_database_safely()
      reports_db_rv(read_reports_db())
      raw_data(tibble())
      erase_status(paste0(
        "Database erased.\nBackup saved to: ", backup_path
      ))
      report_status("")
      process_status("Database erased. Results cleared.")
      updateCheckboxInput(session, "confirm_erase_db", value = FALSE)
    }, error = function(e) {
      erase_status(paste("Error:", conditionMessage(e)))
    })
  })

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

    selected_model <- input$llm_model   # capture now — not accessible inside onFlushed
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
        result <- llm_out$df
        raw_data(result)
        n_done <- length(llm_out$flagged_idx)
        stopped_early <- isTRUE(llm_out$cancelled)
        verdict_lines <- if (n_done > 0) {
          paste(vapply(seq_along(llm_out$flagged_idx), function(j) {
            i <- llm_out$flagged_idx[j]
            cit <- trimws(result$citation[i] %||% paste("Row", i))
            cit_short <- if (nchar(cit) > 60) paste0(substr(cit, 1, 60), "...") else cit
            raw <- llm_out$raw_answers[j]
            normalized <- result$llm_verdict[i]
            paste0("  \u2022 ", cit_short,
                   "\n    Raw: ", if (is.na(raw) || !nzchar(raw)) "(empty)" else raw,
                   "\n    Verdict: ", if (is.na(normalized) || !nzchar(normalized)) "(empty)" else normalized)
          }, character(1)), collapse = "\n")
        } else ""
        prefix <- if (stopped_early) paste0("Stopped early. LLM checked ", n_done, " row(s) (partial results kept).\n")
                  else paste0("Done. LLM checked ", n_done, " row(s).\n")
        llm_status(paste0(
          prefix,
          verdict_lines, "\n\n",
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

shinyApp(ui, server)