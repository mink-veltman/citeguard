#' Run LLM miscitation check on annotated citation data
#'
#' For each row in \code{df} flagged by \code{\link{annotate_with_db_matches}}
#' (i.e. \code{db_known_miscited_paper == "Yes"}) with non-empty
#' \code{expanded_text}, constructs a structured prompt and calls
#' \code{metacheck::llm()} via the Groq API. Rows without database provenance
#' information (no \code{db_why_incorrect}, \code{db_mistake_codes}, or
#' \code{db_quoted_text}) are assigned \code{"No known miscitation found"}
#' without an LLM call.
#'
#' @param df A tibble as returned by \code{\link{annotate_with_db_matches}}.
#' @param model Character. Groq model identifier.
#'   Default \code{"llama-3.3-70b-versatile"}.
#' @param api_key Character. Groq API key.
#'   Defaults to \code{Sys.getenv("GROQ_API_KEY")}.
#' @param status_fn Optional function of one character argument. Called after
#'   each LLM response with a progress message such as
#'   \code{"LLM checked 3/10 row(s)..."}. Pass \code{NULL} (default) to
#'   suppress progress output. In a Shiny context, pass a \code{reactiveVal}
#'   setter.
#' @param cancel_fn Optional zero-argument function returning a logical. If it
#'   returns \code{TRUE}, the loop is interrupted after the current request
#'   completes and results are returned for all rows processed so far. Pass
#'   \code{NULL} (default) to run without cancellation support.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{df}{The input tibble with an added \code{llm_verdict} column.}
#'     \item{raw_answers}{Character vector of unprocessed LLM responses for
#'       completed rows.}
#'     \item{flagged_idx}{Integer indices into \code{df} that were processed.}
#'     \item{debug_info}{Character. Summary string for diagnostics.}
#'     \item{texts}{Character vector of the full prompt texts sent to the API.}
#'     \item{cancelled}{Logical. \code{TRUE} if the loop was interrupted by
#'       \code{cancel_fn}.}
#'   }
#'
#' @seealso \code{\link{annotate_with_db_matches}}, \code{\link{analyze_citations}}
#' @export
run_llm_check <- function(df, model = "llama-3.3-70b-versatile", api_key = Sys.getenv("GROQ_API_KEY"), status_fn = NULL, cancel_fn = NULL) {
  flagged_idx <- which(
    !is.na(df$db_known_miscited_paper) &
    df$db_known_miscited_paper == "Yes" &
    !is.na(df$expanded_text) &
    nzchar(trimws(df$expanded_text))
  )

  df$llm_verdict <- NA_character_
  if (length(flagged_idx) == 0) return(df)

  metacheck::llm_use(TRUE)
  metacheck::llm_max_calls(length(flagged_idx) + 5L)

  # Treats "NA" (literal string from CSV round-trips) as blank before
  # injecting database fields into the LLM prompt.
  clean_field <- function(x) {
    v <- trimws(x %||% "")
    if (length(v) == 0 || is.na(v) || !nzchar(v) || v == "NA") "" else v
  }

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
      why        = clean_field(row$db_why_incorrect),
      db_quote   = clean_field(row$db_quoted_text),
      db_codes   = clean_field(row$db_mistake_codes),
      db_titles  = clean_field(row$db_mistake_titles),
      ref_title  = clean_field(row$ref_title),
      ref_authors= clean_field(row$ref_authors),
      ref_year   = clean_field(row$ref_year),
      section    = clean_field(row$section),
      sentence   = clean_field(row$sentence),
      citation   = trimws(row$expanded_text)
    )
  })

  # Four-step chain-of-thought structure:
  #   VARIABLE IDENTIFIED / PRESENT IN CITATION / CORRECTLY CHARACTERIZED / VERDICT
  # Forcing the model to first name the specific variable dramatically reduces
  # false positives from topically-related-but-not-matching citations.
  system_prompt <- paste0(
    "A paper has been reported as commonly miscited. ",
    "Your job: decide whether a new citation misrepresents what that paper actually found.\n\n",
    "Steps:\n",
    "1. IDENTIFY THE VARIABLE: From 'Prototypical wrong claim' and 'Why it is wrong', name the specific variable ",
    "or construct that is being misrepresented (e.g. 'pitch', 'eye contact').\n",
    "2. CHECK PRESENCE: Does the new citation explicitly mention that variable? ",
    "If the variable is absent — even if the topic is related — stop here and do NOT flag.\n",
    "3. CHECK ACCURACY: If the variable IS present, does the new citation correctly describe what the cited paper ",
    "actually found? Use 'Why it is wrong' as your ground truth for what the paper really found. ",
    "Flag if the citation gets it wrong in ANY direction — not just the prototypical wrong direction.\n\n",
    "Critical rules:\n",
    "- A citation mentioning related-but-different variables is NOT a match.\n",
    "- A citation that merely lists the paper among many references without making a specific claim is NOT a match.\n",
    "- When genuinely unsure, output 'Uncertain'.\n\n",
    "Always respond using EXACTLY these four lines:\n",
    "VARIABLE IDENTIFIED: [the specific variable]\n",
    "PRESENT IN CITATION: yes/no — [exact short quote, or 'not found']\n",
    "CORRECTLY CHARACTERIZED: yes/no — [one sentence: what the citation claims vs. what the paper actually found]\n",
    "VERDICT: [your verdict line below]\n\n",
    "Verdict if flagging — one line per error:\n",
    "  CODE (confidence): quote the exact phrase from the new citation that misrepresents the finding\n",
    "  confidence = high (clear misrepresentation) / medium (likely wrong) / low (uncertain)\n\n",
    "Verdict if not flagging:\n",
    "  No known miscitation found\n\n",
    "Verdict if genuinely ambiguous:\n",
    "  Uncertain\n\n",
    "Output nothing else."
  )

  # Wraps metacheck::llm() and intercepts the "__CANCELLED__" sentinel that
  # allows the caller's cancel_fn to abort the loop cooperatively.
  call_llm_once <- function(text, query, max_tokens) {
    res <- tryCatch(
      metacheck::llm(
        text        = text,
        query       = query,
        model       = model,
        maxTokens   = max_tokens,
        temperature = 0.1,
        API_KEY     = api_key
      ),
      error = function(e) {
        if (conditionMessage(e) == "__CANCELLED__") stop("__CANCELLED__")
        stop(conditionMessage(e))
      }
    )
    answer <- res$answer[1]
    if (isTRUE(res$error[1])) stop(res$error_msg[1])
    if (is.na(answer) || !nzchar(trimws(answer))) stop("Empty response from model")
    trimws(answer)
  }

  texts <- vapply(seq_along(meta), function(j) {
    m <- meta[[j]]

    known_mistake <- paste0(
      "KNOWN MISCITATION\n",
      if (nzchar(m$db_codes)) paste0("Error code: ", m$db_codes,
        if (nzchar(m$db_titles)) paste0(" (", m$db_titles, ")") else "", "\n") else "",
      if (nzchar(m$db_quote)) paste0("Prototypical wrong claim: ", m$db_quote, "\n") else "",
      if (nzchar(m$why))      paste0("Why it is wrong: ", m$why, "\n") else ""
    )

    cited_paper <- paste0(
      if (nzchar(m$ref_title))   paste0("Cited paper title: ", m$ref_title, "\n") else "",
      if (nzchar(m$ref_authors)) paste0("Cited paper authors: ", m$ref_authors, "\n") else "",
      if (nzchar(m$ref_year))    paste0("Cited paper year: ", m$ref_year, "\n") else ""
    )
    citation_block <- paste0(
      if (nzchar(m$section))  paste0("Section: ", m$section, "\n") else "",
      if (nzchar(cited_paper)) paste0(cited_paper) else "",
      if (nzchar(m$sentence) && m$sentence != m$citation)
        paste0("Citation sentence: ", m$sentence, "\n\nSurrounding context:\n", m$citation)
      else
        m$citation
    )

    paste0(known_mistake, "\nNEW CITATION TO CHECK\n", citation_block)
  }, character(1))

  raw_answers     <- character(length(texts))
  cancelled_early <- FALSE

  for (ci in seq_along(texts)) {
    result_i <- tryCatch(
      call_llm_once(texts[[ci]], system_prompt, 500L),
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

    if (!is.null(status_fn)) {
      status_fn(sprintf("LLM checked %d/%d row(s)...", ci, length(texts)))
    }

    if (cancelled_early) break
    if (!is.null(cancel_fn) && isTRUE(cancel_fn())) {
      cancelled_early <- TRUE
      break
    }
  }

  completed  <- if (cancelled_early) ci - 1L else length(raw_answers)
  debug_info <- paste0("model=", model, " | n=", completed, "/", length(raw_answers),
                       " | raw: ", paste(raw_answers[seq_len(completed)], collapse = " | "))

  # Strips the chain-of-thought preamble and returns only the VERDICT line.
  # Falls back to the full raw response if no "VERDICT:" label is found.
  extract_verdict <- function(raw) {
    if (!nzchar(trimws(raw))) return(raw)
    lines <- trimws(unlist(strsplit(raw, "\n")))
    verdict_lines <- grep("^VERDICT:\\s*", lines, ignore.case = TRUE, value = TRUE)
    if (length(verdict_lines) > 0)
      return(trimws(sub("^VERDICT:\\s*", "", verdict_lines[1])))
    raw
  }

  done_idx <- flagged_idx[seq_len(completed)]
  df$llm_verdict[done_idx] <- vapply(raw_answers[seq_len(completed)], extract_verdict, character(1))

  list(
    df          = df,
    raw_answers = raw_answers[seq_len(completed)],
    flagged_idx = done_idx,
    debug_info  = debug_info,
    texts       = texts,
    cancelled   = cancelled_early
  )
}
