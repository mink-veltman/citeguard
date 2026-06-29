#' CiteGuard Miscitation Check
#'
#' @description
#' Checks citations against the CiteGuard database of commonly miscited papers.
#' Flags citations to papers that have been reported as frequently misrepresented,
#' with optional LLM verification of the citation context.
#'
#' @details
#' This module fetches the live CiteGuard database from GitHub and fuzzy-matches
#' each reference against known miscitation reports. A citation is flagged when
#' it has a strong author match (exact author key or ≥2 fuzzy name overlaps) and
#' at least one corroborating signal (year within 1 or title token overlap), with
#' no contradicting signal on either side. Pass \code{run_llm = TRUE} to run an
#' additional LLM-based context check on flagged citations (requires a Groq API key).
#'
#' @keywords reference
#'
#' @author Mink Veltman
#'
#' @param paper a paper object or paperlist object
#' @param known_df the CiteGuard database as returned by
#'   \code{\link{load_github_known_df}}; fetched live from GitHub by default
#' @param run_llm logical; whether to run LLM verification on flagged rows.
#'   Default \code{FALSE}
#' @param llm_api_key Character. Groq API key. Defaults to
#'   \code{Sys.getenv("GROQ_API_KEY")}
#' @param window Integer. Context window passed to \code{contexts_from_paper}.
#'   Default \code{10L}
#'
#' @returns a list
#' @export
ref_citeguard <- function(
  paper,
  known_df    = load_github_known_df(),
  run_llm     = FALSE,
  llm_api_key = Sys.getenv("GROQ_API_KEY"),
  window      = 10L
) {
  pdf_name <- if (!is.null(paper$id) && length(paper$id) > 0) paper$id[[1]] else "paper"

  ctx      <- contexts_from_paper(paper, pdf_name, window)
  annotated <- annotate_with_db_matches(ctx, known_df)

  if (run_llm && nzchar(trimws(llm_api_key))) {
    result    <- run_llm_check(annotated, api_key = llm_api_key)
    annotated <- result$df
  }

  flagged <- annotated[!is.na(annotated$db_known_miscited_paper) &
                         annotated$db_known_miscited_paper == "Yes", ]

  n <- nrow(flagged)
  tl <- if (n > 0) "yellow" else "green"

  summary_text <- if (n == 0) {
    "No known miscitations found."
  } else {
    sprintf("Found %d citation%s to paper%s in the CiteGuard database.",
            n, ifelse(n == 1, "", "s"), ifelse(n == 1, "", "s"))
  }

  report <- if (n == 0) {
    summary_text
  } else {
    doi_groups <- split(seq_len(n), flagged$ref_doi %||% seq_len(n))
    blocks <- lapply(doi_groups, function(idx) {
      rows <- flagged[idx, , drop = FALSE]
      r    <- rows[1, , drop = FALSE]

      title   <- r$ref_title %||% "Unknown title"
      author  <- r$ref_lead_author %||% ""
      year    <- r$ref_year %||% ""
      header  <- sprintf("**%s** (%s%s)",
                         title,
                         author,
                         if (nzchar(year)) paste0(", ", year) else "")

      why <- if (!is.na(r$db_why_incorrect) && nzchar(r$db_why_incorrect))
        paste0("*Why it is a known miscitation:* ", r$db_why_incorrect)
      else
        ""

      correct <- if (!is.na(r$db_correct_citation) && nzchar(r$db_correct_citation))
        paste0("*How to cite correctly:* ", r$db_correct_citation)
      else
        ""

      codes <- if (!is.na(r$db_mistake_titles) && nzchar(r$db_mistake_titles))
        paste0("*Error type:* ", r$db_mistake_titles)
      else
        ""

      instances <- rows$sentence[!is.na(rows$sentence) & nzchar(rows$sentence)]
      instance_block <- if (length(instances) > 0)
        paste(paste(">", head(instances, 5L)), collapse = "\n\n")
      else
        ""

      paste(Filter(nzchar, c(header, why, correct, codes, instance_block)), collapse = "\n\n")
    })

    paste(c(summary_text, unlist(blocks)), collapse = "\n\n---\n\n")
  }

  summary_table <- dplyr::tibble(
    id           = pdf_name,
    miscitations = n
  )

  list(
    table         = annotated,
    summary_table = summary_table,
    na_replace    = 0L,
    traffic_light = tl,
    report        = report,
    summary_text  = summary_text
  )
}
