#' Analyze citations in a PDF for potential miscitations
#'
#' A single-call wrapper that runs the full citation-analysis pipeline:
#' PDF-to-GROBID extraction, fuzzy database annotation, and (optionally)
#' LLM-based miscitation verification. Returns a tidy tibble suitable for
#' downstream filtering, export, or integration into a larger workflow.
#'
#' For batch processing across multiple PDFs, iterate this function and bind
#' results with \code{dplyr::bind_rows()}.
#'
#' @param pdf_path Character. Absolute path to the local PDF file.
#' @param grobid_url Character. Base URL of a running GROBID instance.
#'   Default \code{"http://localhost:8070"}.
#' @param pdf_name Character. Display name used as the \code{file} column in
#'   the returned tibble. Defaults to \code{basename(pdf_path)}.
#' @param known_df A tibble of known miscitations as returned by
#'   \code{summarise_known_miscitations()}, or \code{NULL} (default). When
#'   \code{NULL}, the \code{db_*} annotation columns are still present in the
#'   output with all-default values (no matches flagged).
#' @param run_llm Logical. Whether to run LLM verification on rows flagged by
#'   database matching. Default \code{FALSE}. Requires a valid
#'   \code{llm_api_key}.
#' @param llm_model Character. Groq model identifier used when
#'   \code{run_llm = TRUE}. Default \code{"llama-3.3-70b-versatile"}.
#' @param llm_api_key Character. Groq API key. Defaults to
#'   \code{Sys.getenv("GROQ_API_KEY")}. If \code{run_llm = TRUE} and the key
#'   is empty, a warning is issued and the function returns without LLM results.
#' @param window Integer. Number of surrounding sentences included in
#'   \code{expanded_text} for each citation. Default \code{10L}.
#' @param status_fn Optional function of one character argument. Passed to
#'   \code{\link{run_llm_check}} for progress reporting during LLM calls.
#'   Ignored when \code{run_llm = FALSE}.
#'
#' @return A tibble with one row per citation occurrence. Base columns come
#'   from \code{\link{extract_citation_contexts}}; \code{db_*} annotation
#'   columns are added by \code{\link{annotate_with_db_matches}}; an
#'   \code{llm_verdict} column is added when \code{run_llm = TRUE}.
#'
#' @examples
#' \dontrun{
#' # Basic extraction only
#' ctx <- analyze_citations(
#'   pdf_path   = "/path/to/paper.pdf",
#'   grobid_url = "http://localhost:8070"
#' )
#'
#' # With database annotation
#' db  <- read.csv("miscitation_reports.csv")
#' kdf <- summarise_known_miscitations(db)
#' ctx <- analyze_citations(
#'   pdf_path   = "/path/to/paper.pdf",
#'   grobid_url = "http://localhost:8070",
#'   known_df   = kdf
#' )
#'
#' # With LLM verification
#' ctx <- analyze_citations(
#'   pdf_path    = "/path/to/paper.pdf",
#'   grobid_url  = "http://localhost:8070",
#'   known_df    = kdf,
#'   run_llm     = TRUE,
#'   llm_api_key = Sys.getenv("GROQ_API_KEY")
#' )
#' ctx[ctx$db_known_miscited_paper == "Yes", c("citation", "llm_verdict")]
#'
#' # Batch processing across multiple PDFs
#' results <- dplyr::bind_rows(lapply(pdf_paths, analyze_citations,
#'   grobid_url = "http://localhost:8070", known_df = kdf))
#' }
#'
#' @seealso \code{\link{extract_citation_contexts}},
#'   \code{\link{annotate_with_db_matches}}, \code{\link{run_llm_check}}
#' @export
analyze_citations <- function(
  pdf_path,
  grobid_url  = "http://localhost:8070",
  pdf_name    = basename(pdf_path),
  known_df    = NULL,
  run_llm     = FALSE,
  llm_model   = "llama-3.3-70b-versatile",
  llm_api_key = Sys.getenv("GROQ_API_KEY"),
  window      = 10L,
  status_fn   = NULL
) {
  # Stage 1: extract all citation occurrences from the PDF via GROBID.
  ctx <- extract_citation_contexts(pdf_path, pdf_name, grobid_url, window = window)

  # Stage 2: fuzzy-match against the known-miscitations database.
  # Always called so the output schema is consistent regardless of known_df.
  annotated <- annotate_with_db_matches(ctx, known_df)

  if (!run_llm) return(annotated)

  # Stage 3: LLM verification on flagged rows.
  if (!nzchar(trimws(llm_api_key))) {
    warning("run_llm = TRUE but llm_api_key is empty. Returning results without LLM verdicts.")
    return(annotated)
  }

  llm_result <- run_llm_check(
    annotated,
    model     = llm_model,
    api_key   = llm_api_key,
    status_fn = status_fn
  )

  # Debug metadata (raw_answers, texts, debug_info) is intentionally not
  # returned here. Callers needing that detail should call run_llm_check() directly.
  llm_result$df
}
