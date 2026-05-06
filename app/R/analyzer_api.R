CITEGUARD_GH_RAW_URL <- "https://raw.githubusercontent.com/mink-veltman/citeguard-data/master/miscitation_reports.csv"

#' Load the CiteGuard known-miscitations database from GitHub
#'
#' Fetches the shared miscitation database from the CiteGuard GitHub repository
#' and returns it in the format expected by \code{\link{analyze_citations}} and
#' \code{\link{annotate_with_db_matches}}. Called automatically when
#' \code{known_df} is not supplied to \code{analyze_citations()}.
#'
#' @return A tibble as returned by \code{summarise_known_miscitations()}, or
#'   \code{NULL} if the download fails (a warning is issued in that case).
#' @export
load_github_known_df <- function() {
  tryCatch({
    df <- read.csv(CITEGUARD_GH_RAW_URL, stringsAsFactors = FALSE,
                   na.strings = c("", "NA"), check.names = FALSE)
    summarise_known_miscitations(df)
  }, error = function(e) {
    warning("Could not load CiteGuard database from GitHub: ", conditionMessage(e),
            ". Proceeding without database annotation.")
    NULL
  })
}

#' Analyze citations in a PDF for potential miscitations
#'
#' A single-call wrapper that runs the full citation-analysis pipeline:
#' PDF-to-GROBID extraction, fuzzy database annotation, and (optionally)
#' LLM-based miscitation verification. Returns a tidy tibble suitable for
#' downstream filtering, export, or integration into a larger workflow.
#'
#' By default \code{known_df} is fetched live from the CiteGuard GitHub
#' database, so each call reflects the latest community reports. Pass a
#' pre-fetched tibble (via \code{\link{load_github_known_df}}) when analyzing
#' many PDFs in a batch to avoid redundant network requests.
#'
#' For batch processing across multiple PDFs, fetch the database once and
#' iterate this function, binding results with \code{dplyr::bind_rows()}.
#'
#' @param pdf_path Character. Absolute path to the local PDF file.
#' @param grobid_url Character. Base URL of a running GROBID instance.
#'   Default \code{"http://localhost:8070"}.
#' @param pdf_name Character. Display name used as the \code{file} column in
#'   the returned tibble. Defaults to \code{basename(pdf_path)}.
#' @param known_df A tibble of known miscitations as returned by
#'   \code{\link{load_github_known_df}} or \code{summarise_known_miscitations()}.
#'   Defaults to a live fetch from the CiteGuard GitHub database. Pass
#'   \code{NULL} to skip database annotation entirely.
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
#' # Source all required files first (skip if running inside the Shiny app)
#' for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)
#'
#' # Single PDF — database loaded automatically from GitHub
#' ctx <- analyze_citations(
#'   pdf_path   = "/path/to/paper.pdf",
#'   grobid_url = "http://localhost:8070"
#' )
#' ctx[ctx$db_known_miscited_paper == "Yes", c("citation", "db_mistake_titles")]
#'
#' # Batch processing — fetch database once to avoid repeated network requests
#' kdf     <- load_github_known_df()
#' results <- dplyr::bind_rows(lapply(pdf_paths, analyze_citations,
#'   grobid_url = "http://localhost:8070", known_df = kdf))
#'
#' # With LLM verification on flagged rows
#' ctx <- analyze_citations(
#'   pdf_path    = "/path/to/paper.pdf",
#'   grobid_url  = "http://localhost:8070",
#'   run_llm     = TRUE,
#'   llm_api_key = Sys.getenv("GROQ_API_KEY")
#' )
#' ctx[ctx$db_known_miscited_paper == "Yes", c("citation", "llm_verdict")]
#'
#' # Skip database annotation entirely
#' ctx <- analyze_citations("/path/to/paper.pdf", known_df = NULL)
#' }
#'
#' @seealso \code{\link{load_github_known_df}},
#'   \code{\link{extract_citation_contexts}},
#'   \code{\link{annotate_with_db_matches}}, \code{\link{run_llm_check}}
#' @export
analyze_citations <- function(
  pdf_path,
  grobid_url  = "http://localhost:8070",
  pdf_name    = basename(pdf_path),
  known_df    = load_github_known_df(),
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
