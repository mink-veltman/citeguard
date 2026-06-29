# Test script for the CiteGuard programmatic API
# Run from the project root: source("test_api.R")
# Requires a running GROBID instance on http://localhost:8070

GROBID_URL <- "http://localhost:8070"
TEST_PDF   <- "SSJcitings2026/Schlechter et al. - 2026 - Comparative Thinking and Clinical Social Anxiety Within- and Between-Person Effects in a Daily Ecol.pdf"

# ── Setup ──────────────────────────────────────────────────────────────────────

cat("Loading modules...\n")
for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)
cat("OK\n\n")

# ── 1. GROBID reachable? ───────────────────────────────────────────────────────

cat("1. Checking GROBID at", GROBID_URL, "...\n")
alive <- check_grobid_alive(GROBID_URL)
stopifnot("GROBID not reachable — is it running?" = isTRUE(alive$ok))
cat("   OK (HTTP", alive$status, ")\n\n")

# ── 2. Load GitHub database ────────────────────────────────────────────────────

cat("2. Loading known-miscitations database from GitHub...\n")
kdf <- load_github_known_df()
stopifnot("Database load returned NULL" = !is.null(kdf))
cat("   OK —", nrow(kdf), "known miscitation entries\n\n")

# ── 3. Single PDF — full pipeline ─────────────────────────────────────────────

cat("3. Running analyze_citations() on test PDF...\n")
cat("   File:", basename(TEST_PDF), "\n")
result <- analyze_citations(
  pdf_path   = TEST_PDF,
  grobid_url = GROBID_URL,
  known_df   = kdf
)
stopifnot("Result is not a data frame" = is.data.frame(result))
stopifnot("Result has no rows"         = nrow(result) > 0)
cat("   OK —", nrow(result), "citation rows extracted\n")
cat("   Flagged as known miscitation:", sum(result$db_known_miscited_paper == "Yes", na.rm = TRUE), "\n\n")

# ── 4. Filters work ───────────────────────────────────────────────────────────

cat("4. Checking output columns...\n")
expected_cols <- c("file", "citation", "sentence", "expanded_text",
                   "db_known_miscited_paper", "db_match_score")
missing <- setdiff(expected_cols, names(result))
stopifnot("Missing expected columns" = length(missing) == 0)
cat("   OK — all expected columns present\n\n")

# ── 5. Batch processing ────────────────────────────────────────────────────────

cat("5. Batch processing (first 3 PDFs in SSJcitings2026/)...\n")
pdf_paths <- head(list.files("SSJcitings2026", pattern = "\\.pdf$",
                              full.names = TRUE, ignore.case = TRUE), 3)
cat("   Files:\n")
for (p in pdf_paths) cat("    -", basename(p), "\n")

batch <- dplyr::bind_rows(lapply(pdf_paths, analyze_citations,
  grobid_url = GROBID_URL,
  known_df   = kdf
))
stopifnot("Batch result has no rows" = nrow(batch) > 0)
cat("   OK —", nrow(batch), "total rows across", length(pdf_paths), "PDFs\n\n")

# ── 6. known_df = NULL skips annotation ───────────────────────────────────────

cat("6. Checking known_df = NULL skips database annotation...\n")
no_db <- analyze_citations(TEST_PDF, grobid_url = GROBID_URL, known_df = NULL)
stopifnot("Rows differ when skipping DB" = nrow(no_db) == nrow(result))
stopifnot("db_known_miscited_paper should all be 'No'" =
            all(no_db$db_known_miscited_paper == "No", na.rm = TRUE))
cat("   OK\n\n")

# ── 7. LLM check (optional — uncomment and set API key to run) ────────────────

# cat("7. LLM check on flagged rows...\n")
# Sys.setenv(GROQ_API_KEY = "gsk_...")
# llm_result <- analyze_citations(
#   pdf_path    = TEST_PDF,
#   grobid_url  = GROBID_URL,
#   known_df    = kdf,
#   run_llm     = TRUE,
#   llm_api_key = Sys.getenv("GROQ_API_KEY")
# )
# flagged <- llm_result[!is.na(llm_result$llm_verdict), c("citation", "llm_verdict")]
# print(flagged)

# ── Summary ───────────────────────────────────────────────────────────────────

cat("All tests passed.\n")
