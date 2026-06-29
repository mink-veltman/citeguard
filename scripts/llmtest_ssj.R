# Test the LLM miscitation check against the SSJ citing-papers dataset.
#
# Ground-truth labels (VERDICT column):
#   CORRECT / CORRECT GENERIC / CORRECT ASSUMED / ASSUMED CORRECT -> not a miscitation
#   INCORRECT -> miscitation to flag
#
# Run from the MTPRstudio project root, with GROQ_API_KEY set in .Renviron.

# ── Dependencies ──────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
})

source("app/R/utils.R")
source("app/R/taxonomy.R")
source("app/R/db.R")
source("app/R/llm.R")

# ── Config ────────────────────────────────────────────────────────────────────
TEST_CSV  <- "data/Citations of Sample Size Justification paper(data).csv"
LLM_MODEL <- "llama-3.3-70b-versatile"
OUT_CSV   <- paste0("llm_ssj_test_", format(Sys.Date(), "%Y%m%d"), ".csv")

api_key <- Sys.getenv("GROQ_API_KEY")
if (!nzchar(api_key)) stop("Set GROQ_API_KEY in your .Renviron and restart R.")

# ── Load test data ────────────────────────────────────────────────────────────
raw <- read.csv(TEST_CSV, fileEncoding = "cp1252", stringsAsFactors = FALSE,
                check.names = FALSE, na.strings = c("", "NA"))

raw <- raw |>
  mutate(
    QUOTE   = trimws(coalesce(as.character(QUOTE), "")),
    VERDICT = trimws(coalesce(as.character(VERDICT), "")),
    DOI     = trimws(coalesce(as.character(DOI), ""))
  ) |>
  filter(nzchar(QUOTE))

cat(sprintf("Test rows with quotes: %d\n", nrow(raw)))
cat(sprintf("Rows with ground-truth verdicts: %d\n", sum(nzchar(raw$VERDICT))))

# ── Aggregate SSJ database info ───────────────────────────────────────────────
db     <- read_reports_db()
ssj_db <- db |>
  filter(
    grepl("lakens", tolower(target_lead_author), fixed = FALSE) &
    trimws(target_publication_year) == "2022"
  )

cat(sprintf("SSJ entries in database: %d\n\n", nrow(ssj_db)))

dedup_concat <- function(values, sep = " | ") {
  parts <- trimws(unlist(strsplit(paste(values, collapse = " || "), "\\s*\\|\\|\\s*")))
  parts <- unique(parts[nzchar(parts) & parts != "NA"])
  paste(parts, collapse = sep)
}

ssj_why    <- dedup_concat(ssj_db$why_incorrect)
ssj_quotes <- dedup_concat(ssj_db$quoted_or_paraphrased_text)
ssj_codes  <- paste(unique(trimws(unlist(strsplit(
  paste(ssj_db$mistake_codes,  collapse = "; "), ";\\s*")))), collapse = "; ")
ssj_titles <- paste(unique(trimws(unlist(strsplit(
  paste(ssj_db$mistake_titles, collapse = "; "), ";\\s*")))), collapse = "; ")

# ── Build df in run_llm_check format ─────────────────────────────────────────
llm_df <- tibble(
  file                    = raw$DOI,
  citation                = raw$QUOTE,
  sentence                = raw$QUOTE,
  expanded_text           = raw$QUOTE,
  ref_title               = "Sample size justification",
  ref_authors             = "Lakens",
  ref_year                = "2022",
  section                 = NA_character_,
  db_known_miscited_paper = "Yes",
  db_mistake_codes        = ssj_codes,
  db_mistake_titles       = ssj_titles,
  db_why_incorrect        = ssj_why,
  db_quoted_text          = ssj_quotes,
  db_match_reason         = NA_character_,
  db_report_ids           = NA_character_,
  ground_truth            = raw$VERDICT,
  gpt_verdict             = trimws(coalesce(as.character(raw[["GPT 5.2 VERDICT"]]), ""))
)

# ── Run LLM check ─────────────────────────────────────────────────────────────
cat(sprintf("Running LLM check on %d rows (model: %s)...\n\n", nrow(llm_df), LLM_MODEL))

result <- run_llm_check(
  llm_df,
  model     = LLM_MODEL,
  api_key   = api_key,
  status_fn = function(msg) cat(msg, "\n")
)

# ── Evaluate ──────────────────────────────────────────────────────────────────
classify_gt <- function(v) {
  v <- toupper(trimws(v))
  if (!nzchar(v))                     return(NA_character_)
  if (grepl("^INCORRECT$", v))        return("INCORRECT")
  if (grepl("^CORRECT", v))           return("CORRECT")
  NA_character_
}

classify_llm <- function(v) {
  v <- trimws(v)
  if (is.na(v) || !nzchar(v))                                                  return(NA_character_)
  if (grepl("^no known miscitation found$", v, ignore.case = TRUE))             return("CORRECT")
  if (grepl("^uncertain", v, ignore.case = TRUE))                               return("UNCERTAIN")
  "INCORRECT"
}

out <- result$df |>
  mutate(
    gt_class  = vapply(ground_truth, classify_gt,  character(1)),
    llm_class = vapply(llm_verdict,  classify_llm, character(1)),
    match = case_when(
      is.na(gt_class)                                  ~ NA_character_,
      gt_class == "CORRECT"   & llm_class == "CORRECT"   ~ "TN",
      gt_class == "INCORRECT" & llm_class == "INCORRECT" ~ "TP",
      gt_class == "CORRECT"   & llm_class != "CORRECT"   ~ "FP",
      gt_class == "INCORRECT" & llm_class == "CORRECT"   ~ "FN",
      TRUE                                               ~ NA_character_
    )
  )

with_gt <- out |> filter(!is.na(match))

tp <- sum(with_gt$match == "TP")
tn <- sum(with_gt$match == "TN")
fp <- sum(with_gt$match == "FP")
fn <- sum(with_gt$match == "FN")

cat("\n=== Results ===\n")
cat(sprintf("Rows evaluated: %d\n\n", nrow(with_gt)))
cat(sprintf("True positives  (INCORRECT, flagged):     %d\n", tp))
cat(sprintf("True negatives  (CORRECT,  not flagged):  %d\n", tn))
cat(sprintf("False positives (CORRECT,  flagged):      %d\n", fp))
cat(sprintf("False negatives (INCORRECT, not flagged): %d\n", fn))
cat("\n")
if (tp + fp > 0) cat(sprintf("Precision: %.0f%% (%d / %d)\n", 100*tp/(tp+fp), tp, tp+fp))
if (tp + fn > 0) cat(sprintf("Recall:    %.0f%% (%d / %d)\n", 100*tp/(tp+fn), tp, tp+fn))
if (tp+tn+fp+fn > 0) cat(sprintf("Accuracy:  %.0f%% (%d / %d)\n",
                                   100*(tp+tn)/(tp+tn+fp+fn), tp+tn, tp+tn+fp+fn))

# Show misclassified rows for inspection
cat("\n--- False Positives (CORRECT but flagged) ---\n")
fp_rows <- with_gt |> filter(match == "FP") |> select(file, citation, llm_verdict)
if (nrow(fp_rows) == 0) cat("  (none)\n") else {
  for (i in seq_len(nrow(fp_rows))) {
    cat(sprintf("  [%d] %s\n      Quote: %s\n      LLM:   %s\n\n",
                i,
                substr(fp_rows$file[i], 1, 60),
                substr(fp_rows$citation[i], 1, 120),
                substr(fp_rows$llm_verdict[i], 1, 120)))
  }
}

cat("\n--- False Negatives (INCORRECT but missed) ---\n")
fn_rows <- with_gt |> filter(match == "FN") |> select(file, citation, llm_verdict)
if (nrow(fn_rows) == 0) cat("  (none)\n") else {
  for (i in seq_len(nrow(fn_rows))) {
    cat(sprintf("  [%d] %s\n      Quote: %s\n      LLM:   %s\n\n",
                i,
                substr(fn_rows$file[i], 1, 60),
                substr(fn_rows$citation[i], 1, 120),
                substr(fn_rows$llm_verdict[i], 1, 120)))
  }
}

# ── Save ──────────────────────────────────────────────────────────────────────
out_save <- out |> select(file, citation, ground_truth, gt_class, llm_verdict, llm_class, match, gpt_verdict)
write.csv(out_save, OUT_CSV, row.names = FALSE)
cat(sprintf("\nFull results saved to: %s\n", OUT_CSV))
