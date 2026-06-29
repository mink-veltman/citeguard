library(dplyr)
library(readr)
library(irr)

# ── Load data ──────────────────────────────────────────────────────────────────
blind  <- read_csv("data/BLIND COPY(data).csv",                                   show_col_types = FALSE)
orig   <- read_csv("data/Citations of Sample Size Justification paper(data).csv",  show_col_types = FALSE)

# ── Slice from data row 24 onwards ────────────────────────────────────────────
# Excel row = R data-row + 1  (Excel row 1 is the header)
START_ROW <- 24

blind_sub <- slice(blind, START_ROW:n())
orig_sub  <- slice(orig,  START_ROW:n())

# ── Rows Mink was not confident in — excluded from IRR calculation ─────────────
# These are flagged transparently in the output but not used in kappa/agreement.
EXCLUDED_EXCEL_ROWS <- c(52L, 62L)
excluded_reasons <- c(
  "52" = "Mink: not confident in rating",
  "62" = "Mink: did not understand what the authors did"
)

# ── Detect uncertain/uninterpretable verdicts ─────────────────────────────────
is_uncertain <- function(x) {
  grepl("DON.T UNDERSTAND WHAT", toupper(as.character(x)))
}

# ── Normalize verdicts to canonical categories ────────────────────────────────
# Prefix matching so typos ("CORRRECT") and qualifiers ("CORRECT GENERIC")
# don't create false discrepancies.
# "I don't understand why this is related" → INCORRECT (a substantive verdict)
# "I don't understand what they did"       → flagged for exclusion (see above)
normalize_verdict <- function(x) {
  x <- trimws(toupper(as.character(x)))
  dplyr::case_when(
    grepl("^INC",                   x) ~ "INCORRECT",
    grepl("DON.T UNDERSTAND WHY",   x) ~ "INCORRECT",
    grepl("NOT MENTION|IS MENTION",  x) ~ "INCORRECT",
    grepl("^ASS",                   x) ~ "ASSUMED CORRECT",
    grepl("^MIS",                   x) ~ "MISATTRIBUTED",
    grepl("^PAR",                   x) ~ "PARTIALLY CORRECT",
    grepl("^COR",                   x) ~ "CORRECT",
    nzchar(x)                          ~ x,
    TRUE                               ~ NA_character_
  )
}

mink_raw     <- trimws(toupper(blind_sub[["Mink's verdict"]]))
original_raw <- trimws(toupper(orig_sub[["VERDICT"]]))

mink     <- normalize_verdict(mink_raw)
original <- normalize_verdict(original_raw)

# ── Build full comparison table ────────────────────────────────────────────────
comparison <- tibble(
  excel_row    = (START_ROW:(START_ROW + nrow(blind_sub) - 1)) + 1L,
  doi          = blind_sub$DOI,
  authors      = blind_sub$AUTHORS,
  mink_raw     = mink_raw,
  orig_raw     = original_raw,
  mink_verdict = mink,
  orig_verdict = original,
  uncertain    = excel_row %in% EXCLUDED_EXCEL_ROWS | is_uncertain(mink_raw),
  match        = mink == original
)

# ── Hardcoded overrides ───────────────────────────────────────────────────────
comparison$mink_verdict[comparison$excel_row == 81L] <- "INCORRECT"

# ── Separate out excluded rows ─────────────────────────────────────────────────
excluded   <- filter(comparison, uncertain)
analyzable <- filter(comparison, !uncertain,
                     !is.na(mink_verdict), nzchar(mink_verdict),
                     !is.na(orig_verdict), nzchar(orig_verdict))

# ── Cohen's Kappa ──────────────────────────────────────────────────────────────
kappa_result <- kappa2(cbind(analyzable$mink_verdict, analyzable$orig_verdict),
                       weight = "unweighted")

k   <- kappa_result$value
se  <- kappa_result$se
z   <- kappa_result$statistic
p   <- kappa_result$p.value
pct <- 100 * mean(analyzable$match)
n   <- nrow(analyzable)

interpret_kappa <- function(k) {
  dplyr::case_when(
    k < 0    ~ "poor (less than chance)",
    k < 0.20 ~ "slight",
    k < 0.40 ~ "fair",
    k < 0.60 ~ "moderate",
    k < 0.80 ~ "substantial",
    TRUE     ~ "almost perfect"
  )
}

# ── Output ─────────────────────────────────────────────────────────────────────
sep  <- strrep("─", 58)
sep2 <- strrep("·", 44)

cat("\n", sep, "\n", sep = "")
cat("  Inter-Rater Reliability\n")
cat("  Rater 1: Mink  (non-expert)\n")
cat("  Rater 2: Daniel  (expert, original author)\n")
cat(sep, "\n\n", sep = "")

# Excluded rows notice
cat(sprintf("  NOTE: %d row(s) excluded from analysis — Mink (non-expert)\n", nrow(excluded)))
cat("  indicated insufficient confidence to provide a reliable rating:\n\n")
for (i in seq_len(nrow(excluded))) {
  r      <- excluded[i, ]
  reason <- excluded_reasons[as.character(r$excel_row)]
  if (is.na(reason)) reason <- "non-expert: not confident in rating"
  cat(sprintf("    Excel row %d | %s\n",         r$excel_row, r$authors))
  cat(sprintf("    Reason              : %s\n",  reason))
  cat(sprintf("    Non-expert's note   : %s\n\n", r$mink_raw))
}

cat(sep, "\n\n", sep = "")
cat("  Agreement statistics\n")
cat(sprintf("  (based on %d rated pairs after excluding %d uncertain row(s))\n\n",
            n, nrow(excluded)))

cat(sprintf("  Exact agreement  : %d / %d  (%.1f%%)\n", sum(analyzable$match), n, pct))
cat(sprintf("  Cohen's kappa    : %.3f  — %s\n",        k, interpret_kappa(k)))
cat(sprintf("  95%% CI          : [%.3f, %.3f]\n",      k - 1.96 * se, k + 1.96 * se))
cat(sprintf("  z = %.2f,  p %s\n",
            z, ifelse(p < .001, "< .001", sprintf("= %.3f", p))))

cat("\n", sep, "\n\n", sep = "")
cat("  Verdict breakdown\n\n")

cats <- sort(unique(c(analyzable$mink_verdict, analyzable$orig_verdict)))
cat(sprintf("  %-22s  %10s  %8s\n", "Category", "Non-expert", "Expert"))
cat(sprintf("  %s\n", sep2))
for (cat_i in cats) {
  cat(sprintf("  %-22s  %10d  %8d\n",
              cat_i,
              sum(analyzable$mink_verdict == cat_i, na.rm = TRUE),
              sum(analyzable$orig_verdict == cat_i, na.rm = TRUE)))
}

cat("\n", sep, "\n\n", sep = "")
cat("  Confusion matrix  (rows = Non-expert, cols = Expert)\n\n")
cm <- table(`Non-expert (Mink)` = analyzable$mink_verdict,
            `Expert (Daniel)`   = analyzable$orig_verdict)
print(cm)

# ── Discrepancies ──────────────────────────────────────────────────────────────
discrepancies <- filter(analyzable, !match)

cat(sprintf("\n%s\n", sep))
cat(sprintf("  Discrepancies: %d / %d rated pairs\n\n", nrow(discrepancies), n))

if (nrow(discrepancies) > 0) {
  for (i in seq_len(nrow(discrepancies))) {
    r <- discrepancies[i, ]
    cat(sprintf("  Excel row %-4d | %s\n",               r$excel_row, r$authors))
    cat(sprintf("  DOI                  : %s\n",          r$doi))
    cat(sprintf("  Non-expert (Mink)    : %-22s [%s]\n", r$mink_verdict, r$mink_raw))
    cat(sprintf("  Expert (Daniel)      : %-22s [%s]\n\n", r$orig_verdict, r$orig_raw))
  }
} else {
  cat("  No discrepancies found.\n")
}

cat(sep, "\n\n", sep = "")

# ── Export ─────────────────────────────────────────────────────────────────────
write_csv(discrepancies, "data/discrepancies.csv")
write_csv(excluded,      "data/excluded_rows.csv")
cat("  discrepancies.csv  — rows where Mink and Daniel disagreed\n")
cat("  excluded_rows.csv  — rows omitted due to Mink's uncertainty\n\n")
