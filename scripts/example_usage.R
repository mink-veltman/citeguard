# Load all CiteGuard modules (run from the project root)
for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Configuration — set your folder and GROBID URL here
GROBID_URL   <- "https://kermitt2-grobid.hf.space"  # or your own instance
PDF_FOLDER   <- "~/Desktop/mailhos"
GROQ_API_KEY <- Sys.getenv("GROQ_API_KEY")  # set in .Renviron or paste key directly

# Load the latest community miscitation database from GitHub
kdf <- load_github_known_df()

# Find all PDFs in the folder
pdf_paths <- list.files(PDF_FOLDER, pattern = "\\.pdf$",
                        full.names = TRUE, ignore.case = TRUE, recursive = TRUE)

# Run the full pipeline: extract citations, check against database, verify with LLM
results <- dplyr::bind_rows(lapply(pdf_paths, analyze_citations,
  grobid_url  = GROBID_URL,
  known_df    = kdf,
  run_llm     = TRUE,
  llm_api_key = GROQ_API_KEY
))

# Keep only citations matched against a known miscitation in the database
flagged <- results[results$db_known_miscited_paper == "Yes", ]

# Print each flagged citation to the console
for (i in seq_len(nrow(flagged))) {
  row <- flagged[i, ]
  cat("────────────────────────────────────────\n")
  cat("File:       ", row$file, "\n")
  cat("Citation:   ", row$citation, "\n")
  cat("Mistake:    ", row$db_mistake_titles, "\n")
  cat("LLM verdict:", row$llm_verdict, "\n")
}
cat("────────────────────────────────────────\n")
cat(nrow(flagged), "flagged citation(s) across", dplyr::n_distinct(flagged$file), "PDF(s)\n")

# Save flagged results to CSV
write.csv(flagged, "citeguard_results.csv", row.names = FALSE)
