# CiteGuard

Two R Shiny apps for detecting and reporting miscitations in scientific literature.

## Apps

### Reporter (`app/reporter`)

Lets authors and readers submit a miscitation report against a published paper.

- Look up the cited work by DOI (via CrossRef) and verify your identity via ORCID
- Describe what was miscited and select one or more miscitation types from the taxonomy below
- Reports are stored locally as a CSV database

### Analyzer (`app/analyzer`)

Extracts citations from PDF papers and checks them against the miscitation database.

- Upload one or more PDFs or ZIP files
- Extracts references using a local [GROBID](https://github.com/kermitt2/grobid) instance
- Filters citations by string or author name, or shows only papers flagged in the database
- Optionally runs an LLM check on flagged rows via the [Groq](https://groq.com) API

## Miscitation taxonomy

| Code | Type |
|------|------|
| M1 | Direct contradiction of finding |
| M2 | Non-existent finding |
| M3 | Non-significant finding |
| M4 | Overgeneralization of population |
| M5 | Misrepresentation of experimental conditions |
| M6 | Causal relation distorted or confused |
| M7 | Independent variable distortion |
| M8 | Dependent variable distortion |
| M9 | Measure-to-construct inflation |
| M10 | Non-contextualized citation |

## Requirements

- R with the following packages: `shiny`, `shinyjs`, `dplyr`, `purrr`, `stringr`, `DT`, `tibble`, `httr2`, `metacheck`, `digest`, `htmltools`
- Analyzer only: a running GROBID instance (default: `http://localhost:8070`) and optionally a Groq API key for LLM checks

## Running

```r
shiny::runApp("app/reporter")   # report form
shiny::runApp("app/analyzer")   # PDF analyzer
```

## Programmatic API

The analyzer pipeline is available as a standalone R function — no Shiny required. It automatically loads the latest community database from GitHub on every call.

### Setup

Install dependencies and source the shared modules:

```r
install.packages(c("dplyr", "purrr", "stringr", "tibble", "httr2",
                   "metacheck", "digest", "htmltools", "stringdist"))

# Source all modules (adjust path to wherever you cloned the repo)
for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)
```

You also need a running [GROBID](https://github.com/kermitt2/grobid) instance. The quickest way is via Docker. Two image variants are available:

```bash
# CRF-only — lighter, CPU only, sufficient for this use case
docker run --rm --init --ulimit core=0 -p 8070:8070 grobid/grobid:0.9.0-crf

# Full — deep learning models, requires GPU
docker run --rm --gpus all --init --ulimit core=0 -p 8070:8070 grobid/grobid:0.9.0-full
```

### Example

```r
# Load all CiteGuard modules (run from the project root)
for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Configuration — set your folder and GROBID URL here
GROBID_URL   <- "http://localhost:8070"
PDF_FOLDER   <- "/path/to/pdf/folder"
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
```

The LLM check requires a free [Groq](https://groq.com) API key. Set `GROQ_API_KEY` in your `.Renviron` file, or omit `run_llm` and `llm_api_key` to skip it.

### Key function reference

| Function | What it does |
|---|---|
| `analyze_citations(pdf_path, ...)` | Full pipeline: extract → annotate → (optionally) LLM |
| `load_github_known_df()` | Fetch the latest miscitation database from GitHub |
| `extract_citation_contexts(pdf_path, pdf_name, grobid_url)` | Stage 1: PDF → tibble of citation occurrences |
| `annotate_with_db_matches(citation_df, known_df)` | Stage 2: fuzzy-match rows against the database |
| `run_llm_check(annotated_df, model, api_key)` | Stage 3: LLM verdict on flagged rows |
