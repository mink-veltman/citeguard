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
# CRF-only — lighter, CPU only, sufficient for reference extraction
docker run --rm --init --ulimit core=0 -p 8070:8070 grobid/grobid:0.9.0-crf

# Full — deep learning models, requires GPU
docker run --rm --gpus all --init --ulimit core=0 -p 8070:8070 grobid/grobid:0.9.0-full
```

### Analyzing a single PDF

```r
ctx <- analyze_citations(
  pdf_path   = "/path/to/paper.pdf",
  grobid_url = "http://localhost:8070"
)

# See rows matched against the miscitation database
ctx[ctx$db_known_miscited_paper == "Yes",
    c("citation", "db_mistake_titles", "db_why_incorrect")]
```

### Batch processing

Fetch the database once to avoid a network request per PDF:

```r
kdf <- load_github_known_df()

pdf_paths <- list.files("/path/to/papers", pattern = "\\.pdf$", full.names = TRUE)

results <- dplyr::bind_rows(lapply(pdf_paths, analyze_citations,
  grobid_url = "http://localhost:8070",
  known_df   = kdf
))

write.csv(results, "citation_check_results.csv", row.names = FALSE)
```

### With LLM verification

Rows flagged by the database can be passed to an LLM for a second-opinion verdict. Requires a free [Groq](https://groq.com) API key:

```r
ctx <- analyze_citations(
  pdf_path    = "/path/to/paper.pdf",
  grobid_url  = "http://localhost:8070",
  run_llm     = TRUE,
  llm_api_key = Sys.getenv("GROQ_API_KEY")   # or paste key directly
)

ctx[ctx$db_known_miscited_paper == "Yes", c("citation", "llm_verdict")]
```

### Key function reference

| Function | What it does |
|---|---|
| `analyze_citations(pdf_path, ...)` | Full pipeline: extract → annotate → (optionally) LLM |
| `load_github_known_df()` | Fetch the latest miscitation database from GitHub |
| `extract_citation_contexts(pdf_path, pdf_name, grobid_url)` | Stage 1: PDF → tibble of citation occurrences |
| `annotate_with_db_matches(citation_df, known_df)` | Stage 2: fuzzy-match rows against the database |
| `run_llm_check(annotated_df, model, api_key)` | Stage 3: LLM verdict on flagged rows |
