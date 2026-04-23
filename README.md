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
