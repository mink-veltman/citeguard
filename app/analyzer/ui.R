ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background: #f7f8fa; }
      .cg-header {
        background: #fff;
        border-bottom: 1px solid #e0e0e0;
        padding: 13px 24px;
        margin-bottom: 24px;
      }
      .cg-header .title   { font-weight: 700; font-size: 1.1em; }
      .cg-header .subtitle { color: #888; margin-left: 6px; }
      .upload-panel {
        background: #fff;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 18px 16px;
      }
      .upload-panel .btn-primary { width: 100%; }
      .upload-panel details > summary {
        color: #888;
        font-size: 0.83em;
        cursor: pointer;
        user-select: none;
        list-style: none;
      }
      .upload-panel details > summary::-webkit-details-marker { display: none; }
      .upload-panel details > summary::marker { display: none; }
      .process-msg {
        font-size: 0.88em;
        color: #555;
        margin-top: 10px;
        min-height: 1.4em;
      }
      .results-summary {
        font-size: 0.9em;
        color: #555;
        margin-bottom: 10px;
      }
      .empty-state {
        text-align: center;
        color: #aaa;
        padding: 80px 20px;
        font-size: 1em;
      }
      .cg-cards { padding: 0; }
      .cg-card {
        background: #fff;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 16px 18px;
        margin-bottom: 14px;
      }
      .cg-card-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        margin-bottom: 10px;
      }
      .cg-paper-title { font-weight: 700; font-size: 1em; color: #1a1a1a; margin-bottom: 2px; }
      .cg-paper-meta  { font-size: 0.82em; color: #777; }
      .cg-file { color: #bbb; font-size: 0.75em; white-space: nowrap; padding-top: 2px; }
      .cg-cite-label  { font-size: 0.75em; color: #aaa; text-transform: uppercase;
                        letter-spacing: 0.04em; margin-bottom: 3px; }
      .cg-quote {
        font-size: 0.88em;
        color: #444;
        margin-bottom: 10px;
        font-style: italic;
        line-height: 1.5;
      }
      .cg-warning-bar {
        background: #fff3cd;
        border: 1px solid #ffc107;
        border-radius: 4px;
        padding: 5px 9px;
        margin-bottom: 6px;
        font-weight: 600;
        color: #856404;
        font-size: 0.9em;
      }
      mark { background: #fff9c4; border-radius: 2px; font-style: normal; }
      details > summary { list-style: none; cursor: pointer; }
      details > summary::-webkit-details-marker { display: none; }
    "))
  ),

  div(class = "cg-header",
    tags$span(class = "title",    "citeguard"),
    tags$span(class = "subtitle", "Citation checker")
  ),

  fluidRow(
    column(width = 3,
      div(class = "upload-panel",
        fileInput("files",
                  tags$span(style = "font-weight:600", "Upload PDF(s) or ZIP"),
                  multiple    = TRUE,
                  accept      = c(".pdf", ".zip"),
                  buttonLabel = "Browse…",
                  placeholder = "No file selected"),
        actionButton("process", "Check citations", class = "btn-primary"),
        div(class = "process-msg", textOutput("process_msg")),
        hr(style = "margin: 14px 0"),
        tags$details(
          tags$summary("⚙ GROBID settings"),
          br(),
          textInput("grobid_url", NULL,
                    value       = "https://kermitt2-grobid.hf.space",
                    placeholder = "GROBID URL"),
          tags$p(style = "font-size:0.8em;color:#999;margin-top:-6px",
                 "Public instance — replace with a local one for better performance.")
        )
      )
    ),

    column(width = 9,
      div(id = "empty_state",
        div(class = "empty-state",
          tags$p(style = "font-size:1.4em;margin-bottom:8px", "⚠"),
          "Upload a PDF and click", tags$strong("Check citations"),
          "to detect known miscitations."
        )
      ),
      div(id = "results_area", style = "display:none",
        div(class = "results-summary", textOutput("results_summary")),
        uiOutput("results_cards")
      )
    )
  )
)
