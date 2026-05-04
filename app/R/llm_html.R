# Shiny-only HTML rendering helpers. Not exported by the package.

#' @noRd
confidence_badge <- function(conf) {
  conf <- tolower(trimws(conf))
  switch(conf,
    high   = "<span style='background:#5cb85c;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>high</span>",
    medium = "<span style='background:#f0ad4e;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>medium</span>",
    low    = "<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>low</span>",
    paste0("<span style='background:#aaa;color:#fff;border-radius:3px;padding:1px 5px;font-size:0.8em;margin-right:4px'>",
           htmltools::htmlEscape(conf), "</span>")
  )
}

#' @noRd
format_llm_verdict_html <- function(verdict) {
  if (is.na(verdict) || !nzchar(trimws(verdict))) return("")

  v <- trimws(verdict)

  if (grepl("^no(\\.?|\\s+known\\s+miscitation\\s+found\\.?)$", v, ignore.case = TRUE))
    return("<span style='color:#5cb85c;font-weight:bold'>No known miscitation found</span>")
  if (grepl("^uncertain\\.?", v, ignore.case = TRUE))
    return("<span style='color:#f0ad4e;font-weight:bold'>Uncertain</span>")

  lines  <- trimws(unlist(strsplit(v, "\n")))
  lines  <- lines[nzchar(lines)]
  pat    <- "^(M\\d+|Other)\\s*\\((high|medium|low)\\):\\s*(.+)$"
  parsed <- lapply(lines, function(line) {
    m <- regmatches(line, regexec(pat, line, ignore.case = TRUE))[[1]]
    if (length(m) == 4) list(code = m[2], conf = m[3], reason = m[4])
    else NULL
  })
  parsed <- Filter(Negate(is.null), parsed)

  if (length(parsed) > 0) {
    parts <- vapply(parsed, function(p) {
      code <- toupper(p$code)
      row  <- mistake_table[mistake_table$mistake_code == code, ]
      code_badge <- paste0(
        "<span style='background:#d9534f;color:#fff;border-radius:3px;",
        "padding:1px 5px;font-size:0.85em;font-weight:bold;margin-right:4px'>",
        htmltools::htmlEscape(code), "</span>"
      )
      title_span <- if (nrow(row) > 0)
        paste0("<span style='color:#333;font-weight:bold'>", htmltools::htmlEscape(row$mistake_title), "</span>")
      else ""
      paste0(
        code_badge, confidence_badge(p$conf), title_span,
        "<br><span style='color:#555;font-size:0.9em'>", htmltools::htmlEscape(p$reason), "</span>"
      )
    }, character(1))
    paste(parts, collapse = "<br><br>")
  } else {
    paste0("<span style='color:#555'><em>", htmltools::htmlEscape(v), "</em></span>")
  }
}
