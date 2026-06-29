#' Citation-error taxonomy (M1–M10)
#'
#' The canonical set of miscitation error types used throughout the package.
#' Each row defines one error category.
#'
#' @format A data frame with 10 rows and 3 columns:
#' \describe{
#'   \item{mistake_code}{Character. Error code, e.g. \code{"M1"}.}
#'   \item{mistake_title}{Character. Short label for the error type.}
#'   \item{mistake_description}{Character. One-sentence explanation.}
#' }
#' @examples
#' mistake_table$mistake_code
#' mistake_table[mistake_table$mistake_code == "M1", ]
#' @export
mistake_table <- data.frame(
  mistake_code = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10"),
  mistake_title = c(
    "Direct contradiction of finding",
    "Non-existent finding",
    "Non-significant result",
    "Population overgeneralization",
    "Causal distortion",
    "Independent variable misrepresentation",
    "Dependent variable misrepresentation",
    "Unjustified construct inflation",
    "Incomplete citation",
    "Non-contextualized citation"
  ),
  mistake_description = c(
    "The cited claim directly contradicts or reverses the findings of the source.",
    "The described result or claim does not appear in the source.",
    "A statistically non-significant result is presented as meaningful.",
    "Findings from a specific group are attributed to a broader population.",
    "A causal relationship is reversed or made ambiguous relative to the source.",
    "The manipulation or independent variable is incorrectly described.",
    "The outcome variable or findings are distorted or incorrectly described.",
    "Cited findings are generalized to a broader construct without justification.",
    "A recommendation is cited without the required follow-through, including cases used to legitimize inadequate methodology.",
    "Citation is too vague or general to be traced to specific content in the source."
  ),
  stringsAsFactors = FALSE
)

#' Valid miscitation error codes
#'
#' A character vector of the 10 accepted mistake codes (\code{"M1"} through
#' \code{"M10"}). Use this for input validation rather than hard-coding the vector.
#'
#' @export
VALID_MISTAKE_CODES <- mistake_table$mistake_code

#' @noRd
# Named vector for Shiny selectInput widgets only; not part of the package API.
mistake_choices <- stats::setNames(
  mistake_table$mistake_code,
  paste0(mistake_table$mistake_code, " — ", mistake_table$mistake_title)
)

#' @noRd
# Pre-formatted newline-delimited block injected into the LLM system prompt.
TAXONOMY_BLOCK <- paste(
  paste0(mistake_table$mistake_code, " – ", mistake_table$mistake_title),
  collapse = "\n"
)
