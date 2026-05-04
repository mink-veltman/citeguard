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
    "Non-significant finding",
    "Overgeneralization of population",
    "Misrepresentation of experimental conditions",
    "Causal relation distorted or confused",
    "Independent variable distortion",
    "Dependent variable distortion",
    "Measure-to-construct inflation",
    "Non-contextualized citation"
  ),
  mistake_description = c(
    "The findings of the cited paper are directly contradicted in the citation.",
    "Neither the cited finding nor the measures exist in the cited paper.",
    "A non-significant finding is cited.",
    "The cited finding is overgeneralized to a larger population without justification.",
    "The experimental conditions are misrepresented.",
    "The causal claim is reversed or overstated.",
    "The cited independent variable is distorted.",
    "The cited dependent variable is distorted.",
    "A measure is inflated to a broader construct without justification.",
    "Citation relevance is not apparent."
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
