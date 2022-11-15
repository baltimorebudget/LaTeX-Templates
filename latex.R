#' Insert blank page
#'
#' Generates LaTeX for "Intentionally left blank" pages.
#'
#' @importFrom knitr is_latex_output
#' @return LaTeX
#'
#' @export

insert_blank_page <- function() {
  if (is_latex_output()) {
    cat(
      "\\clearpage \\thispagestyle{empty} \\hspace{0pt}",
      "\\vfill \\begin{center} Intentionally left blank. \\end{center}",
      "\\vfill\\hspace{0pt}\\clearpage"
    )
  }
}

#' Stretch array
#'
#' Generates LaTeX for modifying how stretched tables should be
#'
#' @param factor Numeric. A number representing the distance between rows, defaults to 1.
#'
#' @importFrom knitr is_latex_output
#' @return LaTeX
#'
#' @export

stretch_array <- function(factor = 1) {

  if (is_latex_output()) {
    cat("\\renewcommand{\\arraystretch}{", factor, "}")
  }

}

#' Wrap table
#'
#' Generates LaTeX for allowing text to wrap around a table.
#'
#' @param side "l" or "r"
#' @param size In or px
#'
#' @importFrom knitr is_latex_output
#' @return LaTeX
#'
#' @export

wrap_table <- function(side = "l", size = "3in") {
  if (is_latex_output()) {
    cat("\\begin{wraptable}{", side, "}{", size, "}")
  }
}

#' End wrap table
#'
#' Generates LaTeX to end text wrapping around a table.
#'
#' @importFrom knitr is_latex_output
#' @return LaTeX
#'
#' @export

wrap_table_end <- function() {
  if (is_latex_output()) {
    # cat("\\begin{tablenotes}")
    # cat("\\item Table in dollars.")
    # cat("\\end{tablenotes}")
    cat("\\end{wraptable}")
  }
}
