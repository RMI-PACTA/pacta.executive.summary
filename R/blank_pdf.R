#' Get the path to the blank template PDF
#'
#' @return A single string containing the path to a blank template PDF.
#'
#' @export

blank_pdf <- function() {
  system.file("extdata", "blank_pdf_do_not_delete.pdf", package = "pacta.executive.summary", mustWork = TRUE)
}
