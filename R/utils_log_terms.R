# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.log_terms <- function(model) {
  x <- insight::find_terms(model, flatten = TRUE)
  gsub("^log\\((.*)\\)", "\\1", x[grepl("^log\\((.*)\\)", x)])
}
