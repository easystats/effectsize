#' Safe transformation from factor/character to numeric
#'
#' @importFrom stats na.omit
#' @keywords internal
.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}
