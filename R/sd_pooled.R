#' Pooled Standard Deviation
#'
#' The Pooled Standard Deviation is a weighted average of standard deviations for two or more groups, with more "weight" given to larger sample sizes.
#'
#' @inheritParams cohens_d
#'
#' @return Numeric, the pooled standard deviation.
#' @examples
#' sd_pooled(Sepal.Length, Petal.Width, data = iris)
#' # or...
#' sd_pooled(Sepal.Length ~ Petal.Width, data = iris)
#' # or...
#' sd_pooled("Sepal.Length", "Petal.Width", data = iris)
#' @export
sd_pooled <- function(x, y = NULL, data = NULL) {
  x <- deparse(substitute(x), width.cutoff = 500)
  y <- deparse(substitute(y), width.cutoff = 500)
  .sd_pooled(x, y, data, robust = FALSE)
}



#' @rdname sd_pooled
#' @export
mad_pooled <- function(x, y = NULL, data = NULL) {
  x <- deparse(substitute(x), width.cutoff = 500)
  y <- deparse(substitute(y), width.cutoff = 500)
  .sd_pooled(x, y, data, robust = TRUE)
}








#' @importFrom stats mad sd as.formula
.sd_pooled <- function(x, y = NULL, data = NULL, robust = FALSE) {
  x <- .fix_arguments(x)
  y <- .fix_arguments(y)

  out <- .deal_with_cohens_d_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if (robust) {
    sd1 <- stats::mad(x, na.rm = TRUE)
    sd2 <- stats::mad(y, na.rm = TRUE)
  } else {
    sd1 <- stats::sd(x, na.rm = TRUE)
    sd2 <- stats::sd(y, na.rm = TRUE)
  }


  sqrt((sd1^2 + sd2^2) / 2)

  # Cohen's more complicated formula:
  # n1 <- length(x)
  # n2 <- length(y)
  # sqrt( (n1-1) * var(x) + (n2-1) * var(y) / n1 + n2 - 2)
}



.fix_arguments <- function(arg) {
  if (!is.null(arg)) {
    if (arg == "NULL") {
      arg <- NULL
    } else if (grepl("~", arg, fixed = TRUE)) {
      arg <- stats::as.formula(arg)
    } else if (grepl("\"", arg, fixed = TRUE)) {
      arg <- gsub("\"", "", arg, fixed = TRUE)
    }
  }
  arg
}