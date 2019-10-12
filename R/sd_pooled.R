#' Pooled Standard Deviation
#'
#' The Pooled Standard Deviation is a weighted average of standard deviations for two or more groups, with more "weight" given to larger sample sizes.
#'
#' @inheritParams cohens_d
#'
#' @export
sd_pooled <- function(x, y = NULL, data = NULL) {
  .sd_pooled(x, y, data, robust = FALSE)
}



#' @rdname sd_pooled
#' @export
mad_pooled <- function(x, y = NULL, data = NULL) {
  .sd_pooled(x, y, data, robust = TRUE)
}








#' @importFrom stats mad
#' @export
.sd_pooled <- function(x, y = NULL, data = NULL, robust = FALSE) {
  out <- .deal_with_cohens_d_arguments(x, y, data)
  x <- out$x
  y <- out$y

  if (robust) {
    sd1 <- mad(x, na.rm = TRUE)
    sd2 <- mad(y, na.rm = TRUE)
  } else {
    sd1 <- sd(x, na.rm = TRUE)
    sd2 <- sd(y, na.rm = TRUE)
  }


  sqrt((sd1^2 + sd2^2) / 2)

  # Cohen's more complicated formula:
  # n1 <- length(x)
  # n2 <- length(y)
  # sqrt( (n1-1) * var(x) + (n2-1) * var(y) / n1 + n2 - 2)
}
