#' @importFrom stats qnorm
#' @rdname convert-between-effect-sizes
#' @export
percentage_to_d <- function(percentage, ...) {
  range_distribution <- (qnorm(0.999) - qnorm(0.001))
  percentage * range_distribution
}

#' @rdname convert-between-effect-sizes
#' @export
d_to_percentage <- function(d, ...) {
  range_distribution <- (stats::qnorm(0.999) - stats::qnorm(0.001))
  d / range_distribution
}

#' @rdname convert-between-effect-sizes
#' @export
convert_percentage_to_d <- percentage_to_d

#' @rdname convert-between-effect-sizes
#' @export
convert_d_to_percentage <- d_to_percentage
