#' @importFrom stats qnorm
#' @rdname d_to_r
#' @export
percentage_to_d <- function(percentage, ...) {
  range_distribution <- (qnorm(0.999) - qnorm(0.001))
  percentage * range_distribution
}

#' @rdname d_to_r
#' @export
d_to_percentage <- function(d, ...) {
  range_distribution <- (qnorm(0.999) - qnorm(0.001))
  d / range_distribution
}

#' @rdname d_to_r
#' @export
convert_percentage_to_d <- percentage_to_d

#' @rdname d_to_r
#' @export
convert_d_to_percentage <- d_to_percentage
