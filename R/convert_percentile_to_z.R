#' Z score to Percentile
#'
#' Convert between Z scores (values expressed in terms of standard deviation) to percentiles (the proportion of a normal distribution below).
#'
#' @param z,percentile Z score or percentile.
#'
#' @examples
#' z_to_percentile(1.96)
#' percentile_to_z(0.975)
#'
#' @importFrom stats pnorm qnorm
#' @export
convert_z_to_percentile <- function(z) {
  stats::pnorm(z)
}

#' @rdname convert_z_to_percentile
#' @export
convert_percentile_to_z <- function(percentile) {
  stats::qnorm(percentile)
}


#' @rdname convert_z_to_percentile
#' @export
z_to_percentile <- convert_z_to_percentile

#' @rdname convert_z_to_percentile
#' @export
percentile_to_z <- convert_percentile_to_z
