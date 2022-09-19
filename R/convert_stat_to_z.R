#' Convert *r* to Fisher *z'*
#'
#' These functions are convenience functions to convert statistics to z.
#'
#' @param r A correlation *r* statistic.
#'
#' @examples
#' r_to_fisherz(0.5)
#'
#' @export
r_to_fisherz <- function (r) {
  (log(1 + r) - log(1 - r)) / 2
}
