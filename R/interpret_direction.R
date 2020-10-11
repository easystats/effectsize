#' Interpret direction
#'
#' @param x Numeric value.
#'
#'
#'
#' @examples
#' interpret_direction(.02)
#' interpret_direction(c(.5, -.02))
#' #
#' @export
interpret_direction <- function(x) {
  interpret(0, rules(0, c("negative", "positive"), name = "math"))
}
