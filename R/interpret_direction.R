#' Interpret Direction
#'
#' @param x Numeric value.
#'
#'
#' @examples
#' interpret_direction(.02)
#' interpret_direction(c(.5, -.02))
#' interpret_direction(0)
#'
#' @keywords interpreters
#' @export
interpret_direction <- function(x) {
  interpret(x, rules(0, c("negative", "positive"), name = "math", right = FALSE),
    transform = function(.x) {
      s <- sign(.x)
      replace(s, s == 0, NA_real_)
    }
  )
}
