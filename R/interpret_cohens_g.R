#' Interpret Cohen's *g*
#'
#'
#' @param g Value or vector of effect size values.
#' @param rules Can be `"cohen1988"` (default) or a custom set of [rules()].
#' @param ... Not directly used.
#'
#' @section Rules:
#'
#' Rules apply to equally to positive and negative *g* (i.e., they are given as
#' absolute values).
#'
#' - Cohen (1988) (`"cohen1988"`; default)
#'   - **d < 0.05** - Very small
#'   - **0.05 <= d < 0.15** - Small
#'   - **0.15 <= d < 0.25** - Medium
#'   - **d >= 0.25** - Large
#'
#' @note "*Since **g** is so transparently clear a unit, it is expected that
#'   workers in any given substantive area of the behavioral sciences will very
#'   frequently be able to set relevant \[effect size\] values without the
#'   proposed conventions, or set up conventions of their own which are suited
#'   to their area of inquiry.*" - Cohen, 1988, page 147.
#'
#' @examples
#' interpret_cohens_g(.02)
#' interpret_cohens_g(c(.3, .15))
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd Ed.). New York: Routledge.
#'
#' @keywords interpreters
#' @export
interpret_cohens_g <- function(g, rules = "cohen1988", ...) {
  rules <- .match.rules(
    rules,
    list(
      cohen1988 = rules(
        c(0.05, 0.15, 0.25),
        c("very small", "small", "medium", "large"),
        name = "cohen1988",
        right = FALSE
      )
    )
  )

  interpret(g, rules, transform = abs)
}
