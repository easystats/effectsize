#' Interpret ANOVA Effect Sizes
#'
#' @param es Value or vector of (partial) eta / omega / epsilon squared or semipartial r squared values.
#' @param rules Can be `"field2013"` (default), `"cohen1992"` or custom set of [rules()].
#' @param ... Not used for now.
#'
#' @section Rules:
#'
#' - Field (2013) (`"field2013"`; default)
#'   - **ES < 0.01** - Very small
#'   - **0.01 <= ES < 0.06** - Small
#'   - **0.06 <= ES < 0.14** - Medium
#'   - **ES >= 0.14 ** - Large
#' - Cohen (1992) (`"cohen1992"`; multiple and multiple partial correlation)
#'   - **ES < 0.02** - Very small
#'   - **0.02 <= ES < 0.13** - Small
#'   - **0.13 <= ES < 0.26** - Medium
#'   - **ES >= 0.26** - Large
#'
#' The `"cohen1992"` cutoffs correspond to the rounded \eqn{R^2} values
#' obtained from the \eqn{f^2} benchmarks 0.02, 0.15, and 0.35 in row 8 of
#' Cohen's Table 1 using \eqn{R^2 = f^2 / (1 + f^2)}. Row 7 gives separate
#' one-way ANOVA benchmarks of \eqn{f} = 0.10, 0.25, and 0.40. Squaring
#' these values and applying the same conversion gives rounded eta-squared
#' benchmarks of 0.01, 0.06, and 0.14. Applying either set of cutoffs to
#' rank epsilon squared from a Kruskal-Wallis test is a descriptive heuristic.
#'
#' @examples
#' interpret_eta_squared(.02)
#' interpret_eta_squared(c(.5, .02), rules = "cohen1992")
#' @seealso https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize/
#'
#'
#' @references
#' - Field, A (2013) Discovering statistics using IBM SPSS Statistics. Fourth
#' Edition. Sage:London.
#'
#' - Cohen, J. (1992). [A power primer](https://doi.org/10.1037/0033-2909.112.1.155).
#'   Psychological Bulletin, 112(1), 155-159.
#'
#' @keywords interpreters
#' @export
interpret_omega_squared <- function(es, rules = "field2013", ...) {
  rules <- .match.rules(
    rules,
    list(
      field2013 = rules(
        c(0.01, 0.06, 0.14),
        c("very small", "small", "medium", "large"),
        name = "field2013",
        right = FALSE
      ),
      cohen1992 = rules(
        c(0.02, 0.13, 0.26),
        c("very small", "small", "medium", "large"),
        name = "cohen1992",
        right = FALSE
      )
    )
  )

  interpret(es, rules)
}

#' @export
#' @rdname interpret_omega_squared
interpret_eta_squared <- interpret_omega_squared

#' @export
#' @rdname interpret_omega_squared
interpret_epsilon_squared <- interpret_omega_squared

#' @export
#' @rdname interpret_omega_squared
interpret_r2_semipartial <- interpret_omega_squared
