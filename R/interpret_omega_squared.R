#' Interpret ANOVA Effect Sizes
#'
#' @param es Value or vector of (partial) eta / omega / epsilon squared or semipartial r squared values.
#' @param rules Can be `"field2013"` (default), `"cohen1992"` or custom set of [rules()].
#' @param ... Not used for now.
#'
#' @section Rules:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Field (2013) (`{.rn}`; default):",
#'             "Cohen (1992) (`{.rn}`):\n(applicable to one-way anova, or to *partial* eta / omega / epsilon squared in multi-way anova)")
#'
#' insight::print_md(.omega_squared_rules, "ES", titles)
#' ```
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
#' - Cohen, J. (1992). A power primer. Psychological bulletin, 112(1), 155.
#'
#' @keywords interpreters
#' @export
interpret_omega_squared <- function(es, rules = "field2013", ...) {
  rules <- .match.rules(rules, .omega_squared_rules)

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


# rules -------------------------------------------------------------------

#' @keywords internal
.omega_squared_rules <- c(
  rules(c(0.01, 0.06, 0.14),
        c("very small", "small", "medium", "large"),
        name = "field2013", right = FALSE
  ),
  rules(c(0.02, 0.13, 0.26),
        c("very small", "small", "medium", "large"),
        name = "cohen1992", right = FALSE
  )
)
