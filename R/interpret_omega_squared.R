#' Interpret ANOVA Effect Sizes
#'
#' @param es Value or vector of (partial) eta / omega / epsilon squared or
#'   semipartial r squared values, or Cohen's f or f squared.
#' @param rules Can be `"field2013"` (default for eta squared), `"cohen1992"`
#'   (default for f squared) or custom set of [rules()].
#' @param ... Not used for now.
#'
#' @section Rules:
#'
#' - Field (2013) (`"field2013"`; default)
#'   - **ES < 0.01** - Very small
#'   - **0.01 <= ES < 0.06** - Small
#'   - **0.06 <= ES < 0.14** - Medium
#'   - **ES >= 0.14 ** - Large
#' - Cohen (1992) table 1 row 8 (`"cohen1992r8"` or `"cohen1992"`; multiple and multiple partial correlation)
#'   - **ES < 0.02** - Very small
#'   - **0.02 <= ES < 0.13** - Small
#'   - **0.13 <= ES < 0.26** - Medium
#'   - **ES >= 0.26** - Large
#' - Cohen (1992) row 7 (`"cohen1992r7"`; one-way ANOVA)
#'   - **ES < 0.01** - Very small
#'   - **0.01 <= ES < 0.06** - Small
#'   - **0.06 <= ES < 0.14** - Medium
#'   - **ES >= 0.14** - Large
#'
#' The same rules can be applied to Cohen's \eqn{f^2} (and \eqn{f}) by transforming those values:
#'
#' ```{r}
#' # Field (2013)
#' eta2_to_f2(c(small = 0.01, medium = 0.06, large = 0.14))
#' ```
#'
#' Cohen's rules are derived by applying the reverse transformation on Cohen's
#' cut-offs for \eqn{f^2} (row 8) and \eqn{f} (row 7):
#'
#' ```{r}
#' # Row 8
#' f2_to_eta2(c(small = 0.02, medium = 0.15, large = 0.35))
#'
#' # Row 7
#' f_to_eta2(c(small = 0.1, medium = 0.25, large = 0.4))
#' ```
#'
#' @examples
#' interpret_eta_squared(.02)
#' interpret_eta_squared(c(.5, .02), rules = "cohen1992r8")
#'
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
  rules <- .match.rules(rules, .anova_rules)

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

#' @export
#' @rdname interpret_omega_squared
interpret_cohens_f2 <- function(es, rules = "field2013", ...) {
  rules <- .match.rules(
    rules,
    lapply(.anova_rules, transform, transform = eta2_to_f2)
  )

  interpret(es, rules)
}

#' @export
#' @rdname interpret_omega_squared
interpret_cohens_f <- function(es, rules = "field2013", ...) {
  rules <- .match.rules(
    rules,
    lapply(.anova_rules, transform, transform = eta2_to_f)
  )

  interpret(es, rules)
}

# Rules ------------------------------------------------------------------

#' @keywords internal
.anova_rules <- list(
  field2013 = rules(
    c(0.01, 0.06, 0.14),
    c("very small", "small", "medium", "large"),
    name = "field2013",
    right = FALSE
  ),

  cohen1992 = rules(
    f2_to_eta2(c(0.02, 0.15, 0.35)),
    c("very small", "small", "medium", "large"),
    name = "cohen1992",
    right = FALSE
  ),
  cohen1992r7 = rules(
    f_to_eta2(c(0.1, 0.25, 0.40)),
    c("very small", "small", "medium", "large"),
    name = "cohen1992",
    right = FALSE
  )
)
.anova_rules$cohen1992r8 <- .anova_rules$cohen1992
