#' Interpret Bayesian Posterior Percentage in ROPE.
#'
#' Interpretation of
#'
#' @param rope Value or vector of percentages in ROPE.
#' @param ci The Credible Interval (CI) probability, corresponding to the proportion of HDI, that was used. Can be `1` in the case of "full ROPE".
#' @param rules A character string (see details) or a custom set of [rules()].
#'
#' @section Rules:
#'
#' - Default
#'   - For CI < 1
#'     - **Rope = 0** - Significant
#'     - **0 < Rope < 1** - Undecided
#'     - **Rope = 1** - Negligible
#'   - For CI = 1
#'     - **Rope < 0.01** - Significant
#'     - **0.01 < Rope < 0.025** - Probably significant
#'     - **0.025 < Rope < 0.975** - Undecided
#'     - **0.975 < Rope < 0.99** - Probably negligible
#'     - **Rope > 0.99** - Negligible
#'
#'
#' @examples
#' interpret_rope(0, ci = 0.9)
#' interpret_rope(c(0.005, 0.99), ci = 1)
#' @references
#' [BayestestR's reporting guidelines](https://easystats.github.io/bayestestR/articles/guidelines.html)
#'
#' @export
interpret_rope <- function(rope, ci = 0.9, rules = "default") {
  if (ci < 1) {
    e <- .Machine$double.eps

    default_rule <- rules(c(0, 0 + e, 1 - e, 1),
      c("significant", "undecided", "undecided", "negligible"),
      name = "default"
    )
  } else {
    default_rule <- rules(c(0.01, 0.025, 0.975, 0.99),
      c(
        "significant", "probably significant",
        "undecided",
        "probably negligible", "negligible"
      ),
      name = "default"
    )
  }

  rules <- .match.rules(
    rules,
    list(
      default = default_rule
    )
  )

  interpret(rope, rules)
}
