#' Interpret Bayesian diagnostic indices
#'
#' Interpretation of Bayesian indices of percentage in ROPE.
#'
#' @param rope Value or vector of percentages in ROPE.
#' @param ci The Credible Interval (CI) probability, corresponding to the proportion of HDI, that was used. Can be `1` in the case of "full ROPE".
#' @param rules A character string (see details) or a custom set of [rules()].
#'
#' @details
#' Rules sets:
#' - **ROPE**: Can be ["default"](https://easystats.github.io/bayestestR/articles/guidelines.html).
#'
#'
#' @examples#'
#' interpret_rope(0, ci = 0.9)
#' interpret_rope(c(0.005, 0.99), ci = 1)
#'
#' @references
#' [BayestestR's reporting guidelines](https://easystats.github.io/bayestestR/articles/guidelines.html)
#'
#' @export
interpret_rope <- function(rope, ci = 0.9, rules = "default") {
  if (is.character(rules) && rules == "default" && ci < 1) {
    return(ifelse(rope == 0, "significant",
                  ifelse(rope == 1, "negligible",
                         "not significant")))
  }


  rules <- .match.rules(
    rules,
    list(
      default = rules(c(0.01, 0.025,0.975, 0.99),
                      c("significant", "probably significant",
                        "not significant",
                        "probably negligible", "negligible"))
    )
  )

  interpret(rope, rules)
}
