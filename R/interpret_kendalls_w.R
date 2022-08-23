#' Interpret Kendall's coefficient of concordance
#'
#' @param w Value or vector of Kendall's coefficient of concordance.
#' @param rules Can be `"landis1977"` (default) or a custom set of [rules()].
#'
#' @section Rules:
#'
#' ```{r, echo = FALSE, results='asis'}
#' insight::print_md(.rules_kendalls_w$landis1977, value_name = "w", title = "Landis & Koch (1977)")
#' ```
#'
#' @references
#' - Landis, J. R., & Koch G. G. (1977). The measurement of observer agreement
#' for categorical data. Biometrics, 33:159-74.
#'
#' @export
#'
interpret_kendalls_w <- function(w, rules = "landis1977") {
  rules <- .match.rules(
    rules,
    .rules_kendalls_w
  )

  interpret(w, rules)
}

#' @keywords internal
.rules_kendalls_w <- list(landis1977 = rules(
  c(0.2, 0.4, 0.6, 0.8),
  c(
    "slight agreement",
    "fair agreement",
    "moderate agreement",
    "substantial agreement",
    "almost perfect agreement"
  ),
  name = "landis1977",
  right = FALSE
))
