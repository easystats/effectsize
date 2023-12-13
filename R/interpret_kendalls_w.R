#' Interpret Kendall's Coefficient of Concordance *W*
#'
#' @param w Value or vector of Kendall's coefficient of concordance.
#' @param rules Can be `"landis1977"` (default) or a custom set of [rules()].
#'
#' @section Rules:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' insight::print_md(.kendalls_w_rules, "W", "Landis & Koch (1977) (`{.rn}`; default):")
#' ```
#'
#' @references
#' - Landis, J. R., & Koch G. G. (1977). The measurement of observer agreement
#' for categorical data. Biometrics, 33:159-74.
#'
#' @keywords interpreters
#' @export
interpret_kendalls_w <- function(w, rules = "landis1977") {
  rules <- .match.rules(.kendalls_w_rules, rules)

  interpret(w, rules)
}



# rules --------------------------------------------------------------------

#' @keywords internal
.kendalls_w_rules <- c(
  rules(c(0.2, 0.4, 0.6, 0.8),
        c(
          "slight agreement", "fair agreement", "moderate agreement",
          "substantial agreement", "almost perfect agreement"
        ),
        name = "landis1977",
        right = FALSE
  )
)
