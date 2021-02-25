#' Interpret Kendall's coefficient of concordance
#'
#' @param w Value or vector of Kendall's coefficient of concordance.
#' @param rules Can be `"landis1977"` (default) or a custom set of [rules()].
#'
#' @section Rules:
#'
#' - Landis & Koch (1977) (`"landis1977"`; default)
#'   - **0.00 <= w < 0.20** - Slight agreement
#'   - **0.20 <= w < 0.40** - Fair agreement
#'   - **0.40 <= w < 0.60** - Moderate agreement
#'   - **0.60 <= w < 0.80** - Substantial agreement
#'   - **w >= 0.80**        - Almost perfect agreement

#' @references
#' - Landis, J. R., & Koch G. G. (1977). The measurement of observer agreement
#' for categorical data. Biometrics, 33:159-74.
#'
#' @export
#'
interpret_kendalls_w <- function(w, rules = "landis1977") {
  rules <- .match.rules(
    rules,
    list(
      landis1977 = rules(c(0, 0.2, 0.4, 0.6, 0.8),
        c(
          "slight agreement", "fair agreement", "moderate agreement",
          "substantial agreement", "almost perfect agreement"
        ),
        name = "landis1977",
        right = FALSE
      )
    )
  )

  interpret(w, rules)
}
