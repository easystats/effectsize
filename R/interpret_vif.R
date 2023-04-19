#' Interpret the Variance Inflation Factor (VIF)
#'
#' Interpret VIF index of multicollinearity.
#'
#' @param vif Value or vector of VIFs.
#' @param rules Can be `"default"` or a custom set of [rules()].
#'
#' @section Rules:
#'
#' - Default
#'   - **VIF < 5** - Low
#'   - **5 <= VIF < 10** - Moderate
#'   - **VIF >= 10** - High
#'
#' @examples
#'
#' interpret_vif(c(1.4, 30.4))
#'
#' @keywords interpreters
#' @export
interpret_vif <- function(vif, rules = "default") {
  rules <- .match.rules(
    rules,
    list(
      default = rules(c(5, 10),
        c("low", "moderate", "high"),
        name = "default", right = FALSE
      )
    )
  )

  interpret(vif, rules)
}
