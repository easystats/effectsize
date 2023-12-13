#' Interpret Intraclass Correlation Coefficient (ICC)
#'
#' The value of an ICC lies between 0 to 1, with 0 indicating no reliability among raters and 1 indicating perfect reliability.
#'
#' @param icc Value or vector of Intraclass Correlation Coefficient (ICC) values.
#' @param rules Can be `"koo2016"` (default) or custom set of [rules()].
#' @param ... Not used for now.
#'
#' @section Rules:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' insight::print_md(.icc_rules, "ICC", "Koo (2016) (`{.rn}`; default):")
#' ```
#'
#' @examples
#' interpret_icc(0.6)
#' interpret_icc(c(0.4, 0.8))
#' @references
#' - Koo, T. K., and Li, M. Y. (2016). A guideline of selecting and reporting intraclass correlation coefficients for reliability research. Journal of chiropractic medicine, 15(2), 155-163.
#'
#' @keywords interpreters
#' @export
interpret_icc <- function(icc, rules = "koo2016", ...) {
  rules <- .match.rules(rules, .icc_rules)

  interpret(icc, rules)
}


# rules -------------------------------------------------------------------

#' @keywords internal
.icc_rules <- c(
  rules(c(0.5, 0.75, 0.9),
        c("poor", "moderate", "good", "excellent"),
        name = "koo2016", right = FALSE)
)
