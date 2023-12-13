#' Interpret Odds Ratio
#'
#' @param OR Value or vector of (log) odds ratio values.
#' @param rules Can be "`chen2010"` (default), `"cohen1988"` (through
#'   transformation to standardized difference, see [oddsratio_to_d()]) or custom set
#'   of [rules()].
#' @param log Are the provided values log odds ratio.
#' @inheritParams interpret
#'
#' @section Rules:
#'
#' Rules apply to OR as ratios, so OR of 10 is as extreme as a OR of 0.1 (1/10).
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Chen et al. (2010) (`{.rn}`; default):",
#'             "Cohen (1988) (`{.rn}`, based on the [oddsratio_to_d()] conversion, see [interpret_cohens_d()]):")
#'
#' insight::print_md(.oddsratio_rules, "OR", titles)
#' ```
#'
#' @examples
#' interpret_oddsratio(1)
#' interpret_oddsratio(c(5, 2))
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd Ed.). New York: Routledge.
#'
#' - Chen, H., Cohen, P., & Chen, S. (2010). How big is a big odds ratio?
#' Interpreting the magnitudes of odds ratios in epidemiological studies.
#' Communications in Statistics-Simulation and Computation, 39(4), 860-864.
#'
#' - Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
#' Effect-size indices for dichotomized outcomes in meta-analysis. Psychological
#' methods, 8(4), 448.
#'
#' @keywords interpreters
#' @export
interpret_oddsratio <- function(OR, rules = "chen2010", log = FALSE, ...) {
  if (log) {
    OR <- exp(abs(OR))
  } else {
    OR <- exp(abs(log(OR)))
  }

  rules <- .match.rules(rules, .oddsratio_rules)

  interpret(OR, rules)
}



# rules -------------------------------------------------------------------

#' @keywords internal
.oddsratio_rules <- c(
  rules(c(1.68, 3.47, 6.71), c("very small", "small", "medium", "large"),
        name = "chen2010", right = FALSE
  ),
  rules(d_to_oddsratio(c(0.2, 0.5, 0.8)), c("very small", "small", "medium", "large"),
        name = "cohen1988", right = FALSE)
)
