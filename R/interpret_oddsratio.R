#' Interpret Odds Ratio
#'
#' @param OR Value or vector of (log) odds ratio values.
#' @param rules If `"cohen1988"` (default), `OR` is transformed to a
#'   standardized difference (via [oddsratio_to_d()]) and interpreted according
#'   to Cohen's rules (see [interpret_cohens_d()]; see Chen et al., 2010). If a
#'   custom set of [rules()] is used, OR is interpreted as is.
#' @param log Are the provided values log odds ratio.
#' @inheritParams interpret
#' @inheritParams oddsratio_to_d
#'
#' @section Rules:
#'
#' Rules apply to OR as ratios, so OR of 10 is as extreme as a OR of 0.1 (1/10).
#'
#' - Cohen (1988) (`"cohen1988"`, based on the [oddsratio_to_d()] conversion, see [interpret_cohens_d()])
#'   - **OR < 1.44** - Very small
#'   - **1.44 <= OR < 2.48** - Small
#'   - **2.48 <= OR < 4.27** - Medium
#'   - **OR >= 4.27** - Large
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
interpret_oddsratio <- function(OR, rules = "cohen1988", p0 = NULL, log = FALSE, ...) {
  if (is.character(rules) && rules == "cohen1988") {
    d <- oddsratio_to_d(OR, p0, log = log)
    return(interpret_cohens_d(d, rules = rules))
  }

  if (log) {
    OR <- exp(OR)
  }

  interpret(OR, rules, transform = function(.x) ifelse(.x < 1, 1 / .x, .x))
}
