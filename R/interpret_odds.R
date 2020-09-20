#' Interpret (log) Odds ratio
#'
#' @param odds Value or vector of (log) odds ratio values.
#' @param rules Can be "`chen2010"` (default), `"cohen1988"` (through transformation to standardized difference, see [odds_to_d()]) or custom set of [rules()].
#' @param log Are the provided values log odds ratio.
#'
#'
#' @examples
#' interpret_odds(1)
#' interpret_odds(c(5, 2))
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#' - Chen, H., Cohen, P., & Chen, S. (2010). How big is a big odds ratio? Interpreting the magnitudes of odds ratios in epidemiological studies. Communications in Statistics—Simulation and Computation, 39(4), 860-864.
#' - Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological methods, 8(4), 448.
#'
#' @export
interpret_odds <- function(odds, rules = "chen2010", log = FALSE) {

  if (log) {
    odds <- exp(abs(odds))
  } else {
    odds <- exp(abs(log(odds)))
  }


  if (is.character(rules) && rules == "cohen1988") {
    d <- oddsratio_to_d(odds, log = FALSE)
    return(interpret_d(abs(d), rules = rules))
  }

  rules <- .match.rules(
    rules,
    list(
      chen2010 = rules(c(1.68, 3.47, 6.71), c("very small", "small", "medium", "large")),
      cohen1988 = NA # for correct error msg
    )
  )

  interpret(odds, rules)
}
