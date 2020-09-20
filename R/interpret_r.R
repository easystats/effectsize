#' Interpret correlation
#'
#' @param r Value or vector of correlation coefficient.
#' @param rules Can be `"funder2019"` (default), `"gignac2016"`, `"cohen1988"`, `"evans1996"` or custom set of [rules()].
#'
#'
#'
#' @examples
#' interpret_r(r = .015)
#' interpret_r(r = c(.5, -.02))
#'
#' @seealso Page 88 of APA's 6th Edition.
#'
#' @references
#' - Funder, D. C., & Ozer, D. J. (2019). Evaluating effect size in psychological research: sense and nonsense. Advances in Methods and Practices in Psychological Science.
#' - Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for individual differences researchers. Personality and individual differences, 102, 74-78.
#' - Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#' - Evans, J. D. (1996). Straightforward statistics for the behavioral sciences. Thomson Brooks/Cole Publishing Co.
#'
#' @export
interpret_r <- function(r, rules = "funder2019") {
  rules <- .match.rules(
    rules,
    list(
      funder2019 = rules(c(0.05, 0.1, 0.2, 0.3, 0.4),
                         c("tiny", "very small", "small", "medium", "large", "very large")),
      gignac2016 = rules(c(0.1, 0.2, 0.3),
                         c("very small", "small", "moderate", "large")),
      cohen1988 = rules(c(0.1, 0.3, 0.5),
                        c("very small", "small", "moderate", "large")),
      evans1996 = rules(c(0.2, 0.4, 0.6, 0.8),
                        c("very weak", "weak", "moderate", "strong", "very strong"))
    )
  )

  interpret(abs(r), rules)
}
