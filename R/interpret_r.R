#' Interpret correlation
#'
#' @param r Value or vector of correlation coefficient.
#' @param rules Can be `"funder2019"` (default), `"gignac2016"`, `"cohen1988"`, `"evans1996"`, `"lovakov2021"` or a custom set of [rules()].
#'
#' @section Rules:
#'
#' Rules apply positive and negative *r* alike.
#'
#' - Funder & Ozer (2019) (`"funder2019"`; default)
#'   - **r < 0.05** - Tiny
#'   - **0.05 <= r < 0.1** - Very small
#'   - **0.1 <= r < 0.2** - Small
#'   - **0.2 <= r < 0.3** - Medium
#'   - **0.3 <= r < 0.4** - Large
#'   - **r >= 0.4** - Very large
#' - Gignac & Szodorai (2016) (`"gignac2016"`)
#'   - **r < 0.1** - Very small
#'   - **0.1 <= r < 0.2** - Small
#'   - **0.2 <= r < 0.3** - Moderate
#'   - **r >= 0.3** - Large
#' - Cohen (1988) (`"cohen1988"`)
#'   - **r < 0.1** - Very small
#'   - **0.1 <= r < 0.3** - Small
#'   - **0.3 <= r < 0.5** - Moderate
#'   - **r >= 0.5** - Large
#' - Lovakov & Agadullina (2021) (`"lovakov2021"`)
#'   - **r < 0.12** - Very small
#'   - **0.12 <= r < 0.24** - Small
#'   - **0.24 <= r < 0.41** - Moderate
#'   - **r >= 0.41** - Large
#' - Evans (1996) (`"evans1996"`)
#'   - **r < 0.2** - Very weak
#'   - **0.2 <= r < 0.4** - Weak
#'   - **0.4 <= r < 0.6** - Moderate
#'   - **0.6 <= r < 0.8** - Strong
#'   - **r >= 0.8** - Very strong
#'
#' @examples
#' interpret_r(.015)
#' interpret_r(c(.5, -.02))
#' interpret_r(.3, rules = "lovakov2021")
#' @seealso Page 88 of APA's 6th Edition.
#'
#' @references
#' - Lovakov, A., & Agadullina, E. R. (2021). Empirically Derived Guidelines for Effect Size Interpretation in Social Psychology. European Journal of Social Psychology.
#' - Funder, D. C., & Ozer, D. J. (2019). Evaluating effect size in psychological research: sense and nonsense. Advances in Methods and Practices in Psychological Science.
#' - Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for individual differences researchers. Personality and individual differences, 102, 74-78.
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Evans, J. D. (1996). Straightforward statistics for the behavioral sciences. Thomson Brooks/Cole Publishing Co.
#'
#' @export
interpret_r <- function(r, rules = "funder2019") {
  rules <- .match.rules(
    rules,
    list(
      funder2019 = rules(c(0.05, 0.1, 0.2, 0.3, 0.4),
        c("tiny", "very small", "small", "medium", "large", "very large"),
        name = "funder2019", right = FALSE
      ),
      gignac2016 = rules(c(0.1, 0.2, 0.3),
        c("very small", "small", "moderate", "large"),
        name = "gignac2016", right = FALSE
      ),
      cohen1988 = rules(c(0.1, 0.3, 0.5),
        c("very small", "small", "moderate", "large"),
        name = "cohen1988", right = FALSE
      ),
      evans1996 = rules(c(0.2, 0.4, 0.6, 0.8),
        c("very weak", "weak", "moderate", "strong", "very strong"),
        name = "evans1996", right = FALSE
      ),
      lovakov2021 = rules(c(0.12, 0.24, 0.41),
        c("very small", "small", "medium", "large"),
        name = "lovakov2021", right = FALSE
      )
    )
  )

  interpret(abs(r), rules)
}
