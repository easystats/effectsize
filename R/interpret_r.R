#' Interpret Correlation Coefficient
#'
#' @param r Value or vector of correlation coefficient.
#' @param rules Can be `"funder2019"` (default), `"gignac2016"`, `"cohen1988"`,
#'   `"evans1996"`, `"lovakov2021"` or a custom set of [rules()].
#' @param ... Not directly used.
#'
#' @note As \eqn{\phi}{\phi} can be larger than 1 - it is recommended to compute
#'   and interpret Cramer's *V* instead.
#'
#' @section Rules:
#'
#' Rules apply to positive and negative *r* alike.
#'
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Funder & Ozer (2019) (`{.rn}`; default)",
#'             "Gignac & Szodorai (2016) (`{.rn}`):",
#'             "Cohen (1988) (`{.rn}`):",
#'             "Evans (1996) (`{.rn}`):",
#'             "Lovakov & Agadullina (2021) (`{.rn}`):")
#' insight::print_md(.r_rules, "r", titles)
#' ```
#'
#' @examples
#' interpret_r(.015)
#' interpret_r(c(.5, -.02))
#' interpret_r(.3, rules = "lovakov2021")
#'
#' @seealso Page 88 of APA's 6th Edition.
#'
#' @references
#' - Lovakov, A., & Agadullina, E. R. (2021). Empirically Derived Guidelines for
#' Effect Size Interpretation in Social Psychology. European Journal of Social
#' Psychology.
#'
#' - Funder, D. C., & Ozer, D. J. (2019). Evaluating effect size in
#' psychological research: sense and nonsense. Advances in Methods and Practices
#' in Psychological Science.
#'
#' - Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for
#' individual differences researchers. Personality and individual differences,
#' 102, 74-78.
#'
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd Ed.). New York: Routledge.
#'
#' - Evans, J. D. (1996). Straightforward statistics for the behavioral
#' sciences. Thomson Brooks/Cole Publishing Co.
#'
#' @keywords interpreters
#' @export
interpret_r <- function(r, rules = "funder2019", ...) {
  rules <- .match.rules(rules, .r_rules)

  interpret(abs(r), rules)
}

#' @export
#' @rdname interpret_r
interpret_phi <- interpret_r

#' @export
#' @rdname interpret_r
interpret_cramers_v <- interpret_r

#' @export
#' @rdname interpret_r
interpret_rank_biserial <- interpret_r

#' @export
#' @rdname interpret_r
interpret_fei <- interpret_r


# rules -------------------------------------------------------------------

#' @keywords internal
.r_rules <- c(
  rules(c(0.05, 0.1, 0.2, 0.3, 0.4),
        c("tiny", "very small", "small", "medium", "large", "very large"),
        name = "funder2019", right = FALSE
  ),
  rules(c(0.1, 0.2, 0.3),
        c("very small", "small", "moderate", "large"),
        name = "gignac2016", right = FALSE
  ),
  rules(c(0.1, 0.3, 0.5),
        c("very small", "small", "moderate", "large"),
        name = "cohen1988", right = FALSE
  ),
  rules(c(0.2, 0.4, 0.6, 0.8),
        c("very weak", "weak", "moderate", "strong", "very strong"),
        name = "evans1996", right = FALSE
  ),
  rules(c(0.12, 0.24, 0.41),
        c("very small", "small", "medium", "large"),
        name = "lovakov2021", right = FALSE
  )
)
