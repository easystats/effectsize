#' Interpret Standardized Differences
#'
#' Interpretation of standardized differences using different sets of rules of
#' thumb.
#'
#'
#' @param d,g,delta Value or vector of effect size values.
#' @param rules Can be `"cohen1988"` (default), `"gignac2016"`,
#'   `"sawilowsky2009"`, `"lovakov2021"` or a custom set of [rules()].
#' @param ... Not directly used.
#'
#' @section Rules:
#'
#' Rules apply to equally to positive and negative *d* (i.e., they are given as
#' absolute values).
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Cohen (1988) (`{.rn}`; default):",
#'             "Sawilowsky (2009) (`{.rn}`):",
#'             "Lovakov & Agadullina (2021) (`{.rn}`):",
#'             "Gignac & Szodorai (2016) (`{.rn}`, based on the [d_to_r()] conversion, see [interpret_r()]):")
#'
#' insight::print_md(.cohens_d_rules, "d", titles)
#' ```
#'
#' @examples
#' interpret_cohens_d(.02)
#' interpret_cohens_d(c(.5, .02))
#' interpret_cohens_d(.3, rules = "lovakov2021")
#' @references
#' - Lovakov, A., & Agadullina, E. R. (2021). Empirically Derived Guidelines for
#' Effect Size Interpretation in Social Psychology. European Journal of Social
#' Psychology.
#'
#' - Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for
#' individual differences researchers. Personality and individual differences,
#' 102, 74-78.
#'
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences
#' (2nd Ed.). New York: Routledge.
#'
#' - Sawilowsky, S. S. (2009). New effect size rules of thumb.
#'
#' @keywords interpreters
#' @export
interpret_cohens_d <- function(d, rules = "cohen1988", ...) {
  rules <- .match.rules(rules, .cohens_d_rules)

  interpret(abs(d), rules)
}

#' @rdname interpret_cohens_d
#' @export
interpret_hedges_g <- function(g, rules = "cohen1988") {
  interpret_cohens_d(g, rules)
}

#' @rdname interpret_cohens_d
#' @export
interpret_glass_delta <- function(delta, rules = "cohen1988") {
  interpret_cohens_d(delta, rules)
}


# rules -------------------------------------------------------------------

#' @keywords internal
.cohens_d_rules <- c(
  rules(c(0.2, 0.5, 0.8), c("very small", "small", "medium", "large"),
        name = "cohen1988", right = FALSE),
  rules(c(0.1, 0.2, 0.5, 0.8, 1.2, 2),
        c("tiny", "very small", "small", "medium", "large", "very large", "huge"),
        name = "sawilowsky2009", right = FALSE),
  rules(c(0.15, 0.36, 0.65),
        c("very small", "small", "medium", "large"),
        name = "lovakov2021", right = FALSE),
  rules(r_to_d(c(0.1, 0.2, 0.3)),
        c("very small", "small", "moderate", "large"),
        name = "gignac2016", right = FALSE)
)
