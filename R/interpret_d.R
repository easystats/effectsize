#' Interpret standardized differences
#'
#' Interpretation of standardized differences using different sets of rules of thumb.
#'
#'
#' @param d,g,delta Value or vector of effect size values.
#' @param rules Can be `"cohen1988"` (default), `"gignac2016"`, `"sawilowsky2009"` or custom set of [rules()].
#'
#' @section Rules:
#'
#' Rules apply to equally to positive and negative *d*.
#'
#' - Cohen (1988) (`"cohen1988"`; default)
#'   - **d < 0.2** - Very small
#'   - **0.2 < d < 0.5** - Small
#'   - **0.5 < d < 0.8** - Medium
#'   - **d > 0.8** - Large
#' - Sawilowsky (2009) (`"sawilowsky2009"`)
#'   - **d < 0.1** - Tiny
#'   - **0.1 < d < 0.2** - Very small
#'   - **0.2 < d < 0.5** - Small
#'   - **0.5 < d < 0.8** - Medium
#'   - **0.8 < d < 1.2** - Large
#'   - **1.2 < d < 2** - Very large
#'   - **d > 2** - Huge
#' - Gignac & Szodorai (2016) (`"gignac2016"`, based on the [d_to_r()] conversion, see [interpret_r()])
#'   - **d < 0.2** - Very small
#'   - **0.2 < d < 0.41** - Small
#'   - **0.41 < d < 0.63** - Moderate
#'   - **d > 0.63** - Large
#'
#' @examples
#' interpret_d(.02)
#' interpret_d(c(.5, .02))
#'
#' @references
#' - Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for individual differences researchers. Personality and individual differences, 102, 74-78.
#' - Cohen, J. (1988). Statistical power analysis for the behavioural sciences.
#' - Sawilowsky, S. S. (2009). New effect size rules of thumb.
#'
#' @export
interpret_d <- function(d, rules = "cohen1988") {
  if (is.character(rules) && rules == "gignac2016") {
    return(interpret_r(d_to_r(d), rules))
  }

  rules <- .match.rules(
    rules,
    list(
      cohen1988 = rules(c(0.2, 0.5, 0.8), c("very small", "small", "medium", "large"),
                        name = "cohen1988"),
      sawilowsky2009 = rules(c(0.1, 0.2, 0.5, 0.8, 1.2, 2),
                             c("tiny", "very small", "small", "medium", "large", "very large", "huge"),
                             name = "sawilowsky2009"),
      gignac2016 = NA # added for the correct error msg
    )
  )

  interpret(abs(d), rules)
}

#' @rdname interpret_d
#' @export
interpret_g <- function(g, rules = "cohen1988") {
  interpret_d(g, rules)
}

#' @rdname interpret_d
#' @export
interpret_delta <- function(delta, rules = "cohen1988") {
  interpret_d(delta, rules)
}
