#' Interpret standardized differences
#'
#' Interpretation of standardized differences using different sets of rules of thumb.
#'
#'
#' @param d,g,delta Value or vector of effect size values.
#' @param rules Can be `"cohen1988"` (default), `"gignac2016"`, `"sawilowsky2009"` or custom set of [rules()].
#'
#' @note
#' The use of `"gignac2016"` is based on the [d_to_r()] conversion.
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
  if (is.rules(rules)) {
    return(interpret(abs(d), rules))
  } else {
    if (rules == "gignac2016") {
      return(interpret_r(d_to_r(d), rules = rules))
    } else if (rules == "cohen1988") {
      return(interpret(abs(d), rules(c(0.2, 0.5, 0.8), c("very small", "small", "medium", "large"))))
    } else if (rules == "sawilowsky2009") {
      return(interpret(abs(d),
                       rules(c(0.1, 0.2, 0.5, 0.8, 1.2, 2),
                             c("tiny", "very small", "small", "medium", "large", "very large", "huge"))))
    } else {
      stop("rules must be 'gignac2016','cohen1988', 'sawilowsky2009' or an object of type rules.")
    }
  }
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
