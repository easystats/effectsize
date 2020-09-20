#' Interpret ANOVA effect size
#'
#' @param es Value or vector of eta / omega / epsilon squared values.
#' @param rules Can be `"field2013"` (default), `"cohen1992"` or custom set of [rules()].
#'
#' @note `"cohen1992"`'s rules are applicable to one-way anova, or to *partial*
#'   eta / omega / epsilon squared in multi-way anova.
#'
#' @examples
#' interpret_eta_squared(.02)
#' interpret_eta_squared(c(.5, .02), rules = "cohen1992")
#'
#' @seealso http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
#'
#'
#' @references
#' - Field, A (2013) Discovering statistics using IBM SPSS Statistics. Fourth Edition. Sage:London.
#' - Cohen, J. (1992). A power primer. Psychological bulletin, 112(1), 155.
#'
#' @export
interpret_omega_squared <- function(es, rules = "field2013") {
  rules <- .match.rules(
    rules,
    list(
      field2013 = rules(c(0.01, 0.06, 0.14),
                        c("very small", "small", "medium", "large")),
      cohen1992 = rules(c(0.02, 0.13, 0.26),
                        c("very small", "small", "medium", "large"))
    )
  )

  interpret(es, rules)
}

#' @export
#' @rdname interpret_omega_squared
interpret_eta_squared <- interpret_omega_squared


#' @export
#' @rdname interpret_omega_squared
interpret_epsilon_squared <- interpret_omega_squared
