#' Interpret ANOVA effect size
#'
#' @param es Value or vector of eta / omega / epsilon squared values.
#' @param rules Can be `"field2013"` (default), `"cohen1992"` or custom set of [rules()].
#'
#'
#' @section Rules:
#'
#' - Field (2013) (`"field2013"`; default)
#'   - **ES < 0.01** - Very small
#'   - **0.01 < ES < 0.06** - Small
#'   - **0.16 < ES < 0.14** - Medium
#'   - **ES > 0.14 ** - Large
#' - Cohen (1992) (`"cohen1992"`) applicable to one-way anova, or to *partial*
#' eta / omega / epsilon squared in multi-way anova.
#'   - **ES < 0.02** - Very small
#'   - **0.02 < ES < 0.13** - Small
#'   - **0.13 < ES < 0.26** - Medium
#'   - **ES > 0.26** - Large
#'
#' @examples
#' interpret_eta_squared(.02)
#' interpret_eta_squared(c(.5, .02), rules = "cohen1992")
#'
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
                        c("very small", "small", "medium", "large"),
                        name = "field2013"),
      cohen1992 = rules(c(0.02, 0.13, 0.26),
                        c("very small", "small", "medium", "large"),
                        name = "cohen1992")
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
