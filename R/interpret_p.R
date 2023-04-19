#' Interpret *p*-Values
#'
#' @param p Value or vector of p-values.
#' @param rules Can be `"default"`, `"rss"` (for *Redefine statistical
#'   significance* rules) or custom set of [rules()].
#'
#' @section Rules:
#'
#' - Default
#'   - **p >= 0.05** - Not significant
#'   - **p < 0.05** - Significant
#' - Benjamin et al. (2018) (`"rss"`)
#'   - **p >= 0.05** - Not significant
#'   - **0.005 <= p < 0.05** - Suggestive
#'   - **p < 0.005** - Significant
#'
#' @references
#' - Benjamin, D. J., Berger, J. O., Johannesson, M., Nosek, B. A., Wagenmakers, E. J., Berk, R., ... & Cesarini, D. (2018). Redefine statistical significance. Nature Human Behaviour, 2(1), 6-10.
#'
#' @examples
#' interpret_p(c(.5, .02, 0.001))
#' interpret_p(c(.5, .02, 0.001), rules = "rss")
#'
#' @keywords interpreters
#' @export
interpret_p <- function(p, rules = "default") {
  rules <- .match.rules(
    rules,
    list(
      default = rules(c(0.05), c("significant", "not significant"),
        name = "default", right = FALSE
      ),
      rss = rules(c(0.005, 0.05), c("significant", "suggestive", "not significant"),
        name = "rss", right = FALSE
      )
    )
  )

  interpret(p, rules)
}
