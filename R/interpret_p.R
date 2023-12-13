#' Interpret *p*-Values
#'
#' @param p Value or vector of p-values.
#' @param rules Can be `"default"`, `"rss"` (for *Redefine statistical
#'   significance* rules) or custom set of [rules()].
#'
#' @section Rules:
#'
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Default (`{.rn}`):",
#'             "RSS (`{.rn}`):")
#' insight::print_md(.p_rules, "p", titles)
#' ```
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
  rules <- .match.rules(rules, .p_rules)

  interpret(p, rules)
}


# rules -------------------------------------------------------------------

#' @keywords internal
.p_rules <- c(
  rules(0.05, c("significant", "not significant"),
        name = "default", right = FALSE
  ),
  rules(c(0.005, 0.05), c("significant", "suggestive", "not significant"),
        name = "rss", right = FALSE
  )
)
