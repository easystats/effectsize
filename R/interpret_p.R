#' Interpret p-values
#'
#' @param p Value or vector of p-values.
#' @param rules Can be `"default"`, `"rss"` (for *Redefine statistical
#'   significance* rules) or custom set of [rules()].
#'
#' @references
#' - Benjamin, D. J., Berger, J. O., Johannesson, M., Nosek, B. A., Wagenmakers, E. J., Berk, R., ... & Cesarini, D. (2018). Redefine statistical significance. Nature Human Behaviour, 2(1), 6-10.
#'
#' @examples
#' interpret_p(.02)
#' interpret_p(c(.5, .02))
#'
#' @export
interpret_p <- function(p, rules = "default") {
  if (is.rules(rules)) {
    return(interpret(p, rules))
  } else {
    if (rules == "default") {
      return(interpret(p, rules(c(0.05), c("significant", "not significant"))))
    } else if (rules == "rss") {
      return(interpret(p, rules(c(0.005, 0.05), c("significant", "suggestive", "not significant"))))
    } else {
      stop("rules must be 'default', 'rss' or an object of type rules.")
    }
  }
}
