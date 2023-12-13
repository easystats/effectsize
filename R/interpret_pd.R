#' Interpret Probability of Direction (pd)
#'
#' @param pd Value or vector of probabilities of direction.
#' @param rules Can be `"default"`, `"makowski2019"` or a custom set of
#'   [rules()].
#' @param ... Not directly used.
#'
#' @section Rules:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' titles <- c("Default (i.e., equivalent to p-values):",
#'             "Makowski et al. (2019) (`{.rn}`):")
#' insight::print_md(.pd_rules, "pd", titles)
#' ```
#'
#' @examples
#' interpret_pd(.98)
#' interpret_pd(c(.96, .99), rules = "makowski2019")
#' @references
#' - Makowski, D., Ben-Shachar, M. S., Chen, S. H., and Lüdecke, D. (2019). Indices of effect existence and significance in the Bayesian framework. Frontiers in psychology, 10, 2767.
#'
#' @keywords interpreters
#' @export
interpret_pd <- function(pd, rules = "default", ...) {
  rules <- .match.rules(rules, .pd_rules)

  interpret(pd, rules)
}



# rules -------------------------------------------------------------------

#' @keywords internal
.pd_rules <- c(
  rules(c(0.975), c("not significant", "significant"),
        name = "default", right = TRUE
  ),
  rules(c(0.95, 0.97, 0.99, 0.999), c("uncertain", "possibly existing", "likely existing", "probably existing", "certainly existing"),
        name = "makowski2019", right = TRUE
  )
)
