#' Interpret Probability of Direction (pd)
#'
#' @param pd Value or vector of probabilities of direction.
#' @param rules Can be `"default"`, `"makowski2019"` or a custom set of
#'   [rules()].
#' @param ... Not directly used.
#'
#' @section Rules:
#'
#' - Default (i.e., equivalent to p-values)
#'   - **pd <= 0.975** - not significant
#'   - **pd > 0.975** - significant
#'
#' - Makowski et al. (2019) (`"makowski2019"`)
#'   - **pd <= 0.95** - uncertain
#'   - **pd > 0.95** - possibly existing
#'   - **pd > 0.97** - likely existing
#'   - **pd > 0.99** - probably existing
#'   - **pd > 0.999** - certainly existing
#'
#' @examples
#' interpret_pd(.98)
#' interpret_pd(c(.96, .99), rules = "makowski2019")
#' @references
#' - Makowski, D., Ben-Shachar, M. S., Chen, S. H., and Lüdecke, D. (2019). Indices of effect existence and significance in the Bayesian framework. Frontiers in psychology, 10, 2767.
#'
#' @export
interpret_pd <- function(pd, rules = "default", ...) {
  rules <- .match.rules(
    rules,
    list(
      default = rules(c(0.975), c("not significant", "significant"),
        name = "default", right = TRUE
      ),
      makowski2019 = rules(c(0.95, 0.97, 0.99, 0.999), c("uncertain", "possibly existing", "likely existing", "probably existing", "certainly existing"),
        name = "makowski2019", right = TRUE
      )
    )
  )

  interpret(pd, rules)
}
