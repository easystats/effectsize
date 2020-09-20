#' Interpret Bayes Factor (BF)
#'
#' @param bf Value or vector of Bayes factor (BF) values.
#' @param rules Can be `"jeffreys1961"` (default), `"raftery1995"` or custom set of [rules()].
#' @param include_value Include the value in the output.
#'
#'
#' @examples
#' interpret_bf(1)
#' interpret_bf(c(5, 2))
#'
#' @references
#' - Jeffreys, H. (1961), Theory of Probability, 3rd ed., Oxford University Press, Oxford.
#' - Raftery, A. E. (1995). Bayesian model selection in social research. Sociological methodology, 25, 111-164.
#' - Jarosz, A. F., & Wiley, J. (2014). What are the odds? A practical guide to computing and reporting Bayes factors. The Journal of Problem Solving, 7(1), 2.
#'
#' @export
interpret_bf <- function(bf, rules = "jeffreys1961", include_value = FALSE) {
  if (any(bf < 0)) {
    warning("Negative BFs detected. These are not possible. Converting to NA.")
    bf[bf < 0] <- NA
  }
  orig_bf <- bf

  dir <- ifelse(bf < 1, "against", "in favour of")
  bf <- exp(abs(log(bf)))

  rules <- .match.rules(
    rules,
    list(
      jeffreys1961 = rules(c(3, 10, 30, 100), c("anecdotal", "moderate", "strong", "very strong", "extreme")),
      raftery1995 = rules(c(3, 20, 150), c("weak", "positive", "strong", "very strong"))
    )
  )

  interpretation <- interpret(bf, rules)


  interpretation <- paste0(interpretation, " evidence ", dir)
  interpretation[orig_bf==1] <- "no evidence"
  if (include_value) {
    interpretation <- paste0(interpretation, " (", insight::format_bf(orig_bf, protect_ratio = TRUE),")")
  }

  interpretation[is.na(orig_bf)] <- NA

  return(interpretation)
}
