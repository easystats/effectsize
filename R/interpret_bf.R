#' Interpret Bayes Factor (BF)
#'
#' @param bf Value or vector of Bayes factor (BF) values.
#' @param rules Can be `"jeffreys1961"` (default), `"raftery1995"` or custom set
#'   of [rules()] (for the *absolute magnitude* of evidence).
#' @param log Is the `bf` value `log(bf)`?
#' @param include_value Include the value in the output.
#' @inheritParams insight::format_bf
#'
#' @details Argument names can be partially matched.
#'
#' @section Rules:
#'
#' Rules apply to BF as ratios, so BF of 10 is as extreme as a BF of 0.1 (1/10).
#'
#' - Jeffreys (1961) (`"jeffreys1961"`; default)
#'   - **BF = 1** - No evidence
#'   - **1 < BF <= 3** - Anecdotal
#'   - **3 < BF <= 10** - Moderate
#'   - **10 < BF <= 30** - Strong
#'   - **30 < BF <= 100** - Very strong
#'   - **BF > 100** - Extreme.
#' - Raftery (1995) (`"raftery1995"`)
#'   - **BF = 1** - No evidence
#'   - **1 < BF <= 3** - Weak
#'   - **3 < BF <= 20** - Positive
#'   - **20 < BF <= 150** - Strong
#'   - **BF > 150** - Very strong
#'
#'
#' @examples
#' interpret_bf(1)
#' interpret_bf(c(5, 2, 0.01))
#'
#' @references
#' - Jeffreys, H. (1961), Theory of Probability, 3rd ed., Oxford University
#' Press, Oxford.
#'
#' - Raftery, A. E. (1995). Bayesian model selection in social research.
#' Sociological methodology, 25, 111-164.
#'
#' - Jarosz, A. F., & Wiley, J. (2014). What are the odds? A practical guide to
#' computing and reporting Bayes factors. The Journal of Problem Solving, 7(1), 2.
#'
#' @keywords interpreters
#' @export
interpret_bf <- function(bf,
                         rules = "jeffreys1961",
                         log = FALSE,
                         include_value = FALSE,
                         protect_ratio = TRUE,
                         exact = TRUE) {
  if (!log && any(bf < 0, na.rm = TRUE)) {
    insight::format_error("Negative BFs detected. These are not possible, and are {.i ignored}.")
  }

  if (!log) bf <- log(bf)

  # interpret strength
  rules <- .match.rules(
    rules,
    list(
      jeffreys1961 = rules(c(3, 10, 30, 100), c("anecdotal", "moderate", "strong", "very strong", "extreme"),
        name = "jeffreys1961"
      ),
      raftery1995 = rules(c(3, 20, 150), c("weak", "positive", "strong", "very strong"),
        name = "raftery1995"
      )
    )
  )

  interpretation <- interpret(bf, rules, transform = function(.x) exp(abs(.x)))
  interpretation[bf == 0] <- "no"

  # interpret direction
  dir <- interpret(bf, rules(0, c("against", "in favour of")))
  dir[bf == 0] <- "against or in favour of"

  # Format text
  if (include_value) {
    bf_fmt <- insight::format_bf(exp(bf), protect_ratio = protect_ratio, exact = exact)
    interpretation[] <- sprintf("%s evidence (%s) %s", interpretation, bf_fmt, dir)
  } else {
    interpretation[] <- paste0(interpretation, " evidence ", dir)
  }

  interpretation[is.na(bf)] <- ""
  interpretation[] <- trimws(interpretation, "right")

  interpretation
}
