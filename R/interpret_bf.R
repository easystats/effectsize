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
#' interpret_bf(c(5, 2))
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
  if (log) bf <- exp(bf)

  if (any(bf < 0, na.rm = TRUE)) {
    insight::format_warning("Negative BFs detected. These are not possible, and are {.i ignored}.")
    bf[bf < 0] <- NA
  }

  orig_bf <- bf

  dir <- rep("against or in favour of", length.out = length(bf))
  dir <- replace(dir, is.na(bf), NA_character_)
  dir <- replace(dir, bf < 1, "against")
  dir <- replace(dir, bf > 1, "in favour of")
  bf <- exp(abs(log(bf)))

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

  interpretation <- interpret(bf, rules)

  # Format text
  interpretation[] <- paste0(interpretation, " evidence")
  interpretation[orig_bf == 1] <- "no evidence"

  # Add value if asked for
  if (include_value) {
    interpretation[] <-
      paste0(
        interpretation,
        " (",
        insight::format_bf(orig_bf, protect_ratio = protect_ratio, exact = exact),
        ")"
      )
  }

  # Add direction
  interpretation[] <- paste(interpretation[], dir)

  interpretation[is.na(orig_bf)] <- ""

  interpretation
}
