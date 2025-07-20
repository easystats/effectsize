#' Convert Between Odds and Probabilities
#'
#' @param odds The *Odds* (or `log(odds)` when `log = TRUE`) to convert.
#' @param probs Probability values to convert.
#' @param log Take in or output log odds (such as in logistic models).
#' @param ... Arguments passed to or from other methods.
#'
#' @return Converted index.
#'
#' @seealso [stats::plogis()]
#' @family convert between effect sizes
#'
#' @examples
#' odds_to_probs(3)
#' odds_to_probs(1.09, log = TRUE)
#'
#' probs_to_odds(0.95)
#' probs_to_odds(0.95, log = TRUE)
#' @export
odds_to_probs <- function(odds, log = FALSE, ...) {
  if (is.data.frame(odds)) {
    .deprecated_df_methods("odds_to_probs")
  }

  if (log) {
    stats::plogis(odds)
  } else {
    stats::plogis(log(odds))
  }
}

#' @rdname odds_to_probs
#' @export
probs_to_odds <- function(probs, log = FALSE, ...) {
  if (is.data.frame(probs)) {
    .deprecated_df_methods("probs_to_odds")
  }

  if (log) {
    stats::qlogis(probs)
  } else {
    exp(stats::qlogis(probs))
  }
}
