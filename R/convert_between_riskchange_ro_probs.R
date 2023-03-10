#' Convert Between Metrics of Change in Probabilities and Probabilities
#'
#'
#' @param OR Odds ratio `odds(p1)/odds(p0)`
#' @param p0,Odds0 Base rate or odds. Exactly one must be specified.
#' @param log If `TRUE`, inputs `OR` and `Odds0` are taken to be `logOR` and
#'   `logOdds0`, and outputted odds are `logOdds1`.
#' @param ... Ignored
#'
#' @export
oddsratio_to_odds <- function(OR, p0, Odds0, log = FALSE, ...) {
  if ((missing(p0) && missing(Odds0)) || (!missing(p0) && !missing(Odds0))) {
    insight::format_error("Exactly one of `p0` or `Odds0` must be supplied.")
  }

  if (missing(Odds0)) {
    Odds0 <- probs_to_odds(Odds0, log = TRUE)
  } else if (!log) {
    Odds0 <- log(Odds0)
  }

  Odds1 <- OR + Odds0

  if (!log) Odds1 <- exp(Odds1)

  return(Odds1)
}

#' @export
#' @rdname oddsratio_to_odds
oddsratio_to_probs <- function(OR, p0, Odds0, log = FALSE, ...) {
  Odds1 <- oddsratio_to_odds(OR, p0, Odds0, log = log)
  if (log) Odds1 <- exp(Odds1)
  odds_to_probs(Odds1)
}


# LogOR -------------------------------------------------------------------


# RR ----------------------------------------------------------------------


# ARR ---------------------------------------------------------------------


# NNT ---------------------------------------------------------------------
