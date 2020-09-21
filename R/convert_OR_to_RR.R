#' Convert between Odds ratios and Risk ratios
#'
#' @param OR,RR Risk ratio of `p1/p0` or Odds ratio of `odds(p1)/odds(p0)`, possibly log-ed.
#' @param p0 Baseline risk
#' @inheritParams oddsratio_to_d
#'
#' @examples
#' p0 <- 0.4
#' p1 <- 0.7
#'
#' (OR <- probs_to_odds(p1) / probs_to_odds(p0))
#' (RR <- p1 / p0)
#'
#' riskratio_to_oddsratio(RR, p0 = p0)
#' oddsratio_to_riskratio(OR, p0 = p0)
#'
#' @references Grant, R. L. (2014). Converting an odds ratio to a range of plausible relative risks for better communication of research findings. Bmj, 348, f7450.
#' @export
oddsratio_to_riskratio <- function(OR, p0, log = FALSE) {
  if (log) OR <- exp(OR)

  RR <- OR / (1 - p0 + (p0 * OR))

  if (log) RR <- log(RR)
  return(RR)
}



#' @rdname oddsratio_to_riskratio
#' @export
riskratio_to_oddsratio <- function(RR, p0, log = FALSE) {
  if (log) RR <- exp(RR)

  OR <- RR * (1 - p0) / (1 - RR * p0)

  if (log) OR <- log(OR)
  return(OR)
}
