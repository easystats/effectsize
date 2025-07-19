#' Convert Between Metrics of Change in Probabilities and Probabilities
#'
#' @inheritParams arr_to_nnt
#' @param odds Should odds be returned instead of probabilities?
#'
#' @return Probabilities (or probability odds).
#'
#' @seealso [oddsratio()], [riskratio()], [arr()], and [nnt()],
#' [odds_to_probs()], and [oddsratio_to_arr()] and others.
#'
#' @examples
#'
#' p0 <- 0.4
#' p1 <- 0.7
#'
#' (OR <- probs_to_odds(p1) / probs_to_odds(p0))
#' (RR <- p1 / p0)
#' (ARR <- p1 - p0)
#' (NNT <- arr_to_nnt(ARR))
#'
#' riskratio_to_probs(RR, p0 = p0)
#' oddsratio_to_probs(OR, p0 = p0)
#'
#' all.equal(nnt_to_probs(NNT, p0 = p0, odds = TRUE),
#'           probs_to_odds(p1))
#'
#' arr_to_probs(-ARR, p0 = p1)
#' nnt_to_probs(-NNT, p0 = p1)
#'
#'
#'
#' # RR |>
#' #   riskratio_to_arr(p0) |>
#' #   arr_to_oddsratio(p0) |>
#' #   oddsratio_to_nnt(p0) |>
#' #   nnt_to_probs(p0)
#'
#' @export
oddsratio_to_probs <- function(OR, p0, log = FALSE, odds = FALSE, ...) {
  ARR <- oddsratio_to_arr(OR, p0, log = log)
  arr_to_probs(ARR, p0, odds = odds)
}

#' @export
#' @rdname oddsratio_to_probs
logoddsratio_to_probs <- function(OR, p0, log = TRUE, odds = FALSE, ...) {
  oddsratio_to_probs(PR, p0, log = log, odds = odds)
}

#' @export
#' @rdname oddsratio_to_probs
riskratio_to_probs <- function(RR, p0, odds = FALSE, ...) {
  ARR <- riskratio_to_arr(RR, p0)
  arr_to_probs(ARR, p0, odds = odds)
}

#' @export
#' @rdname oddsratio_to_probs
arr_to_probs <- function(ARR, p0, odds = FALSE, ...) {
  p1 <- p0 + ARR
  if (odds) {
    return(probs_to_odds(p1))
  }
  p1
}

#' @export
#' @rdname oddsratio_to_probs
nnt_to_probs <- function(NNT, p0, odds = FALSE, ...) {
  ARR <- nnt_to_arr(NNT)
  arr_to_probs(ARR, p0, odds = odds)
}
