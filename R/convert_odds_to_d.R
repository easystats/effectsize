
# Odds - d ----------------------------------------------------------------



#' @rdname convert-between-effect-sizes
#' @export
odds_to_d <- function(odds, log = FALSE, ...) {
  if (log == FALSE) {
    log_odds <- log(odds)
  } else {
    log_odds <- odds
  }

  log_odds * (sqrt(3) / pi)
}

#' @rdname convert-between-effect-sizes
#' @export
convert_odds_to_d <- odds_to_d

#' @rdname convert-between-effect-sizes
#' @export
logodds_to_d <- function(odds, log = TRUE, ...) odds_to_d(odds, log = log, ...)



#' @rdname convert-between-effect-sizes
#' @export
d_to_odds <- function(d, log = FALSE, ...) {
  if (log == TRUE) {
    d * pi / sqrt(3)
  } else {
    exp(d * pi / sqrt(3))
  }
}

#' @rdname convert-between-effect-sizes
#' @export
convert_d_to_odds <- d_to_odds




# Odds - r ----------------------------------------------------------------

#' @rdname convert-between-effect-sizes
#' @export
odds_to_r <- function(odds, log = FALSE, ...) {
  d_to_r(convert_odds_to_d(odds, log = log))
}

#' @rdname convert-between-effect-sizes
#' @export
convert_odds_to_r <- odds_to_r

#' @rdname convert-between-effect-sizes
#' @export
logodds_to_r <- function(odds, log = TRUE, ...) odds_to_r(odds, log = log, ...)



#' @rdname convert-between-effect-sizes
#' @export
r_to_odds <- function(r, log = FALSE, ...) {
  d_to_odds(convert_r_to_d(r), log = log)
}

#' @rdname convert-between-effect-sizes
#' @export
convert_r_to_odds <- r_to_odds




