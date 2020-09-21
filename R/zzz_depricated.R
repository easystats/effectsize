#' @export
odds_to_d <- function(odds, log = FALSE, ...) {
  .Deprecated("oddsratio_to_d")
  oddsratio_to_d(odds, log = log)
}

#' @export
logodds_to_d <- function(odds, log = TRUE, ...) {
  .Deprecated("logoddsratio_to_d")
  logoddsratio_to_d(odds, log = log, ...)
}

#' @export
d_to_odds <- function(d, log = FALSE, ...) {
  .Deprecated("d_to_oddsratio")
  d_to_oddsratio(d, log = log)
}

#' @export
odds_to_r <- function(odds, log = FALSE, ...) {
  .Deprecated("oddsratio_to_r")
  oddsratio_to_r(odds, log = log)
}


#' @export
logodds_to_r <- function(odds, log = TRUE, ...) {
  .Deprecated("logoddsratio_to_r")
  odds_to_r(odds, log = log, ...)
}

#' @export
r_to_odds <- function(r, log = FALSE, ...) {
  .Deprecated("r_to_oddsratio")
  d_to_odds(r_to_d(r), log = log)
}

#' @export
interpret_odds <- function(odds, rules = "chen2010", log = FALSE) {
  .Deprecated("interpret_oddsratio")
  interpret_oddsratio(odds, rules = rules, log = log)
}






#' @export
convert_odds_to_d <- odds_to_d

#' @export
convert_d_to_odds <- d_to_odds

#' @export
convert_odds_to_r <- odds_to_r

#' @export
convert_r_to_odds <- r_to_odds