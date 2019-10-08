#' @rdname d_to_r
#' @export
odds_to_probs <- function(odds = 1, log = FALSE, ...) {
  UseMethod("odds_to_probs")
}

#' @export
odds_to_probs.numeric <- function(odds = 1, log = FALSE, ...) {
  .odds_to_probs(odds, log = log)
}

#' @export
odds_to_probs.double <- odds_to_probs.numeric




#' @rdname d_to_r
#' @export
odds_to_probs.data.frame <- function(odds, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(odds = odds, log = log, select = select, exclude = exclude, ...)
}


#' @rdname d_to_r
#' @export
probs_to_odds <- function(probs = 0.5, log = FALSE, ...) {
  UseMethod("probs_to_odds")
}

#' @export
probs_to_odds.numeric <- function(probs = 0.5, log = FALSE, ...) {
  .probs_to_odds(probs, log = log)
}

#' @export
probs_to_odds.double <- probs_to_odds.numeric

#' @export
probs_to_odds.data.frame <- function(probs, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(probs = probs, log = log, select = select, exclude = exclude, ...)
}


#' @rdname d_to_r
#' @export
convert_odds_to_probs <- odds_to_probs

#' @rdname d_to_r
#' @export
convert_probs_to_odds <- probs_to_odds













#' @keywords internal
.odds_to_probs_df <- function(odds = NULL, probs = NULL, log = FALSE, select = NULL, exclude = NULL, ...) {

  # If vector
  if (!is.null(odds)) {
    df <- odds
  } else {
    df <- probs
  }

  # Variable order
  var_order <- names(df)

  # Keep subset
  if (!is.null(select) && select %in% names(df)) {
    to_keep <- as.data.frame(df[!names(df) %in% c(select)])
    df <- df[names(df) %in% c(select)]
  } else {
    to_keep <- NULL
  }

  # Remove exceptions
  if (!is.null(exclude) && exclude %in% names(df)) {
    if (is.null(to_keep)) {
      to_keep <- as.data.frame(df[exclude])
    } else {
      to_keep <- cbind(to_keep, as.data.frame(df[exclude]))
    }

    df <- df[!names(df) %in% c(exclude)]
  }

  # Remove non-numerics
  dfother <- df[!sapply(df, is.numeric, simplify = TRUE)]
  dfnum <- df[sapply(df, is.numeric, simplify = TRUE)]

  # Tranform
  if (!is.null(odds)) {
    dfnum <- .odds_to_probs(dfnum, log = log)
  } else {
    dfnum <- .probs_to_odds(dfnum, log = log)
  }

  # Add non-numerics
  if (is.null(ncol(dfother))) {
    df <- dfnum
  } else {
    df <- cbind(dfother, dfnum)
  }

  # Add exceptions
  if (!is.null(select) | !is.null(exclude) && exists("to_keep")) {
    df <- cbind(df, to_keep)
  }

  # Reorder
  df <- df[var_order]

  return(df)
}


#' @keywords internal
.odds_to_probs <- function(odds, log = TRUE) {
  if (log == TRUE) {
    odds <- exp(odds)
  }
  probs <- odds / (1 + odds)
  return(probs)
}

#' @keywords internal
.probs_to_odds <- function(probs, log = TRUE) {
  odds <- probs / (1 - probs)
  if (log == TRUE) {
    odds <- log(odds)
  }
  return(odds)
}
