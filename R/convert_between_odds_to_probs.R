#' Convert Between Odds and Probabilities
#'
#' @param odds The *Odds* (or `log(odds)` when `log = TRUE`) to convert.
#' @param probs Probability values to convert.
#' @param log Take in or output log odds (such as in logistic models).
#' @param select When a data frame is passed, character or list of of column
#'   names to be transformed.
#' @param exclude When a data frame is passed, character or list of column names
#'   to be excluded from transformation.
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
#' @aliases convert_odds_to_probs
odds_to_probs <- function(odds, log = FALSE, ...) {
  UseMethod("odds_to_probs")
}

#' @export
convert_odds_to_probs <- odds_to_probs


#' @export
#' @importFrom stats plogis
odds_to_probs.numeric <- function(odds, log = FALSE, ...) {
  if (log) {
    stats::plogis(odds)
  } else {
    stats::plogis(log(odds))
  }
}


#' @rdname odds_to_probs
#' @export
odds_to_probs.data.frame <- function(odds, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(odds = odds, log = log, select = select, exclude = exclude, ...)
}


#' @rdname odds_to_probs
#' @aliases convert_probs_to_odds
#' @export
probs_to_odds <- function(probs, log = FALSE, ...) {
  UseMethod("probs_to_odds")
}

#' @export
convert_probs_to_odds <- probs_to_odds

#' @export
#' @importFrom stats qlogis
probs_to_odds.numeric <- function(probs, log = FALSE, ...) {
  if (log) {
    stats::qlogis(probs)
  } else {
    exp(stats::qlogis(probs))
  }
}

#' @rdname odds_to_probs
#' @export
probs_to_odds.data.frame <- function(probs, log = FALSE, select = NULL, exclude = NULL, ...) {
  .odds_to_probs_df(probs = probs, log = log, select = select, exclude = exclude, ...)
}









# Data frame --------------------------------------------------------------



#' @keywords internal
.odds_to_probs_df <- function(odds = NULL, probs = NULL, log = FALSE, select = NULL, exclude = NULL, ...) {
  # If vector
  if (!is.null(odds)) {
    df <- odds
  } else {
    df <- probs
  }

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
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
    dfnum <- data.frame(lapply(dfnum, odds_to_probs.numeric, log = log))
  } else {
    dfnum <- data.frame(lapply(dfnum, probs_to_odds.numeric, log = log))
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
