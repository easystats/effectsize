#' Deprecated / Defunct Functions
#'
#' @param ... Arguments to the deprecated function.
#'
#' @rdname effectsize_deprecated
#' @name effectsize_deprecated
NULL


# July 2025 ---------------------------------------------------------------

.deprecated_df_methods <- function(funname) {
  insight::format_error(
    sprintf("%s(<data.frame>) is deprecated.", funname),
    "You can use:",
    sprintf("datawizard::data_modify(data, .at = ..., .modify = %s)", funname),
    "Or",
    sprintf("dplyr::mutate(data, dplyr::across(..., %s))", funname),
    "Instead."
  )
}

# March 2023 --------------------------------------------------------------

#' @rdname effectsize_deprecated
#' @export
convert_odds_to_probs <- function(...) {
  .Deprecated("odds_to_probs")
  odds_to_probs(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_probs_to_odds <- function(...) {
  .Deprecated("probs_to_odds")
  probs_to_odds(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_d_to_r <- function(...) {
  .Deprecated("d_to_r")
  d_to_r(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_r_to_d <- function(...) {
  .Deprecated("r_to_d")
  r_to_d(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_oddsratio_to_d <- function(...) {
  .Deprecated("oddsratio_to_d")
  oddsratio_to_d(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_d_to_oddsratio <- function(...) {
  .Deprecated("d_to_oddsratio")
  d_to_oddsratio(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_oddsratio_to_r <- function(...) {
  .Deprecated("oddsratio_to_r")
  oddsratio_to_r(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_r_to_oddsratio <- function(...) {
  .Deprecated("r_to_oddsratio")
  r_to_oddsratio(...)
}
