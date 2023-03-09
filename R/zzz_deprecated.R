#' Deprecated / Defunct Functions
#'
#' @param ... Arguments to the deprecated function.
#'
#' @rdname effectsize_deprecated
#' @name effectsize_deprecated
NULL


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

# Older -------------------------------------------------------------------

#' @rdname effectsize_deprecated
#' @export
interpret_d <- function(...) {
  .Deprecated("interpret_cohens_d")
  interpret_cohens_d(...)
}

#' @rdname effectsize_deprecated
#' @export
interpret_g <- function(...) {
  .Deprecated("interpret_hedges_g")
  interpret_hedges_g(...)
}

#' @rdname effectsize_deprecated
#' @export
interpret_delta <- function(...) {
  .Deprecated("interpret_glass_delta")
  interpret_glass_delta(...)
}

#' @rdname effectsize_deprecated
#' @export
interpret_parameters <- function(...) {
  .Deprecated("interpret_r")
  interpret_r(...)
}

#' @rdname effectsize_deprecated
#' @export
normalized_chi <- function(...) {
  .Deprecated("fei")
  fei(...)
}

#' @rdname effectsize_deprecated
#' @export
chisq_to_normalized <- function(...) {
  .Deprecated("chisq_to_fei")
  chisq_to_fei(...)
}

#' @rdname effectsize_deprecated
#' @export
d_to_cles <- function(...) {
  .Defunct(NULL, msg = 'See help("d_to_cles") for the available functions.')
}

#' @rdname effectsize_deprecated
#' @export
rb_to_cles <- function(...) {
  .Deprecated("rb_to_p_superiority")
  rb_to_p_superiority(...)
}

#' @rdname effectsize_deprecated
#' @export
convert_d_to_common_language <- d_to_cles

#' @rdname effectsize_deprecated
#' @export
d_to_common_language <- d_to_cles

#' @rdname effectsize_deprecated
#' @export
convert_rb_to_common_language <- rb_to_cles

#' @rdname effectsize_deprecated
#' @export
rb_to_common_language <- rb_to_cles

#' @export
cles <- function(...) {
  .Defunct(NULL, msg = 'See help("p_superiority") for the available functions.')
}

#' @rdname effectsize_deprecated
#' @export
common_language <- cles
