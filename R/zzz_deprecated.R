#' Deprecated / Defunct Functions
#'
#' @param ... Arguments to the deprecated function.
#'
#' @details
#' - `interpret_d` is now [`interpret_cohens_d`].
#' - `interpret_g` is now [`interpret_hedges_g`].
#' - `interpret_delta` is now [`interpret_glass_delta`].
#' - `interpret_parameters` for *standardized parameters* was incorrect. Use [`interpret_r`] instead.
#' - `normalized_chi` is now [`fei`].
#' - `chisq_to_normalized` is now [`chisq_to_fei`].
#' - `d_to_cles` and `rb_to_cles` are now one of the available functions for CLES conversion, e.g. [`d_to_U1`].
#'
#' @rdname effectsize_deprecated
#' @name effectsize_deprecated
NULL


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

#' @export
d_to_cles <- function(...) {
  .Defunct(NULL, msg = 'See help("d_to_cles") for the available functions.')
}

#' @export
rb_to_cles <- function(...) {
  .Deprecated("rb_to_p_superiority")
  rb_to_p_superiority(...)
}

#' @export
convert_d_to_common_language <- d_to_cles

#' @export
d_to_common_language <- d_to_cles

#' @export
convert_rb_to_common_language <- rb_to_cles

#' @export
rb_to_common_language <- rb_to_cles
