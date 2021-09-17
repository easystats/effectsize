#' Deprecated functions
#'
#' @param ... Arguments to the deprecated function.
#'
#' @details
#' - `interpret_d` is now [`interpret_cohens_d`].
#' - `interpret_g` is now [`interpret_hedges_d`].
#' - `interpret_delta` is now [`interpret_glass_delta`].
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