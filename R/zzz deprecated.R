#' @export
interpret_d <- function(...) {
  .Deprecated("interpret_cohens_d")
  interpret_cohens_d(...)
}

#' @export
interpret_g <- function(...) {
  .Deprecated("interpret_hedges_g")
  interpret_hedges_g(...)
}

#' @export
interpret_delta <- function(...) {
  .Deprecated("interpret_glass_delta")
  interpret_glass_delta(...)
}