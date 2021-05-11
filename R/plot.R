#' @rdname print.effectsize_table
#' @export
plot.effectsize_table <- function(x, ...) {
  insight::check_if_installed("see", reason = "for plotting")
  NextMethod()
}

#' @export
plot.equivalence_test_effectsize <- function(x, ...) {
  insight::check_if_installed("see", reason = "for plotting")
  NextMethod()
}
