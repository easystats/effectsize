#' @rdname print.effectsize_table
#' @export
#' @importFrom insight check_if_installed
plot.effectsize_table <- function(x, ...) {
  insight::check_if_installed("see", reason = "for plotting")
  NextMethod()
}

#' @rdname print.effectsize_table
#' @export
#' @importFrom insight check_if_installed
plot.equivalence_test_effectsize <- function(x, ...) {
  insight::check_if_installed("see", reason = "for plotting")
  NextMethod()
}
