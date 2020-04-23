#' @export
plot.effectsize_table <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}

#' @export
plot.equivalence_test_effectsize <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


