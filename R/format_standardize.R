#' Transform a standardized vector into character
#'
#' Transform a standardized vector into character, e.g., `c("-1 SD", "Mean", "+1 SD")`.
#'
#' @param x A standardized numeric vector.
#' @param reference The reference vector from which to compute the mean and SD.
#' @inheritParams standardize
#' @inheritParams insight::format_value
#'
#' @examples
#' format_standardize(c(-1, 0, 1))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE)
#' @importFrom stats median mad sd
#' @importFrom insight format_value
#' @export
format_standardize <- function(x, reference = x, robust = FALSE, digits = NULL, ...) {
  if (robust) {
    central <- stats::median(reference, na.rm = TRUE)
    central_name <- "Median"
    deviation <- stats::mad(reference, na.rm = TRUE)
    deviation_name <- "MAD"
  } else {
    central <- mean(reference, na.rm = TRUE)
    central_name <- "Mean"
    deviation <- stats::sd(reference, na.rm = TRUE)
    deviation_name <- "SD"
  }

  # Express in deviations
  x <- (x - central) / deviation

  # Round
  if (is.null(digits)) {
    L <- insight::format_value(x, round(1 / diff(range(x, na.rm = TRUE))), protect_integers = TRUE)
  } else {
    L <- insight::format_value(x, digits = digits, protect_integers = TRUE)
  }

  # Complete
  L[!grepl("-", L)] <- paste0("+", L[!grepl("-", L)])
  L <- paste(L, deviation_name)
  is_central <- sapply(x, function(.) isTRUE(all.equal(., 0)))
  L[is_central] <- central_name

  factor(L, levels = rev(unique(L)))
}
