#' Transform a standardized vector into character
#'
#' Transform a standardized vector into character, e.g., `c("-1 SD", "Mean", "+1 SD")`.
#'
#' @param x A standardized numeric vector.
#' @param reference The reference vector from which to compute the mean and SD.
#' @inheritParams datawizard::standardize.default
#' @inheritParams insight::format_value
#' @param ... Other arguments to pass to \code{\link[insight:format_value]{insight::format_value()}} such as \code{digits}, etc.
#'
#' @examples
#' format_standardize(c(-1, 0, 1))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000))
#' format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE)
#'
#' format_standardize(standardize(mtcars$wt), digits = 1)
#' format_standardize(standardize(mtcars$wt, robust = TRUE), digits = 1)
#' @importFrom stats median mad sd
#' @importFrom insight format_value
#' @export
format_standardize <- function(x, reference = x, robust = FALSE, digits = 1, protect_integers = TRUE, ...) {
  # Check if robust info stored in attributes
  if ("robust" %in% names(attributes(reference))) {
    robust <- attributes(reference)$robust
  }

  # Find parameters and their names
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

  # See if they are not stored as attributes
  if ("center" %in% names(attributes(reference))) {
    central <- attributes(reference)$center
  }
  if ("scale" %in% names(attributes(reference))) {
    deviation <- attributes(reference)$scale
  }


  # Express in deviations
  if (length(x) != length(reference) || any(x != reference)) {
    x <- (x - central) / deviation
  }

  # Round
  x <- round(x, digits = digits)


  # Format vector as character
  L <- insight::format_value(x, digits = digits, ...)

  # Complete
  L[!grepl("-", L)] <- paste0("+", L[!grepl("-", L)])
  L <- paste(L, deviation_name)
  L[x == 0] <- central_name

  # Order
  idx <- L[order(x, decreasing = TRUE)]
  factor(L, levels = unique(idx))
}
