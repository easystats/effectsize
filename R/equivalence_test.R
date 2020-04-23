#' @importFrom bayestestR equivalence_test
#' @export
bayestestR::equivalence_test

#' @title Equivalence test
#'
#' @description Compute the equivalence test for frequentist models.
#'
#' @param x An effect size table, such as returned by \code{cohens_d, eta_squared, F_to_r}, etc.
#' @param range The range of practical equivalence of an effect. If a single value is provided,
#'   the test is done against \code{c(-range, range)}. For effect sizes that cannot be negative,
#'   the lower bound is set to 0. If \code{"default"}, will be set to \code{[-.1, .1]}.
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso For more details, see \code{\link[bayestestR:equivalence_test]{equivalence_test}}.
#'
#' @return A data frame.
#'
#' @examples
#'
#' ds <- t_to_d(t = c(0.45, -0.65, -2.2, 2.25),
#'              df_error = c(675, 525, 900, 1875))
#' (equi <- equivalence_test(ds, range = 0.2))
#' if (require(see)) plot(equi)
#'
#'
#' model <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
#' es <- eta_squared(model)
#' (equi <- equivalence_test(es, range = 0.15))
#' if (require(see)) plot(equi)
#'
#' @export
equivalence_test.effectsize_table <- function(x, range = "default", ...) {

  if (!all(c("CI", "CI_low", "CI_high") %in% colnames(x))) {
    stop("CI values missing from effect size table.", call. = FALSE)
  }

  if (range == "default") {
    range <- c(-0.1, 0.1)
  }
  range <- sort(range)

  if (any(colnames(x) %in% es_names$onetail)) {
    if (all(range < 0)) {
      stop("'range' is completely negative. For this effect size, only positive values can be tested.", call. = FALSE)
    } else if (length(range) > 1) {
      if (all(range > 0)) {
        warning("'range' is completely positive. Using only the smallest value as a cutoff.")
        range <- range[1]
      } else {
        range <- range[2]
      }
    }

    signif <- x$CI_low > 0
    in_rope <- x$CI_high < range

    range <- c(0,range)

  } else {
    if (length(range) == 1) {
      range <- c(-range,range)
    }
    range <- sort(range)

    signif <- x$CI_high < 0 | 0 < x$CI_low
    in_rope <- range[1] < x$CI_low & x$CI_high < range[2]
  }

  x$ROPE_Equivalence <- "Undecided"
  x$ROPE_Equivalence[in_rope] <- "Accepted"
  x$ROPE_Equivalence[signif & !in_rope] <- "Rejected"

  attr(x, "rope") <- range
  class(x) <- c("equivalence_test_effectsize", "see_equivalence_test_effectsize", "data.frame")
  return(x)
}
