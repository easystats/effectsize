#' Semi-Partial Correlation Squared (Delta R2)
#'
#' Compute the semi-partial correlation squared (also known as the delta
#' R2), for a `lm` model.
#'
#' @details For other, non-lm models, as well as more verbose info / options,
#' please see the documentation for [parameters::dominance_analysis()].
#'
#' @param model An `lm` model.
#' @param ... Arguments passed to `lm`.
#'   these can be `subset` and `na.action`.
#'
#' @return A data frame with the effect size.
#'
#' @family effect size correlation
#'
#' @examples
#' m <- lm(mpg ~ cyl + disp + hp * drat, data = mtcars)
#' semipartialR2(m)
#' deltaR2(m)
#' sr2(m)
#' @export

semipartialR2 <- function(model, ...) {
  data <- insight::get_data(model)
  response <- insight::find_response(model)

  list.parameters0 <- insight::find_parameters(model)$conditional
  list.parameters <- list.parameters0[-1]
  list.parameters.all <- lapply(seq(list.parameters), function(x) {
    list.parameters[-x]
  })

  formulas <- lapply(list.parameters.all, function(x) {
    x <- paste0(x, collapse = " + ")
    paste(response, "~", x)
  })
  list.models <- lapply(formulas, stats::lm, data = data, ...)

  r_total <- performance::r2(model)$R2
  list.R2 <- lapply(list.models, performance::r2, data = data)
  list.sr2 <- lapply(list.R2, function(x) {
    r_total - x$R2
  })

  data.frame(
    Parameter = list.parameters0,
    sr2 = c(NA, unlist(list.sr2))
  )
}

#' @export
#' @rdname semipartialR2
deltaR2 <- semipartialR2

#' @export
#' @rdname semipartialR2
sr2 <- semipartialR2