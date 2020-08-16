#' Effect size for contingency tables
#'
#' Compute Cramer's V and phi (\eqn{\phi}) from contingency tables.
#'
#' @param x a numeric vector or matrix. x and y can also both be factors.
#' @param y a numeric vector; ignored if x is a matrix. If x is a factor, y should be a factor of the same length.
#' @param ci Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param CI Deprecated in favor of `ci`.
#' @param ... Ignored.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence interval(s).
#'
#' @seealso [chisq_to_phi()] for details regarding estimation and CIs.
#'
#' @examples
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#'
#' phi(contingency_table)
#'
#' cramers_v(contingency_table)
#'
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI, ...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- stats::chisq.test(x, y)
  Obs <- res$observed
  Exp <- res$expected

  chisq_to_phi(chisq = .chisq(Obs, Exp),
               n = sum(Obs),
               nrow = nrow(Obs),
               ncol = ncol(Obs),
               ci = ci,
               adjust = adjust)
}

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cramers_v <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI,...){
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  res <- stats::chisq.test(x, y)
  Obs <- res$observed
  Exp <- res$expected

  chisq_to_cramers_v(chisq = .chisq(Obs, Exp),
                     n = sum(Obs),
                     nrow = nrow(Obs),
                     ncol = ncol(Obs),
                     ci = ci,
                     adjust = adjust)
}


#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp) ^ 2) / Exp)
}
