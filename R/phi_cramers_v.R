#' Effect size for contingency tables
#'
#' Compute Cramer's V and phi (\eqn{\phi}) from contingency tables.
#'
#' @param x a numeric vector or matrix. x and y can also both be factors.
#' @param y a numeric vector; ignored if x is a matrix. If x is a factor, y should be a factor of the same length.
#' @param CI Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to \code{FALSE}.
#' @param ... Passed to \code{chisq.test()}.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence interval(s).
#'
#' @details These functions use the following formulae:
#' \cr\cr
#' \deqn{Cramer's V = \sqrt{\chi^2 / (n * (min(nrow,ncol)-1))}}
#' \cr\cr
#' \deqn{\phi = \sqrt{\chi^2 / n}}
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods search for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' @examples
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#'
#' phi(contingency_table)
#'
#' cramers_v(contingency_table)
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, CI = 0.95, adjust = FALSE, ...){
  res <- stats::chisq.test(x, y, ...)

  chisq_to_phi(unname(res$statistic),
               n = sum(res$observed),
               nrow = nrow(res$observed),
               ncol = ncol(res$observed),
               CI = CI,
               adjust = adjust)
}

#' @rdname phi
#' @export
cramers_v <- function(x, y = NULL, CI = 0.95, adjust = FALSE, ...){
  res <- stats::chisq.test(x, y, ...)

  chisq_to_cramers_v(unname(res$statistic),
                     n = sum(res$observed),
                     nrow = nrow(res$observed),
                     ncol = ncol(res$observed),
                     CI = CI,
                     adjust = adjust)
}