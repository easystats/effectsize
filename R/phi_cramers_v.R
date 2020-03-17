#' Effect size for contingency tables
#'
#' Compute Cramer's V and phi (\eqn{\phi}) from contingency tables.
#'
#' @param x a numeric vector or matrix. x and y can also both be factors.
#' @param y a numeric vector; ignored if x is a matrix. If x is a factor, y should be a factor of the same length.
#' @param ... Passed to \code{chisq.test()}.
#'
#' @return A numeric value between 0-1.
#'
#' @details These functions use the following formulae:
#' \cr\cr
#' \deqn{Cramer's V = \sqrt{\chi^2 / (n * (min(nrow,ncol)-1))}}
#' \cr\cr
#' \deqn{\phi = \sqrt{\chi^2 / n}}
#'
#' @examples
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#'
#' phi(contingency_table)
#'
#' cramers_v(contingency_table)
#'
#' @export
phi <- function(x, y = NULL, ...){
  res <- chisq.test(x, y, ...)

  chisq_to_phi(unname(res$statistic),
               n = sum(x))
}

#' @rdname phi
#' @export
cramers_v <- function(x, y = NULL, ...){
  res <- chisq.test(x, y, ...)

  chisq_to_cramers_v(unname(res$statistic),
                     n = sum(x),
                     nrow = nrow(res$observed),
                     ncol = nrow(res$observed))
}