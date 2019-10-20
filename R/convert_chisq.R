#' Conversion between Effect sizes for Contingency Tables (Chi2, Phi, Cramer's V...)
#'
#' Convert between Chi square, (\eqn{chi^2}), phi (\eqn{\phi}) and Cramer's V.
#'
#' @param chisq The Chi2 statistic.
#' @param phi The Phi statistic.
#' @param n Sample size.
#' @param nrow The number of rows in the contingency table.
#' @param ncol The number of columns in the contingency tables.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A numeric integer between 0-1.
#'
#' @examples
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#'
#' chisq.test(contingency_table)
#' #
#' #         Pearson's Chi-squared test
#' #
#' # data:  ctab
#' # X-squared = 41.234, df = 4, p-value = 2.405e-08
#'
#' chisq_to_phi(30.07, n = sum(contingency_table))
#' chisq_to_cramers_v(30.07,
#'   n = sum(contingency_table),
#'   nrow = nrow(contingency_table),
#'   ncol = ncol(contingency_table)
#' )
#' @export
chisq_to_phi <- function(chisq, n, ...) {
  sqrt(chisq / n)
}

#' @rdname chisq_to_phi
#' @export
convert_chisq_to_phi <- chisq_to_phi

#' @rdname chisq_to_phi
#' @export
phi_to_chisq <- function(phi, n, ...) {
  (phi * n)^2
}

#' @rdname chisq_to_phi
#' @export
convert_phi_to_chisq <- phi_to_chisq





#' @rdname chisq_to_phi
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol, ...) {
  chisq_to_phi(chisq, n) / sqrt((min(nrow, ncol) - 1))
}

#' @rdname chisq_to_phi
#' @export
convert_chisq_to_cramers_v <- chisq_to_cramers_v

# We lack the reverse
