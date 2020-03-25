#' Conversion between Effect sizes for Contingency Tables (Chi2, Phi, Cramer's V...)
#'
#' Convert between Chi square, (\eqn{chi^2}), phi (\eqn{\phi}) and Cramer's V.
#'
#' @param chisq The Chi2 statistic.
#' @param phi The Phi statistic.
#' @param n Sample size.
#' @param nrow,ncol The number of rows/columns in the contingency table (ignored for Phi when \code{adjust=FALSE} and \code{CI=NULL}).
#' @param ci Confidence Interval (CI) level level
#' @param adjust Should the effect size be bias-corrected? Defaults to \code{FALSE}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence interval(s).
#'
#' @details These functions use the following formulae:
#' \cr
#' \deqn{\phi = \sqrt{\chi^2 / n}}
#' \cr
#' \deqn{Cramer's V = \phi / \sqrt{min(nrow,ncol)-1}}
#' \cr
#' For adjusted versions, see Bergsma, 2013.
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
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
#' chisq_to_phi(41.234,
#'   n = sum(contingency_table),
#'   nrow = nrow(contingency_table),
#'   ncol = ncol(contingency_table)
#' )
#' chisq_to_cramers_v(41.234,
#'   n = sum(contingency_table),
#'   nrow = nrow(contingency_table),
#'   ncol = ncol(contingency_table)
#' )
#'
#' @references
#' \itemize{
#'   \item Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and calculation of confidence intervals that are based on central and noncentral distributions. Educational and Psychological Measurement, 61(4), 532-574.
#'   \item Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T. Journal of the Korean Statistical Society, 42(3), 323-328.
#' }
#'
#' @export
chisq_to_phi <- function(chisq, n, nrow, ncol, ci = 0.95, adjust = FALSE, ...){
  if (adjust) {
    .es <- function(chisq) {
      pmax(0, sqrt(chisq / n) - ((nrow - 1) * (ncol - 1)) / (n - 1))
    }
  } else {
    .es <- function(chisq) {
      sqrt(chisq / n)
    }
  }

  res <- data.frame(phi = .es(chisq))


  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    chisqs <- t(mapply(.get_ncp_chi,
                       chisq, (nrow - 1) * (ncol - 1), ci))

    res$CI_low <- .es(chisqs[,1])
    res$CI_high <- .es(chisqs[,2])
  }

  return(res)
}

#' @rdname chisq_to_phi
#' @export
convert_chisq_to_phi <- chisq_to_phi


#' @rdname chisq_to_phi
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol, ci = 0.95, adjust = FALSE, ...) {
  if (adjust) {
    .es <- function(chisq) {
      phi <- pmax(0, sqrt(chisq / n) - ((nrow - 1) * (ncol - 1)) / (n - 1))
      k <- nrow - ((nrow - 1) ^ 2) / (n - 1)
      l <- ncol - ((ncol - 1) ^ 2) / (n - 1)

      phi / sqrt((pmin(k, l) - 1))
    }
  } else {
    .es <- function(chisq) {
      phi <- sqrt(chisq / n)
      phi / sqrt((pmin(nrow, ncol) - 1))
    }
  }

  res <- data.frame(cramers_v = .es(chisq))


  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    chisqs <- t(mapply(.get_ncp_chi,
                       chisq, (nrow - 1) * (ncol - 1), ci))

    res$CI_low <- .es(chisqs[,1])
    res$CI_high <- .es(chisqs[,2])
  }

  return(res)
}

#' @rdname chisq_to_phi
#' @export
convert_chisq_to_cramers_v <- chisq_to_cramers_v


# Reverse -----------------------------------------------------------------

#' @rdname chisq_to_phi
#' @export
phi_to_chisq <- function(phi, n, ...) {
  (phi * n)^2
}

#' @rdname chisq_to_phi
#' @export
convert_phi_to_chisq <- phi_to_chisq
