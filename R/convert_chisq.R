#' Conversion Chi-Squared to Phi or Cramer's V
#'
#' Convert between Chi square, (\eqn{\chi^2}), Cramer's V, phi (\eqn{\phi}) and
#' Cohen's *w* for contingency tables or goodness of fit.
#'
#' @param chisq The Chi-squared statistic.
#' @param phi The Phi statistic.
#' @param n Sample size.
#' @param nrow,ncol The number of rows/columns in the contingency table (ignored for Phi when `adjust=FALSE` and `CI=NULL`).
#' @param ci Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
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
#' @inheritSection cohens_d Confidence Intervals
#'
#' @section CI Contains Zero:
#' Special care should be taken when interpreting CIs with a lower bound equal
#' to (or small then) 0, and even more care should be taken when the *upper*
#' bound is equal to (or small then) 0 (Steiger, 2004; Morey et al., 2016).
#'
#' @family effect size from test statistic
#'
#' @note Cohen's *w* is equivalent to *Phi*.
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
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and calculation of confidence intervals that are based on central and noncentral distributions. Educational and Psychological Measurement, 61(4), 532-574.
#' - Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T. Journal of the Korean Statistical Society, 42(3), 323-328.
#'
#' @export
chisq_to_phi <- function(chisq, n, nrow, ncol, ci = 0.95, adjust = FALSE, ...){
  if (adjust || is.numeric(ci)) {
    is_goodness <- ncol == 1 || nrow == 1

    if (is_goodness) {
      df <- pmax(nrow - 1, ncol - 1)
    } else {
      df <- (nrow - 1) * (ncol - 1)
    }
  }

  if (adjust) {
    res <- data.frame(
      phi_adjusted = sqrt(pmax(0, (chisq / n) -
                                 (df / (n - 1))))
    )
  } else {
    res <- data.frame(phi = sqrt(chisq / n))
  }


  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    chisq_ <- phi_to_chisq(res[[1]], n)

    chisqs <- t(mapply(.get_ncp_chi,
                       chisq_, df, ci))

    res$CI_low <-
      chisq_to_phi(chisqs[, 1], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
    res$CI_high <-
      chisq_to_phi(chisqs[, 2], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}


#' @rdname chisq_to_phi
#' @export
chisq_to_cohens_w <- chisq_to_phi


#' @rdname chisq_to_phi
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol, ci = 0.95, adjust = FALSE, ...) {
  is_goodness <- ncol == 1 || nrow == 1


  if (is_goodness) {
    df <- pmax(nrow - 1, ncol - 1)
  } else {
    df <- (nrow - 1) * (ncol - 1)
  }

  phi <- chisq_to_phi(chisq, n, nrow, ncol, ci = NULL, adjust = adjust)[[1]]

  if (adjust) {
    k <- nrow - ((nrow - 1) ^ 2) / (n - 1)
    l <- ncol - ((ncol - 1) ^ 2) / (n - 1)

    if (is_goodness) {
      V <- phi / sqrt((pmax(k, l) - 1))
    } else {
      V <- phi / sqrt((pmin(k, l) - 1))
    }

    res <- data.frame(Cramers_v_adjusted = V)
  } else {

    if (is_goodness) {
      V <- phi / sqrt((pmax(nrow, ncol) - 1))
    } else {
      V <- phi / sqrt((pmin(nrow, ncol) - 1))
    }

    res <- data.frame(Cramers_v = V)
  }


  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    chisq_ <- phi_to_chisq(phi, n)

    chisqs <- t(mapply(.get_ncp_chi,
                       chisq_, df, ci))

    res$CI_low <-
      chisq_to_cramers_v(chisqs[, 1], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
    res$CI_high <-
      chisq_to_cramers_v(chisqs[, 2], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}


# Reverse -----------------------------------------------------------------

#' @rdname chisq_to_phi
#' @export
phi_to_chisq <- function(phi, n, ...) {
  n * (phi ^ 2)
}

