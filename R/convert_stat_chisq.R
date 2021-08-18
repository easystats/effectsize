#' Conversion Chi-Squared to Phi or Cramer's V
#'
#' Convert between Chi square (\eqn{\chi^2}), Cramer's V, phi (\eqn{\phi}) and
#' Cohen's *w* for contingency tables or goodness of fit.
#'
#' @param chisq The Chi-squared statistic.
#' @param n Total sample size.
#' @param nrow,ncol The number of rows/columns in the contingency table (ignored
#'   for Phi when `adjust=FALSE` and `CI=NULL`).
#' @param ci Confidence Interval (CI) level
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"greater"` (default) or `"less"`
#'   (one-sided CI), or `"two.sided"` (default, two-sided CI). Partial matching
#'   is allowed (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
#'   [effectsize_CIs].
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size(s) between 0-1, and confidence
#'   interval(s). See [cramers_v()].
#'
#' @details These functions use the following formulae:
#' \cr
#' \deqn{\phi = \sqrt{\chi^2 / n}}{phi = sqrt(\chi^2 / n)}
#' \cr
#' \deqn{Cramer's V = \phi / \sqrt{min(nrow,ncol)-1}}{Cramer's V = \phi / sqrt(min(nrow,ncol)-1)}
#' \cr
#' For adjusted versions, see Bergsma, 2013.
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
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
#' # data:  contingency_table
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
#' @references
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
#' calculation of confidence intervals that are based on central and noncentral
#' distributions. Educational and Psychological Measurement, 61(4), 532-574.
#'
#' - Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' Journal of the Korean Statistical Society, 42(3), 323-328.
#'
#' @export
chisq_to_phi <- function(chisq, n, nrow, ncol, ci = 0.95, alternative = "greater", adjust = FALSE, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
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

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    chisq_ <- phi_to_chisq(res[[1]], n)

    chisqs <- t(mapply(
      .get_ncp_chi,
      chisq_, df, ci.level
    ))

    res$CI_low <-
      chisq_to_phi(chisqs[, 1], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
    res$CI_high <-
      chisq_to_phi(chisqs[, 2], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]

    ci_method <- list(method = "ncp", distribution = "chisq")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  } else {
    alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "adjust") <- adjust
  attr(res, "alternative") <- alternative
  return(res)
}


#' @rdname chisq_to_phi
#' @export
chisq_to_cohens_w <- chisq_to_phi


#' @rdname chisq_to_phi
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol, ci = 0.95, alternative = "greater", adjust = FALSE, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  is_goodness <- ncol == 1 || nrow == 1


  if (is_goodness) {
    df <- pmax(nrow - 1, ncol - 1)
  } else {
    df <- (nrow - 1) * (ncol - 1)
  }

  phi <- chisq_to_phi(chisq, n, nrow, ncol, ci = NULL, adjust = adjust)[[1]]

  if (adjust) {
    k <- nrow - ((nrow - 1)^2) / (n - 1)
    l <- ncol - ((ncol - 1)^2) / (n - 1)

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

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    chisq_ <- phi_to_chisq(phi, n)

    chisqs <- t(mapply(
      .get_ncp_chi,
      chisq_, df, ci.level
    ))

    res$CI_low <-
      chisq_to_cramers_v(chisqs[, 1], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]
    res$CI_high <-
      chisq_to_cramers_v(chisqs[, 2], n, nrow, ncol, ci = NULL, adjust = FALSE)[[1]]

    ci_method <- list(method = "ncp", distribution = "chisq")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  } else {
    alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "adjust") <- adjust
  attr(res, "alternative") <- alternative
  return(res)
}


# Reverse -----------------------------------------------------------------

#' @rdname chisq_to_phi
#' @param phi The Phi statistic.
#' @export
phi_to_chisq <- function(phi, n, ...) {
  n * (phi^2)
}