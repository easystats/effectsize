#' Conversion Chi-Squared to Phi or Cramer's V
#'
#' Convert between Chi square (\eqn{\chi^2}), Cramer's V, phi (\eqn{\phi}),
#' Cohen's *w* and Pearson's *C* for contingency tables or goodness of fit.
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
#' @return A data frame with the effect size(s), and confidence interval(s). See
#'   [cramers_v()].
#'
#' @details These functions use the following formulae:
#' \cr
#' \deqn{\phi = \sqrt{\chi^2 / n}}{phi = sqrt(\chi^2 / n)}
#' \cr
#' \deqn{Cramer's V = \phi / \sqrt{min(nrow,ncol)-1}}{Cramer's V = \phi / sqrt(min(nrow,ncol)-1)}
#' \cr
#' \deqn{Pearson's C = \sqrt{\chi^2 / (\chi^2 + n)}}{Pearson's C = sqrt(\chi^2 / (\chi^2 + n))}
#' \cr\cr
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
#' # chisq.test(contingency_table)
#' #>
#' #>         Pearson's Chi-squared test
#' #>
#' #> data:  contingency_table
#' #> X-squared = 41.234, df = 4, p-value = 2.405e-08
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
#' chisq_to_pearsons_c(41.234,
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
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
  if (adjust || is.numeric(ci)) {
    is_goodness <- ncol == 1 || nrow == 1

    if (is_goodness) {
      df <- pmax(nrow - 1, ncol - 1)
      max_upper <- Inf
    } else {
      df <- (nrow - 1) * (ncol - 1)
      max_upper <- sqrt((pmin(nrow, ncol) - 1))
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

    chisqs <- t(mapply(
      .get_ncp_chi,
      chisq, df, ci.level
    ))

    res$CI_low <-
      chisq_to_phi(chisqs[, 1], n, nrow, ncol, ci = NULL, adjust = adjust)[[1]]
    res$CI_high <-
      chisq_to_phi(chisqs[, 2], n, nrow, ncol, ci = NULL, adjust = adjust)[[1]]

    ci_method <- list(method = "ncp", distribution = "chisq")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- max_upper
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
  if (ncol == 1 || nrow == 1) {
    stop("Cramer's V not applicable to goodness-of-fit tests.", call. = FALSE)
  }

  if (adjust) {
    k <- nrow - ((nrow - 1)^2) / (n - 1)
    l <- ncol - ((ncol - 1)^2) / (n - 1)
  } else {
    k <- nrow
    l <- ncol
  }

  phi_2_V <- sqrt((pmin(k, l) - 1))

  res <- chisq_to_phi(chisq, n, nrow, ncol, ci = ci, alternative = alternative, adjust = adjust)
  res[grepl("^(phi|CI_)", colnames(res))] <- res[grepl("^(phi|CI_)", colnames(res))] / phi_2_V
  colnames(res)[1] <- gsub("phi", "Cramers_v", colnames(res)[1])

  if ("CI" %in% colnames(res))
    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  return(res)
}


#' @rdname chisq_to_phi
#' @export
chisq_to_pearsons_c <- function(chisq, n, nrow, ncol, ci = 0.95, alternative = "greater", ...) {

  res <- chisq_to_phi(chisq, n, nrow, ncol, ci = ci, alternative = alternative, adjust = FALSE)
  res[grepl("^(phi|CI_)", colnames(res))] <- lapply(res[grepl("^(phi|CI_)", colnames(res))], function(phi) sqrt(1/(1/phi^2 + 1)))
  colnames(res)[1] <- "Pearsons_c"

  if ("CI" %in% colnames(res))
    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  return(res)
}


# Reverse -----------------------------------------------------------------

#' @rdname chisq_to_phi
#' @param phi The Phi statistic.
#' @export
phi_to_chisq <- function(phi, n, ...) {
  n * (phi^2)
}
