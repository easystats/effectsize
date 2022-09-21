#' Convert \eqn{\chi^2} to \eqn{\phi} and Other Correlation-like Effect Sizes
#'
#' Convert between \eqn{\chi^2} (chi-square), \eqn{\phi} (phi), Cramer's \eqn{V},
#' Cohen's \eqn{w}, \ifelse{latex}{\eqn{Fei}}{פ (Fei)} and Pearson's \eqn{C}
#' for contingency tables or goodness of fit.
#'
#' @name chisq_to_phi
#' @rdname convert_chisq
#'
#' @param chisq The \eqn{\chi^2} (chi-square) statistic.
#' @param n Total sample size.
#' @param nrow,ncol The number of rows/columns in the contingency table.
#' @param ci Confidence Interval (CI) level
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"greater"` (default) or `"less"`
#'   (one-sided CI), or `"two.sided"` (default, two-sided CI). Partial matching
#'   is allowed (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
#'   [effectsize_CIs].
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`;
#'   For small samples and large tables, it is advisable to set to `TRUE`.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size(s), and confidence interval(s). See
#'   [cramers_v()].
#'
#' @details These functions use the following formulas:
#'
#' \deqn{\phi = w = \sqrt{\chi^2 / n}}{phi = w = sqrt(\frac{\chi^2}{n})}
#' \ifelse{latex}{
#' \deqn{\textrm{Cramer's } V = \phi / \sqrt{\min(\textit{nrow}, \textit{ncol}) - 1}}
#' }{
#' \deqn{\textrm{Cramer's } V = \phi / \sqrt{\min(\textit{nrow}, \textit{ncol}) - 1}}{Cramer's V = \phi / sqrt(min(nrow, ncol) - 1)}
#' }
#'
#' \ifelse{latex}{
#' \deqn{\textit{Fei} = \phi / \sqrt{[1 / \min(p_E)] - 1}}
#' }{
#' \deqn{פ = \phi / \sqrt{[1 / \min(p_E)] - 1}}{פ = w / sqrt(1 / min(p_E) - 1))}
#' }
#' Where \eqn{p_E} are the expected probabilities.
#'
#' \deqn{\textrm{Pearson's } C = \sqrt{\chi^2 / (\chi^2 + n)}}{Pearson's C = sqrt(\chi^2 / (\chi^2 + n))}
#'
#' For bias-adjusted versions of \eqn{\phi} and \eqn{V}, see [Bergsma, 2013](https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V#Bias_correction).
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @family effect size from test statistic
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
#' chisq_to_cohens_w(41.234,
#'   n = sum(contingency_table),
#'   nrow = nrow(contingency_table),
#'   ncol = ncol(contingency_table)
#' )
#'
#'
#'
#'
#' Smoking_ASD <- as.table(c(ASD = 17, ASP = 11, TD = 640))
#'
#' # chisq.test(Smoking_ASD, p = c(0.015, 0.010, 0.975))
#' #>
#' #> 	Chi-squared test for given probabilities
#' #>
#' #> data:  Smoking_ASD
#' #> X-squared = 7.8521, df = 2, p-value = 0.01972
#'
#' chisq_to_fei(
#'   7.8521,
#'   n = sum(Smoking_ASD),
#'   nrow = 1,
#'   ncol = 3,
#'   p = c(0.015, 0.010, 0.975)
#' )
#'
#' @references
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
#' calculation of confidence intervals that are based on central and noncentral
#' distributions. Educational and Psychological Measurement, 61(4), 532-574.
#'
#' - Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' Journal of the Korean Statistical Society, 42(3), 323-328.
#'
#' - Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
#' effect size for chi-squared and likelihood-ratio goodness-of-fit tests.
#' Perceptual and motor skills, 103(2), 412-414.
#'
#' - Rosenberg, M. S. (2010). A generalized formula for converting chi-square
#' tests to effect sizes for meta-analysis. PloS one, 5(4), e10059.
#'
#' @export
chisq_to_phi <- function(chisq, n, nrow = 2, ncol = 2,
                         adjust = FALSE,
                         ci = 0.95, alternative = "greater",
                         ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  cl <- match.call()
  dont_stop <- eval(cl[["dont_stop"]], envir = parent.frame())
  if (is.null(dont_stop)) dont_stop <- FALSE

  if (!dont_stop && (nrow != 2 || ncol != 2)) {
    stop("Phi is not appropriate for non-2x2 tables.", call. = FALSE)
  }

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

    chisqs <- t(mapply(
      .get_ncp_chi,
      chisq, df, ci.level
    ))

    res$CI_low <-
      chisq_to_phi(chisqs[, 1], n, nrow, ncol,
        ci = NULL, adjust = adjust,
        dont_stop = dont_stop
      )[[1]]
    res$CI_high <-
      chisq_to_phi(chisqs[, 2], n, nrow, ncol,
        ci = NULL, adjust = adjust,
        dont_stop = dont_stop
      )[[1]]

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
  attr(res, "alternative") <- alternative
  return(res)
}


#' @rdname convert_chisq
#' @export
chisq_to_cohens_w <- function(chisq, n, nrow, ncol,
                              ci = 0.95, alternative = "greater",
                              ...) {
  res <- chisq_to_phi(chisq, n, nrow, ncol,
                      adjust = FALSE,
                      ci = ci, alternative = alternative,
                      dont_stop = TRUE)
  colnames(res)[1] <- "Cohens_w"

  max_value <- Inf

  if (!missing(nrow) && !missing(ncol)) {

  }

  if ("CI" %in% colnames(res)) {
    if (ncol == 2 && nrow == 2) {
      max_value <- 1
    } else if (ncol == 1 || nrow == 1) {
      max_value <- Inf # really is sqrt(chisqMax/N)
    } else if (ncol > 2 || nrow > 2) {
      max_value <- sqrt((pmin(ncol, nrow) - 1))
    }

    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- max_value
    }
  }

  return(res)
}

#' @rdname convert_chisq
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol,
                               adjust = FALSE,
                               ci = 0.95, alternative = "greater",
                               ...) {
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

  res <- chisq_to_phi(chisq, n, nrow, ncol,
                      adjust = adjust,
                      ci = ci, alternative = alternative,
                      dont_stop = TRUE)
  res[grepl("^(phi|CI_)", colnames(res))] <- res[grepl("^(phi|CI_)", colnames(res))] / phi_2_V
  colnames(res)[1] <- gsub("phi", "Cramers_v", colnames(res)[1])

  if ("CI" %in% colnames(res)) {
    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  }
  return(res)
}


#' @rdname convert_chisq
#' @export
#' @param p Vector of expected values. See [stats::chisq.test()].
chisq_to_fei <- function(chisq, n, nrow, ncol, p,
                         ci = 0.95, alternative = "greater",
                         ...) {
  if (is.numeric(ci) || (!missing(nrow) && !missing(ncol))) {
    # This means that the user passed ncol/nrow
    if (ncol == 2 && nrow == 2) {
      return(chisq_to_phi(chisq, n, nrow, ncol, ci = ci, alternative = alternative))
    } else if ((nrow != 1 && ncol > 2) || (ncol != 1 && nrow > 2)) {
      return(chisq_to_cramers_v(chisq, n, nrow, ncol, ci = ci, alternative = alternative))
    }
  }

  # if (!1 %in% c(nrow, ncol)) {
  #   stop("Correlation coefficiant is only applicable to goodness of fit tests.", call. = FALSE)
  # }

  # if (is.null(p)) {
  #   p <- rep(1, pmax(nrow, ncol))
  # }

  if (!length(p) %in% c(ncol, nrow)) stop("Length of `p` must match number of rows/columns.", call. = FALSE)
  p <- p / sum(p)

  q <- min(p)
  N <- n * (1 - q) / q

  res <- chisq_to_phi(chisq, N, nrow, ncol,
                      adjust = FALSE,
                      ci = ci, alternative = alternative,
                      dont_stop = TRUE)
  colnames(res)[1] <- "Fei"

  if ("CI" %in% colnames(res)) {
    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  }

  is_uniform <- length(unique(p)) > 1L
  attr(res, "table_footer") <- if (!is_uniform || max(ncol, nrow) > 2) {
    sprintf("Adjusted for %suniform expected probabilities.", if (is_uniform) "non-" else "")
  }
  return(res)
}

#' @rdname convert_chisq
#' @export
chisq_to_pearsons_c <- function(chisq, n, nrow, ncol,
                                ci = 0.95, alternative = "greater",
                                ...) {
  res <- chisq_to_phi(chisq, n, nrow, ncol,
                      adjust = FALSE,
                      ci = ci, alternative = alternative,
                      dont_stop = TRUE)
  res[grepl("^(phi|CI_)", colnames(res))] <- lapply(res[grepl("^(phi|CI_)", colnames(res))], function(phi) sqrt(1 / (1 / phi^2 + 1)))
  colnames(res)[1] <- "Pearsons_c"

  if ("CI" %in% colnames(res)) {
    if ((alternative <- attr(res, "alternative")) == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  }
  return(res)
}


# Reverse -----------------------------------------------------------------

#' @rdname convert_chisq
#' @param phi The \eqn{\phi} (phi) statistic.
#' @export
phi_to_chisq <- function(phi, n, ...) {
  n * (phi^2)
}
