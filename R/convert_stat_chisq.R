#' Convert \eqn{\chi^2} to \eqn{\phi} and Other Correlation-like Effect Sizes
#'
#' Convert between \eqn{\chi^2} (chi-square), \eqn{\phi} (phi), Cramer's
#' \eqn{V}, Tschuprow's \eqn{T}, Cohen's \eqn{w},
#' \ifelse{latex}{\eqn{Fei}}{פ (Fei)} and Pearson's \eqn{C} for contingency
#' tables or goodness of fit.
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
#' @param adjust Should the effect size be bias-corrected? Defaults to `TRUE`;
#'   Advisable for small samples and large tables.
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
#' \deqn{\textrm{Tschuprow's } T = \phi / \sqrt[4]{(\textit{nrow} - 1) \times (\textit{ncol} - 1)}}
#' }{
#' \deqn{\textrm{Tschuprow's } T = \phi / \sqrt[4]{(\textit{nrow} - 1) \times (\textit{ncol} - 1)}}{Tschuprow's T = \phi / sqrt(sqrt((nrow-1) * (ncol-1)))}
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
#' @seealso [phi()] for more details.
#'
#' @examples
#'
#' data("Music_preferences")
#'
#' # chisq.test(Music_preferences)
#' #>
#' #> 	Pearson's Chi-squared test
#' #>
#' #> data:  Music_preferences
#' #> X-squared = 95.508, df = 6, p-value < 2.2e-16
#' #>
#'
#' chisq_to_cohens_w(95.508,
#'   n = sum(Music_preferences),
#'   nrow = nrow(Music_preferences),
#'   ncol = ncol(Music_preferences)
#' )
#'
#'
#'
#'
#' data("Smoking_FASD")
#'
#' # chisq.test(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#' #>
#' #> 	Chi-squared test for given probabilities
#' #>
#' #> data:  Smoking_FASD
#' #> X-squared = 7.8521, df = 2, p-value = 0.01972
#'
#' chisq_to_fei(
#'   7.8521,
#'   n = sum(Smoking_FASD),
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
                         adjust = TRUE,
                         ci = 0.95, alternative = "greater",
                         ...) {
  if ((!missing(nrow) && nrow != 2) || (!missing(ncol) && ncol != 2)) {
    stop("Phi is not appropriate for non-2x2 tables.", call. = FALSE)
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )

  if (adjust) {
    res <- .adjust_phi(res, n, nrow, ncol)
  }

  if ("CI" %in% colnames(res)) {
    if (attr(res, "alternative") == "greater") {
      res$CI_high <- 1
    } else {
      res$CI_high <- pmin(res$CI_high, 1)
    }
  }

  return(res)
}


#' @rdname convert_chisq
#' @export
chisq_to_cohens_w <- function(chisq, n, nrow, ncol,
                              ci = 0.95, alternative = "greater",
                              ...) {
  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )
  colnames(res)[1] <- "Cohens_w"

  if ("CI" %in% colnames(res)) {
    if (ncol > 2 && nrow > 2) {
      max_possible <- sqrt((pmin(ncol, nrow) - 1))
    } else if (ncol == 1 || nrow == 1) {
      max_possible <- Inf # really is chisqMax, but can't compute it without p
    }

    if (attr(res, "alternative") == "greater") {
      res$CI_high <- max_possible
    } else {
      res$CI_high <- pmin(res$CI_high, max_possible)
    }
  }

  return(res)
}

#' @rdname convert_chisq
#' @export
chisq_to_cramers_v <- function(chisq, n, nrow, ncol,
                               adjust = TRUE,
                               ci = 0.95, alternative = "greater",
                               ...) {
  if (nrow == 1 || ncol == 1) {
    stop("Cramer's V not applicable to goodness-of-fit tests.", call. = FALSE)
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )
  # Adjust
  if (adjust) {
    k <- nrow - ((nrow - 1)^2) / (n - 1)
    l <- ncol - ((ncol - 1)^2) / (n - 1)

    res <- .adjust_phi(res, n, nrow, ncol)
  } else {
    k <- nrow
    l <- ncol
  }

  div <- sqrt((pmin(k, l) - 1))

  # Convert
  res[grepl("^(phi|CI_)", colnames(res))] <-
    lapply(res[grepl("^(phi|CI_)", colnames(res))], "/", y = div)
  colnames(res)[1] <- gsub("phi", "Cramers_v", colnames(res)[1])

  if ("CI" %in% colnames(res)) {
    if (attr(res, "alternative") == "greater") {
      res$CI_high <- 1
    } else {
      res$CI_high <- pmin(res$CI_high, 1)
    }
  }
  return(res)
}

#' @rdname convert_chisq
#' @export
chisq_to_tschuprows_t <- function(chisq, n, nrow, ncol,
                                  ci = 0.95, alternative = "greater",
                                  ...) {
  if (nrow == 1 || ncol == 1) {
    stop("Tschuprow's T not applicable to goodness-of-fit tests.", call. = FALSE)
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )

  # Convert
  div <- sqrt(sqrt((nrow - 1) * (ncol - 1)))
  res[grepl("^(phi|CI_)", colnames(res))] <-
    lapply(res[grepl("^(phi|CI_)", colnames(res))], "/", y = div)
  colnames(res)[1] <- "Tschuprows_t"

  if ("CI" %in% colnames(res)) {
    if (attr(res, "alternative") == "greater") {
      res$CI_high <- 1
    } else {
      res$CI_high <- pmin(res$CI_high, 1)
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
  if (!missing(nrow) && !missing(ncol)) {
    if (!1 %in% c(nrow, ncol)) {
      stop("Fei is only applicable to goodness of fit tests.", call. = FALSE)
    }

    if (!length(p) %in% c(ncol, nrow)) {
      stop("Length of `p` must match number of rows/columns.", call. = FALSE)
    }
  }

  p <- p / sum(p)
  q <- min(p)

  N <- n * (1 - q) / q

  res <- .chisq_to_generic_phi(chisq, N, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )
  colnames(res)[1] <- "Fei"

  if ("CI" %in% colnames(res)) {
    if (attr(res, "alternative") == "greater") {
      res$CI_high <- 1
    } else {
      res$CI_high <- pmin(res$CI_high, 1)
    }
  }

  is_uniform <- insight::n_unique(p) > 1L
  if (!is_uniform || max(ncol, nrow) > 2) {
    attr(res, "table_footer") <-
      sprintf("Adjusted for %suniform expected probabilities.", if (is_uniform) "non-" else "")
  }
  return(res)
}

#' @rdname convert_chisq
#' @export
chisq_to_pearsons_c <- function(chisq, n, nrow, ncol,
                                ci = 0.95, alternative = "greater",
                                ...) {
  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )

  to_convert <- grepl("^(phi|CI_)", colnames(res))
  res[to_convert] <- lapply(res[to_convert], function(phi) sqrt(1 / (1 / phi^2 + 1)))
  colnames(res)[1] <- "Pearsons_c"

  if ("CI" %in% colnames(res) && alternative == "greater") {
    res$CI_high <- 1
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


# Utils  ------------------------------------------------------------------

#' @keywords internal
.chisq_to_generic_phi <- function(chisq, den, nrow, ncol,
                                  ci = NULL, alternative = "greater",
                                  ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (is.numeric(ci)) {
    is_goodness <- ncol == 1 || nrow == 1

    if (is_goodness) {
      df <- pmax(nrow - 1, ncol - 1)
    } else {
      df <- (nrow - 1) * (ncol - 1)
    }
  }

  res <- data.frame(phi = sqrt(chisq / den))

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    chisqs <- t(mapply(
      .get_ncp_chi,
      chisq, df, ci.level
    ))

    res$CI_low <- .chisq_to_generic_phi(chisqs[, 1], den, nrow, ncol)[[1]]
    res$CI_high <- .chisq_to_generic_phi(chisqs[, 2], den, nrow, ncol)[[1]]

    ci_method <- list(method = "ncp", distribution = "chisq")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  } else {
    ci_method <- NULL
    alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}

#' @keywords internal
.adjust_phi <- function(res, n, nrow, ncol) {
  to_convert <- grepl("^(phi|CI_)", colnames(res))

  res[to_convert] <- lapply(res[to_convert], function(phi) {
    df <- (nrow - 1) * (ncol - 1)
    E <- df / (n - 1)
    sqrt(pmax(0, phi^2 - E))
  })

  colnames(res)[1] <- "phi_adjusted"

  res
}
