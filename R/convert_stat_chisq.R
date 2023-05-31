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
#' @param adjust Should the effect size be corrected for small-sample bias?
#'   Defaults to `TRUE`; Advisable for small samples and large tables.
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
#' For versions adjusted for small-sample bias of \eqn{\phi}, \eqn{V}, and \eqn{T}, see [Bergsma, 2013](https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V#Bias_correction).
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
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
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
#' calculation of confidence intervals that are based on central and noncentral
#' distributions. Educational and Psychological Measurement, 61(4), 532-574.
#' - Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M., Lüdecke, D.
#' (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical Data That Use the
#' Chi‑Squared Statistic. Mathematics, 11, 1982. \doi{10.3390/math11091982}
#' - Bergsma, W. (2013). A bias-correction for Cramer's V and Tschuprow's T.
#' Journal of the Korean Statistical Society, 42(3), 323-328.
#' - Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
#' effect size for chi-squared and likelihood-ratio goodness-of-fit tests.
#' Perceptual and motor skills, 103(2), 412-414.
#' - Rosenberg, M. S. (2010). A generalized formula for converting chi-square
#' tests to effect sizes for meta-analysis. PloS one, 5(4), e10059.
#'
#' @export
chisq_to_phi <- function(chisq, n, nrow = 2, ncol = 2,
                         adjust = TRUE,
                         ci = 0.95, alternative = "greater",
                         ...) {
  if ((!missing(nrow) && nrow != 2) || (!missing(ncol) && ncol != 2)) {
    insight::format_error("Phi is not appropriate for non-2x2 tables.")
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    adjust = adjust,
    ci = ci, alternative = alternative,
    ...
  )

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
chisq_to_cohens_w <- function(chisq, n, nrow, ncol, p,
                              ci = 0.95, alternative = "greater",
                              ...) {
  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )
  colnames(res)[1] <- "Cohens_w"

  if ("CI" %in% colnames(res)) {
    if (ncol == 1 || nrow == 1) {
      if (missing(p)) {
        max_possible <- Inf # really is chisqMax, but can't compute it without p
      } else {
        p <- p / sum(p)
        q <- min(p)
        max_possible <- sqrt((1 / q) - 1)
      }
    } else {
      max_possible <- sqrt((pmin(ncol, nrow) - 1))
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
    insight::format_error("Cramer's V not applicable to goodness-of-fit tests.")
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    adjust = adjust,
    ci = ci, alternative = alternative,
    ...
  )

  # Convert
  kl <- .possibly_adjust_k_and_l(nrow, ncol, n, adjust = adjust)
  to_convert <- grepl("^(phi|CI_)", colnames(res))
  res[to_convert] <- lapply(res[to_convert], w_to_v, nrow = kl[["k"]], ncol = kl[["l"]])
  colnames(res)[1] <- gsub("phi", "Cramers_v", colnames(res)[1], fixed = TRUE)

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
                                  adjust = TRUE,
                                  ci = 0.95, alternative = "greater",
                                  ...) {
  if (nrow == 1 || ncol == 1) {
    insight::format_error("Tschuprow's T not applicable to goodness-of-fit tests.")
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    adjust = adjust,
    ci = ci, alternative = alternative,
    ...
  )

  # Convert
  kl <- .possibly_adjust_k_and_l(nrow, ncol, n, adjust = adjust)
  to_convert <- grepl("^(phi|CI_)", colnames(res))
  res[to_convert] <- lapply(res[to_convert], w_to_t, nrow = kl[["k"]], ncol = kl[["l"]])
  colnames(res)[1] <- gsub("phi", "Tschuprows_t", colnames(res)[1], fixed = TRUE)

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
      insight::format_error("Fei is only applicable to goodness of fit tests.")
    }

    if (!length(p) %in% c(ncol, nrow)) {
      insight::format_error("Length of `p` must match number of rows/columns.")
    }
  }

  res <- .chisq_to_generic_phi(chisq, n, nrow, ncol,
    ci = ci, alternative = alternative,
    ...
  )

  # Convert
  p <- p / sum(p)
  to_convert <- grepl("^(phi|CI_)", colnames(res))
  res[to_convert] <- lapply(res[to_convert], w_to_fei, p = p)
  colnames(res)[1] <- "Fei"

  if ("CI" %in% colnames(res)) {
    if (attr(res, "alternative") == "greater") {
      res$CI_high <- 1
    } else {
      res$CI_high <- pmin(res$CI_high, 1)
    }
  }

  is_uniform <- insight::n_unique(p) == 1L
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
  res[to_convert] <- lapply(res[to_convert], w_to_c)
  colnames(res)[1] <- "Pearsons_c"

  if ("CI" %in% colnames(res)) {
    res <- .limit_ci(res, alternative, 0, 1)
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
.chisq_to_generic_phi <- function(chisq, n, nrow, ncol,
                                  adjust = FALSE,
                                  ci = NULL, alternative = "greater",
                                  ...) {
  alternative <- .match.alt(alternative, FALSE)

  ci_numeric <- .test_ci(ci)
  if (ci_numeric || adjust) {
    is_goodness <- ncol == 1 || nrow == 1

    if (is_goodness) {
      df <- pmax(nrow - 1, ncol - 1)
    } else {
      df <- (nrow - 1) * (ncol - 1)
    }
  }

  res <- data.frame(phi = sqrt(chisq / n))

  if (ci_numeric) {
    res$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    chisqs <- vapply(chisq,
      .get_ncp_chi,
      FUN.VALUE = numeric(2),
      df = df,
      conf.level = ci.level
    )

    res$CI_low <- .chisq_to_generic_phi(chisqs[1, ], n, nrow, ncol)[[1]]
    res$CI_high <- .chisq_to_generic_phi(chisqs[2, ], n, nrow, ncol)[[1]]

    ci_method <- list(method = "ncp", distribution = "chisq")
    res <- .limit_ci(res, alternative, 0, 1)
  } else {
    ci_method <- alternative <- NULL
  }

  if (adjust) {
    to_convert <- grepl("^(phi|CI_)", colnames(res))
    E <- df / (n - 1)

    res[to_convert] <- lapply(res[to_convert], function(phi) {
      sqrt(pmax(0, phi^2 - E))
    })

    colnames(res)[1] <- "phi_adjusted"
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}

#' @keywords internal
.possibly_adjust_k_and_l <- function(nrow, ncol, n, adjust = FALSE) {
  k <- nrow
  l <- ncol

  if (adjust) {
    k <- nrow - ((nrow - 1)^2) / (n - 1)
    l <- ncol - ((ncol - 1)^2) / (n - 1)
  }

  return(list(k = k, l = l))
}
