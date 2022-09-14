#' Correlation effect size for contingency tables
#'
#' Compute Cramer's *V*, phi (\eqn{\phi}), Cohen's *w*,
#' \ifelse{latex}{\eqn{Fei}}{פ (Fei)}, Pearson's contingency coefficient for
#' contingency tables or goodness-of-fit. See details.
#'
#' @inheritParams stats::chisq.test
#' @param ci Confidence Interval (CI) level
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"greater"` (one-sided CI; default),
#'   `"two.sided"` (two-sided CI) or `"less"` (one-sided CI). Partial matching
#'   is allowed (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
#'   [effectsize_CIs].
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param ... For goodness-of-fit effect sizes, can pass `rescale.p` (see
#'   [stats::chisq.test()]). Else, ignored.
#'
#' @details
#'
#' Cramer's *V*, phi (\eqn{\phi}), Cohen's *w*, and Pearson's *C* are effect
#' sizes for tests of independence in 2D contingency tables. For 2-by-2 tables,
#' Cramer's *V*, phi and Cohen's *w* are identical, and are equal to the simple
#' correlation between two dichotomous variables, ranging between  0 (no
#' dependence) and 1 (perfect dependence). For larger tables, Cramer's *V* or
#' Pearson's *C* should be used, as they are bounded between 0-1. Cohen's *w*
#' can also be used, but since it is not bounded at 1 (can be larger) its
#' interpretation is more difficult.
#' \cr \cr
#' For goodness-of-fit in 1D tables Cohen's *W*, \ifelse{latex}{\eqn{Fei}}{פ (Fei)}
#' or Pearson's *C* can be used. Cohen's *w* has no upper bound (can be
#' arbitrarily large, depending on the expected distribution). *Fei* is an
#' adjusted Cohen's *w*, accounting for the expected distribution, making it
#' bounded between 0-1. Pearson's *C* is also bounded between 0-1.
#' \cr \cr
#' To summarize, for correlation-like effect sizes, we recommend:
#'
#' - For a 2x2 table, use `phi()`
#' - For larger tables, use `cramers_v()`
#' - For goodness-of-fit, use `fei()`
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @return A data frame with the effect size (`Cramers_v`, `phi` (possibly with
#'   the suffix `_adjusted`), `Cohens_w`, `Fei`) and its CIs (`CI_low` and
#'   `CI_high`).
#'
#' @seealso [chisq_to_phi()] for details regarding estimation and CIs.
#' @family effect size indices
#'
#' @examples
#'
#' ## 2-by-2 tables
#' ## -------------
#' RCT <- matrix(c(71, 50,
#'                 30, 100), nrow = 2)
#' dimnames(RCT) <- list(Diagnosis = c("Sick", "Recovered"),
#'                       Group = c("Treatment", "Control"))
#' RCT # note groups are COLUMNS
#'
#' phi(RCT)
#' pearsons_c(RCT)
#'
#'
#'
#' ## Larger tables
#' ## -------------
#' M <- matrix(c(150, 100, 165,
#'               130, 50, 65,
#'               35, 10, 2,
#'               55, 40, 25), nrow = 4)
#' dimnames(M) <- list(Music = c("Pop", "Rock", "Jazz", "Classic"),
#'                     Study = c("Psych", "Econ", "Law"))
#' M
#'
#' cramers_v(M)
#'
#' cohens_w(M)
#'
#' pearsons_c(M)
#'
#'
#'
#' ## Goodness of fit
#' ## ---------------
#' Smoking_ASD <- as.table(c(ASD = 17, ASP = 11, TD = 640))
#'
#' fei(Smoking_ASD)
#'
#' cohens_w(Smoking_ASD)
#'
#' pearsons_c(Smoking_ASD)
#'
#' # Use custom expected values:
#' fei(Smoking_ASD, p = c(0.015, 0.010, 0.975))
#'
#' cohens_w(Smoking_ASD, p = c(0.015, 0.010, 0.975))
#'
#' pearsons_c(Smoking_ASD, p = c(0.015, 0.010, 0.975))
#'
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
#' effect size for chi-squared and likelihood-ratio goodness-of-fit tests.
#' Perceptual and motor skills, 103(2), 412-414.
#' - Rosenberg, M. S. (2010). A generalized formula for converting chi-square
#' tests to effect sizes for meta-analysis. PloS one, 5(4), e10059.
#'
#'
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, ci = 0.95, alternative = "greater", adjust = FALSE, ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], "BFcontingencyTable")) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
    return(effectsize(x, type = "phi", adjust = adjust, ci = ci, ...))
  }


  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
      grepl("Chi-squared test for given probabilities", x$method))) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
  } else {
    x <- suppressWarnings(stats::chisq.test(x, y))
    x$data.name <- NULL
  }

  effectsize(x, type = "phi", adjust = adjust, ci = ci, alternative = alternative)
}

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cramers_v <- function(x, y = NULL, ci = 0.95, alternative = "greater", adjust = FALSE, ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], "BFcontingencyTable")) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
    return(effectsize(x, type = "cramers_v", adjust = adjust, ci = ci, ...))
  }


  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
          grepl("Chi-squared test for given probabilities", x$method))) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
  } else {
    x <- suppressWarnings(stats::chisq.test(x, y))
    x$data.name <- NULL
  }

  effectsize(x, type = "cramers_v", adjust = adjust, ci = ci, alternative = alternative)
}

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cohens_w <- function(x, y = NULL, p = rep(1/length(x), length(x)),
                     ci = 0.95, alternative = "greater", ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], "BFcontingencyTable")) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
    return(effectsize(x, type = "phi", ci = ci, ...))
  }


  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
      grepl("Chi-squared test for given probabilities", x$method))) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
  } else {
    x <- suppressWarnings(stats::chisq.test(x, y, p = p, ...))
    x$data.name <- NULL
  }

  effectsize(x, type = "cohens_w", ci = ci, alternative = alternative)
}


#' @rdname phi
#' @importFrom stats chisq.test
#' @export
fei <- function(x, p = rep(1/length(x), length(x)), ci = 0.95, alternative = "greater", ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "BFBayesFactor")) {
    stop("Fei is only applicable to goodness of fit tests.", call. = FALSE)
  }


  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
      grepl("Chi-squared test for given probabilities", x$method))) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
  } else {
    x <- suppressWarnings(stats::chisq.test(x, y = NULL, p = p, ...))
    x$data.name <- NULL
  }

  effectsize(x, type = "fei", ci = ci, alternative = alternative)
}

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
pearsons_c <- function(x, y = NULL, p = rep(1/length(x), length(x)),
                     ci = 0.95, alternative = "greater", ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], "BFcontingencyTable")) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
    return(effectsize(x, type = "pearsons_c", ci = ci, ...))
  }


  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
      grepl("Chi-squared test for given probabilities", x$method))) {
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    }
  } else {
    x <- suppressWarnings(stats::chisq.test(x, y, p = p, ...))
    x$data.name <- NULL
  }

  effectsize(x, type = "pearsons_c", ci = ci, alternative = alternative)
}

