#' \eqn{\phi} and Other Contingency Tables Correlations
#'
#' Compute phi (\eqn{\phi}), Cramer's *V*, Tschuprow's *T*, Cohen's *w*,
#' \ifelse{latex}{\eqn{Fei}}{פ (Fei)}, Pearson's contingency coefficient for
#' contingency tables or goodness-of-fit. Pair with any reported
#' [`stats::chisq.test()`].
#'
#' @inheritParams stats::chisq.test
#' @inheritParams chisq_to_phi
#' @param ... Ignored.
#'
#' @details
#'
#' phi (\eqn{\phi}), Cramer's *V*, Tschuprow's *T*, Cohen's *w*, and Pearson's
#' *C* are effect sizes for tests of independence in 2D contingency tables. For
#' 2-by-2 tables, phi, Cramer's *V*, Tschuprow's *T*, and Cohen's *w* are
#' identical, and are equal to the simple correlation between two dichotomous
#' variables, ranging between  0 (no dependence) and 1 (perfect dependence).
#' \cr\cr
#' For larger tables, Cramer's *V*, Tschuprow's *T* or Pearson's *C* should be
#' used, as they are bounded between 0-1. (Cohen's *w* can also be used, but
#' since it is not bounded at 1 (can be larger) its interpretation is more
#' difficult.) For square table, Cramer's *V* and Tschuprow's *T* give the same
#' results, but for non-square tables Tschuprow's *T* is more conservative:
#' while *V* will be 1 if either columns are fully dependent on rows (for each
#' column, there is only one non-0 cell) *or* rows are fully dependent on
#' columns, *T* will only be 1 if both are true.
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
#' @family effect sizes for contingency table
#'
#' @examples
#'
#' ## 2-by-2 tables
#' ## -------------
#' data("RCT_table")
#' RCT_table # note groups are COLUMNS
#'
#' phi(RCT_table)
#' pearsons_c(RCT_table)
#'
#'
#'
#' ## Larger tables
#' ## -------------
#' data("Music_preferences")
#' Music_preferences
#'
#' cramers_v(Music_preferences)
#'
#' cohens_w(Music_preferences)
#'
#' pearsons_c(Music_preferences)
#'
#'
#'
#' ## Goodness of fit
#' ## ---------------
#' data("Smoking_FASD")
#' Smoking_FASD
#'
#' fei(Smoking_FASD)
#'
#' cohens_w(Smoking_FASD)
#'
#' pearsons_c(Smoking_FASD)
#'
#' # Use custom expected values:
#' fei(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#'
#' cohens_w(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#'
#' pearsons_c(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
#' effect size for chi-squared and likelihood-ratio goodness-of-fit tests.
#' Perceptual and motor skills, 103(2), 412-414.
#' - Rosenberg, M. S. (2010). A generalized formula for converting chi-square
#' tests to effect sizes for meta-analysis. PloS one, 5(4), e10059.
#'
#'
#' @export
phi <- function(x, y = NULL,
                adjust = TRUE,
                ci = 0.95, alternative = "greater",
                ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "phi", adjust = adjust, ci = ci))
  } else if (!.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test")) {
    x <- suppressWarnings(stats::chisq.test(x, y))
    x$data.name <- NULL
  }

  effectsize(x, type = "phi", adjust = adjust, ci = ci, alternative = alternative)
}

#' @rdname phi
#' @export
cramers_v <- function(x, y = NULL,
                      adjust = TRUE,
                      ci = 0.95, alternative = "greater",
                      ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "cramers_v", adjust = adjust, ci = ci))
  } else if (!.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test")) {
    x <- suppressWarnings(stats::chisq.test(x, y))
    x$data.name <- NULL
  }

  effectsize(x, type = "cramers_v", adjust = adjust, ci = ci, alternative = alternative)
}


#' @rdname phi
#' @export
tschuprows_t <- function(x, y = NULL,
                         ci = 0.95, alternative = "greater",
                         ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "tschuprows_t", ci = ci))
  } else if (!.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test")) {
    x <- suppressWarnings(stats::chisq.test(x, y))
    x$data.name <- NULL
  }

  effectsize(x, type = "tschuprows_t", ci = ci, alternative = alternative)
}

#' @rdname phi
#' @export
cohens_w <- function(x, y = NULL, p = rep(1, length(x)),
                     ci = 0.95, alternative = "greater",
                     ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "cohens_w", ci = ci))
  } else if (!.is_htest_of_type(
    x, "(Pearson's Chi-squared|Chi-squared test for given probabilities)",
    "Chi-squared-test"
  )) {
    x <- suppressWarnings(stats::chisq.test(x, y, p = p, rescale.p = TRUE))
    x$data.name <- NULL
  }

  effectsize(x, type = "cohens_w", ci = ci, alternative = alternative)
}


#' @rdname phi
#' @export
fei <- function(x, p = rep(1, length(x)),
                ci = 0.95, alternative = "greater",
                ...) {
  alternative <- .match.alt(alternative, FALSE)
  check_1d_table <- dim(x)

  if (inherits(x, "BFBayesFactor") || (!is.null(check_1d_table) && check_1d_table[2] > 1)) {
    insight::format_error("Fei is only applicable to goodness of fit tests.")
  } else if (!.is_htest_of_type(x, "Chi-squared test for given probabilities", "Chi-squared-test")) {
    x <- suppressWarnings(stats::chisq.test(x, y = NULL, p = p, rescale.p = TRUE))
    x$data.name <- NULL
  }

  effectsize(x, type = "fei", ci = ci, alternative = alternative)
}

#' @rdname phi
#' @export
pearsons_c <- function(x, y = NULL, p = rep(1, length(x)),
                       ci = 0.95, alternative = "greater",
                       ...) {
  alternative <- .match.alt(alternative, FALSE)

  if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "pearsons_c", ci = ci))
  } else if (!.is_htest_of_type(
    x, "(Pearson's Chi-squared|Chi-squared test for given probabilities)",
    "Chi-squared-test"
  )) {
    x <- suppressWarnings(stats::chisq.test(x, y, p = p, rescale.p = TRUE))
    x$data.name <- NULL
  }

  effectsize(x, type = "pearsons_c", ci = ci, alternative = alternative)
}
