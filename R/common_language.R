#' Cohen's *U*s and Other Common Language Effect Sizes (CLES)
#'
#' Cohen's \eqn{U_1}, \eqn{U_2}, and \eqn{U_3}, probability of superiority,
#' proportion of overlap, Wilcoxon-Mann-Whitney odds, and Vargha and Delaney's
#' *A* are CLESs. These are effect sizes that represent differences between two
#' (independent) distributions in probabilistic terms (See details). Pair with
#' any reported [`stats::t.test()`] or [`stats::wilcox.test()`].
#'
#' @inheritParams cohens_d
#' @param parametric Use parametric estimation (see [cohens_d()]) or
#'   non-parametric estimation (see [rank_biserial()]). See details.
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL` and `parametric = FALSE`.
#'
#' @details
#' These measures of effect size present group differences in probabilistic
#' terms:
#' - **Probability of superiority** is the probability that, when sampling an
#'   observation from each of the groups at random, that the observation from
#'   the second group will be larger than the sample from the first group. For
#'   the one-sample (or paired) case, it is the probability that the sample (or
#'   difference) is larger than *mu*. (Vargha and Delaney's *A* is an alias for
#'   the non-parametric *probability of superiority*.)
#' - **Cohen's \eqn{U_1}** is the proportion of the total of both distributions
#'   that does not overlap.
#' - **Cohen's \eqn{U_2}** is the proportion of one of the groups that exceeds
#'   *the same proportion* in the other group.
#' - **Cohen's \eqn{U_3}** is the proportion of the second group that is smaller
#'   than the median of the first group.
#' - **Overlap** (OVL) is the proportional overlap between the distributions.
#'   (When `parametric = FALSE`, [bayestestR::overlap()] is used.)
#'
#' Wilcoxon-Mann-Whitney odds are the *odds* of
#' non-parametric superiority (via [probs_to_odds()]), that is the odds that,
#' when sampling an observation from each of the groups at random, that the
#' observation from the second group will be larger than the sample from the
#' first group.
#'
#' Where \eqn{U_1}, \eqn{U_2}, and *Overlap* are agnostic to the direction of
#' the difference between the groups, \eqn{U_3} and probability of superiority
#' are not.
#'
#' The parametric version of these effects assumes normality of both populations
#' and homoscedasticity. If those are not met, the non parametric versions
#' should be used.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' For parametric CLES, the CIs are transformed CIs for Cohen's *d* (see
#' [`d_to_u3()`]). For non-parametric (`parametric = FALSE`) CLES, the CI of
#' *Pr(superiority)* is a transformed CI of the rank-biserial correlation
#' ([`rb_to_p_superiority()`]), while for all others, confidence intervals are
#' estimated using the bootstrap method (using the `{boot}` package).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection effectsize_CIs Bootstrapped CIs
#'
#' @return A data frame containing the common language effect sizes (and
#'   optionally their CIs).
#'
#' @note If `mu` is not 0, the effect size represents the difference between the
#'   first *shifted sample* (by `mu`) and the second sample.
#'
#' @references
#' - Cohen, J. (1977). Statistical power analysis for the behavioral sciences.
#' New York: Routledge.
#'
#' - Reiser, B., & Faraggi, D. (1999). Confidence intervals for the overlapping
#' coefficient: the normal equal variance case. Journal of the Royal Statistical
#' Society, 48(3), 413-418.
#'
#' - Ruscio, J. (2008). A probability-based measure of effect size: robustness
#' to base rates and other factors. Psychological methods, 13(1), 19–30.
#'
#' - Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the CL
#' common language effect size statistics of McGraw and Wong. Journal of
#' Educational and Behavioral Statistics, 25(2), 101-132.
#'
#' - O’Brien, R. G., & Castelloe, J. (2006, March). Exploiting the link between
#' the Wilcoxon-Mann-Whitney test and a simple odds statistic.
#' In Proceedings of the Thirty-first Annual SAS Users Group International
#' Conference (pp. 209-31). Cary, NC: SAS Institute.
#'
#' - Agresti, A. (1980). Generalized odds ratios for ordinal data.
#' Biometrics, 59-67.
#'
#' @seealso [d_to_cles()] [sd_pooled()]
#' @family effect size indices
#'
#' @examples
#' cohens_u2(mpg ~ am, data = mtcars)
#'
#' p_superiority(mpg ~ am, data = mtcars, parametric = FALSE)
#'
#' wmw_odds(mpg ~ am, data = mtcars)
#'
#' x <- c(1.83, 0.5, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.3)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#'
#' p_overlap(x, y)
#' p_overlap(y, x) # direction of effect does not matter
#'
#' cohens_u3(x, y)
#' cohens_u3(y, x) # direction of effect does matter
#'
#' @export
#' @aliases cles
p_superiority <- function(x, y = NULL, data = NULL,
                          mu = 0, paired = FALSE, parametric = TRUE,
                          ci = 0.95, alternative = "two.sided",
                          verbose = TRUE, ...) {
  if (.is_htest_of_type(x, "(t-test|Wilcoxon)", "t-test or a Wilcoxon-test")) {
    return(effectsize(x, type = "p_superiority", verbose = verbose, ...))
  } else if (.is_BF_of_type(x, c("BFindepSample", "BFoneSample"), "t-test")) {
    return(effectsize(x, type = "p_superiority", ci = ci, verbose = verbose, ...))
  }

  data <- .get_data_2_samples(x, y, data, paired = paired,
                              allow_ordered = !parametric,
                              verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]

  if (parametric) {
    d <- cohens_d(
      x = x,
      y = y,
      paired = paired, pooled_sd = TRUE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    d_to_p_superiority(d)
  } else {
    rb <- rank_biserial(
      x = x,
      y = y,
      paired = paired,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    rb_to_p_superiority(rb)
  }
}

#' @export
#' @rdname p_superiority
cohens_u1 <- function(x, y = NULL, data = NULL,
                      mu = 0, parametric = TRUE,
                      ci = 0.95, alternative = "two.sided", iterations = 200,
                      verbose = TRUE, ...) {
  if (.is_htest_of_type(x, "(t-test|Wilcoxon)", "t-test or a Wilcoxon-test")) {
    return(effectsize(x, type = "u1", verbose = verbose, ...))
  } else if (.is_BF_of_type(x, "BFindepSample", "t-test")) {
    return(effectsize(x, type = "u1", ci = ci, verbose = verbose, ...))
  }


  data <- .get_data_2_samples(x, y, data,
                              allow_ordered = !parametric,
                              verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]
  if (is.null(y)) stop("cohens_u3 only applicable to two sample case.", call. = FALSE)

  if (!parametric) {
    stop("Cohen's U1 only available for parametric estimation.", call. = FALSE)
  }

  d <- cohens_d(
    x = x,
    y = y,
    paired = FALSE, pooled_sd = TRUE,
    mu = mu,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    ...
  )
  out <- d_to_u1(d)

  out
}


#' @export
#' @rdname p_superiority
cohens_u2 <- function(x, y = NULL, data = NULL,
                      mu = 0, parametric = TRUE,
                      ci = 0.95, alternative = "two.sided", iterations = 200,
                      verbose = TRUE, ...) {
  if (.is_htest_of_type(x, "(t-test|Wilcoxon)", "t-test or a Wilcoxon-test")) {
    return(effectsize(x, type = "u2", verbose = verbose, ...))
  } else if (.is_BF_of_type(x, "BFindepSample", "t-test")) {
    return(effectsize(x, type = "u2", ci = ci, verbose = verbose, ...))
  }


  data <- .get_data_2_samples(x, y, data,
                              allow_ordered = !parametric,
                              verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]
  if (is.null(y)) stop("cohens_u3 only applicable to two sample case.", call. = FALSE)

  if (parametric) {
    d <- cohens_d(
      x = x,
      y = y,
      paired = FALSE, pooled_sd = TRUE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    out <- d_to_u2(d)
  } else {
    out <- .cohens_u2_non_parametric(
      x, y, ci = ci,
      mu = mu, alternative = alternative,
      iterations = iterations
    )
  }
  out
}


#' @export
#' @rdname p_superiority
cohens_u3 <- function(x, y = NULL, data = NULL,
                      mu = 0, parametric = TRUE,
                      ci = 0.95, alternative = "two.sided", iterations = 200,
                      verbose = TRUE, ...) {
  if (.is_htest_of_type(x, "(t-test|Wilcoxon)", "t-test or a Wilcoxon-test")) {
    return(effectsize(x, type = "u3", verbose = verbose, ...))
  } else if (.is_BF_of_type(x, "BFindepSample", "t-test")) {
    return(effectsize(x, type = "u3", ci = ci, verbose = verbose, ...))
  }


  data <- .get_data_2_samples(x, y, data,
                              allow_ordered = !parametric,
                              verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]
  if (is.null(y)) stop("cohens_u3 only applicable to two sample case.", call. = FALSE)

  if (parametric) {
    d <- cohens_d(
      x = x,
      y = y,
      paired = FALSE, pooled_sd = TRUE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    out <- d_to_u3(d)
  } else {
    out <- .cohens_u3_non_parametric(
      x, y, ci = ci,
      mu = mu, alternative = alternative,
      iterations = iterations
    )
  }
  out
}

#' @export
#' @rdname p_superiority
p_overlap <- function(x, y = NULL, data = NULL,
                      mu = 0, parametric = TRUE,
                      ci = 0.95, alternative = "two.sided", iterations = 200,
                      verbose = TRUE, ...) {
  if (.is_htest_of_type(x, "(t-test|Wilcoxon)", "t-test or a Wilcoxon-test")) {
    return(effectsize(x, type = "overlap", verbose = verbose, ...))
  } else if (.is_BF_of_type(x, "BFindepSample", "t-test")) {
    return(effectsize(x, type = "overlap", ci = ci, verbose = verbose, ...))
  }


  data <- .get_data_2_samples(x, y, data,
                              allow_ordered = !parametric,
                              verbose = verbose, ...)
  x <- data[["x"]]
  y <- data[["y"]]
  if (is.null(y)) stop("Overlap only applicable to two sample case.", call. = FALSE)

  if (parametric) {
    d <- cohens_d(
      x = x,
      y = y,
      paired = FALSE, pooled_sd = TRUE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    out <- d_to_overlap(d)
  } else {
    out <- .overlap_non_parametric(
      x, y, ci = ci,
      mu = mu, alternative = alternative,
      iterations = iterations
    )
  }
  out
}

#' @export
#' @rdname p_superiority
vd_a <- function(x, y = NULL, data = NULL,
                 mu = 0,
                 ci = 0.95, alternative = "two.sided",
                 verbose = TRUE, ...) {
  cl <- match.call()
  cl[[1]] <- quote(p_superiority)
  cl$parametric <- FALSE
  eval.parent(cl)
}


#' @export
#' @rdname p_superiority
wmw_odds <- function(x, y = NULL, data = NULL,
                     mu = 0, paired = FALSE,
                     ci = 0.95, alternative = "two.sided",
                     verbose = TRUE, ...) {
  cl <- match.call()
  cl[[1]] <- quote(rank_biserial)
  out <- eval.parent(cl)

  rb_to_wmw_odds(out)
}




# Utils -------------------------------------------------------------------

#' @keywords internal
#' @importFrom stats quantile optim
.cohens_u2_non_parametric <- function(..., mu, alternative) {
  U2_np <- function(data, i = seq_len(nrow(data))) {
    data <- data[i, ]
    x <- data[data$g == "x", "r"] - mu
    y <- data[data$g == "y", "r"]

    .foo <- function(p) {
      min(abs(stats::quantile(x, probs = c(p, 1 - p)) -
                stats::quantile(y, probs = c(1 - p, p))))
    }

    stats::optim(par = 0.5, fn = .foo,
                 method = "L-BFGS-B",
                 lower = 0.5, upper = 1,
                 control = list(pgtol = 1e-09))$par
  }

  out <- .cles_non_parametric(..., est = U2_np)
  colnames(out)[1] <- "Cohens_U2"

  if ("CI" %in% colnames(out)) {
    if (alternative == "less") out$CI_low <- 0
    if (alternative == "greater") out$CI_high <- 1
  }

  out
}

#' @keywords internal
.cohens_u3_non_parametric <- function(..., mu, alternative) {
  U3_np <- function(data, i = seq_len(nrow(data))) {
    data <- data[i, ]
    x <- data[data$g == "x", "r"] - mu
    y <- data[data$g == "y", "r"]

    sum(y < median(x)) / length(y)
  }
  out <- .cles_non_parametric(..., est = U3_np)
  colnames(out)[1] <- "Cohens_U3"

  if ("CI" %in% colnames(out)) {
    if (alternative == "less") out$CI_low <- 0
    if (alternative == "greater") out$CI_high <- 1
  }

  out
}

#' @keywords internal
.overlap_non_parametric <- function(..., mu, alternative) {
  OVL_np <- function(data, i = seq_len(nrow(data))) {
    data <- data[i, ]
    x <- data[data$g == "x", "r"] - mu
    y <- data[data$g == "y", "r"]

    bayestestR::overlap(x, y)
  }
  out <- .cles_non_parametric(..., est = OVL_np)
  colnames(out)[1] <- "Overlap"

  if ("CI" %in% colnames(out)) {
    if (alternative == "less") out$CI_low <- 0
    if (alternative == "greater") out$CI_high <- 1
  }

  out
}


## BOOT and stuff ---------------

#' @keywords internal
#' @importFrom utils tail
.cles_non_parametric <-
  function(x,
           y,
           est,
           ci = 0.95,
           mu = 0,
           alternative = "two.sided",
           iterations = 200) {

    d <- data.frame(
      r = c(x, y),
      g = rep(c("x", "y"), c(length(x), length(y)))
    )

    out <- data.frame(ES = est(d))

    ci_method <- NULL
    if (is.numeric(ci)) {
      if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
        stopifnot(length(ci) == 1, ci < 1, ci > 0)

        ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

        out$CI <- ci

        R <- boot::boot(
          data = d,
          statistic = est,
          R = iterations
        )

        bCI <- boot::boot.ci(R, conf = ci, type = "perc")[["percent"]]
        bCI <- utils::tail(as.vector(bCI), 2)
        out$CI_low <- bCI[1]
        out$CI_high <- bCI[2]
        ci_method <- list(method = "percentile bootstrap", iterations = iterations)
      } else {
        alternative <- NULL
      }
    }

    class(out) <- c("effectsize_table", class(out))
    # TODO
    # class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
    .someattributes(out) <- .nlist(mu, ci, ci_method, alternative, approximate = TRUE,
                                   table_footer = "Non-parametric CLES")
    return(out)
  }
