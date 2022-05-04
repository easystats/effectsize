#' Estimate Common Language Effect Sizes (CLES)
#'
#' `cohens_u3()`, `p_superiority()`, and `p_overlap()` give only one of the
#' CLESs.
#'
#' @inheritParams cohens_d
#' @param parametric Use parametric estimation (see [cohens_d()]) or
#'   non-parametric estimation (see [rank_biserial()]).
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL` and `parametric = FALSE`.
#'
#' @details
#' These measures of effect size present group differences in probabilistic
#' terms:
#' - **Probability of superiority** is the probability that, when sampling an
#'   observation from each of the groups at random, that the observation from
#'   the second group will be larger than the sample from the first group.
#' - **Cohen's U3** is the proportion of the second group that is smaller than
#'   the median of the first group.
#' - **Overlap** (OVL) is the proportional overlap between the distributions.
#'   (When `parametric = FALSE`, [bayestestR::overlap()] is used.)
#'
#' For unequal group sizes, it is recommended to use the non-parametric based
#' CLES (`parametric = FALSE`).
#'
#' @section Confidence Intervals (CIs):
#' For parametric CLES, the CIs are transformed CIs for Cohen's *d*
#' ([`d_to_cles()`]). For non-parametric (`parametric = FALSE`) CLES, the CI of
#' *Pr(superiority)* is a transformed CI of the rank-biserial correlation
#' ([`rb_to_cles()`]), while for Cohen's *U3* and the Overlap coefficient the
#' confidence intervals are bootstrapped (requires the `boot` package).
#'
#' @return A data frame containing the common language effect sizes (and
#'   optionally their CIs).
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
#' @seealso [d_to_cles()] [sd_pooled()]
#' @family effect size indices
#'
#' @examples
#' cles(mpg ~ am, data = mtcars)
#'
#' set.seed(4)
#' cles(mpg ~ am, data = mtcars, parametric = FALSE)
#'
#' \dontrun{
#' ## Individual CLES
#' p_superiority(extra ~ group, data = sleep)
#'
#' cohens_u3(extra ~ group, data = sleep, parametric = FALSE)
#'
#' p_overlap(extra ~ group, data = sleep)
#' }
#'
#' @export
cles <- function(x,
                 y = NULL,
                 data = NULL,
                 mu = 0,
                 ci = 0.95,
                 alternative = "two.sided",
                 parametric = TRUE,
                 verbose = TRUE,
                 iterations = 200,
                 ...) {
  if (inherits(x, "htest")) {
    if (!grepl("(t-test|Wilcoxon)", x$method)) {
      stop("'x' is not a t-test or a Wilcoxon-test!", call. = FALSE)
    }
    return(effectsize(x, type = "cles", verbose = verbose, ...))
  } else if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], c("BFindepSample"))) {
      stop("'x' is not a t-test!", call. = FALSE)
    }
    return(effectsize(x, type = "cles", ci = ci, verbose = verbose, ...))
  }

  data <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- na.omit(data[["x"]])
  y <- na.omit(data[["y"]])

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
    d_to_cles(d)
  } else {
    rb <- rank_biserial(
      x = x,
      y = y,
      paired = FALSE,
      mu = mu,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      ...
    )
    out <- rbind(
      rb_to_cles(rb),
      .np_U3_OVL(x, y,
                 ci = ci, alternative = alternative,
                 iterations = iterations)
    )
    attr(out, "table_footer") <- "Non-parametric CLES"
    out
  }
}

#' @export
#' @rdname cles
common_language <- cles

#' @export
#' @rdname cles
cohens_u3 <- function(...) {
  .cles_which("u3", ...)
}

#' @export
#' @rdname cles
p_superiority <- function(...) {
  .cles_which("p", ...)
}

#' @export
#' @rdname cles
p_overlap <- function(...) {
  .cles_which("ovl", ...)
}

# Utils -------------------------------------------------------------------

#' @importFrom utils tail
#' @keywords internal
.np_U3_OVL <- function(x, y, ci = 0.95, alternative = "two.sided", iterations) {
  .get_np_U3_OVL <- function(data, i = seq_len(nrow(data))) {
    data <- data[i, ]

    c(U3 = sum(data[data$g == "y", "r"] < median(data[data$g == "x", "r"])) /
        nrow(data[data$g == "y", ]),
      OVL = bayestestR::overlap(
        data[data$g == "x", "r"],
        data[data$g == "y", "r"]
      ))
  }

  d <- data.frame(
    r = c(x, y),
    g = rep(c("x", "y"), c(length(x), length(y)))
  )

  out <- data.frame(
    Parameter = c("Cohen's U3", "Overlap"),
    Coefficient = .get_np_U3_OVL(d)
  )

  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      stopifnot(length(ci) == 1, ci < 1, ci > 0)
      ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

      R <- boot::boot(
        data = d,
        statistic = .get_np_U3_OVL,
        R = iterations
      )

      bCI <- list(boot::boot.ci(R, conf = ci.level, type = "perc", 1)$percent,
                  boot::boot.ci(R, conf = ci.level, type = "perc", 2)$percent)
      bCI <- lapply(bCI, function(.bci) tail(as.vector(.bci), 2))
      bCI <- matrix(unlist(bCI), 2, byrow = TRUE, dimnames = list(NULL, c("CI_low", "CI_high")))
      bCI <- cbind(CI = ci, as.data.frame(bCI))

      if (alternative == "less") bCI$CI_low <- 0
      if (alternative == "greater") bCI$CI_high <- 1
    } else {
      bCI <- data.frame(
        CI = NA, CI_low = NA, CI_high = NA
      )[c(1,1),]
    }
    out <- cbind(out, bCI)
  }
  out
}

#' @keywords internal
.cles_which <- function(type,
                        x,
                        y = NULL,
                        data = NULL,
                        mu = 0,
                        ci = 0.95,
                        alternative = "two.sided",
                        verbose = TRUE,
                        parametric = TRUE,
                        ...) {
  CLES <- cles(
    x,
    y = y,
    data = data,
    mu = mu,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    parametric = parametric,
    ...
  )

  if (type == "p")  {
    out <- CLES[1, ]
    colnames(out)[2] <- "p_superiority"
  } else if (type == "u3") {
    out <- CLES[2, ]
    colnames(out)[2] <- "Cohens_U3"
  } else if (type == "ovl") {
    out <- CLES[3, ]
    colnames(out)[2] <- "overlap"
  }
  out[[1]] <- NULL
  out
}