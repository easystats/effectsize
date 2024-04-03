#' Dominance Effect Sizes for Rank Based Differences
#'
#' Compute the rank-biserial correlation (\eqn{r_{rb}}{r_rb}) and Cliff's *delta*
#' (\eqn{\delta}) effect sizes for non-parametric
#' (rank sum) differences. These effect sizes of dominance are closely related
#' to the [Common Language Effect Sizes][cohens_u3]. Pair with any reported
#' [`stats::wilcox.test()`].
#'
#' @inheritParams p_superiority
#' @inheritParams cohens_d
#' @param x,y A numeric or ordered vector, or a character name of one in `data`.
#'   Any missing values (`NA`s) are dropped from the resulting vector. `x` can
#'   also be a formula (see [`stats::wilcox.test()`]), in which case `y` is
#'   ignored.
#' @param mu a number indicating the value around which (a-)symmetry (for
#'   one-sample or paired samples) or shift (for independent samples) is to be
#'   estimated. See [stats::wilcox.test].
#'
#' @details
#'
#' The rank-biserial correlation is appropriate for non-parametric tests of
#' differences - both for the one sample or paired samples case, that would
#' normally be tested with Wilcoxon's Signed Rank Test (giving the
#' **matched-pairs** rank-biserial correlation) and for two independent samples
#' case, that would normally be tested with Mann-Whitney's *U* Test (giving
#' **Glass'** rank-biserial correlation). See [stats::wilcox.test]. In both
#' cases, the correlation represents the difference between the proportion of
#' favorable and unfavorable pairs / signed ranks (Kerby, 2014). Values range
#' from `-1` complete dominance of the second sample (*all* values of the second
#' sample are larger than *all* the values of the first sample) to `+1` complete
#' dominance of the fist sample (*all* values of the second sample are smaller
#' than *all* the values of the first sample).
#' \cr\cr
#' Cliff's *delta* is an alias to the rank-biserial correlation in the two sample case.
#'
#' # Ties
#' When tied values occur, they are each given the average of the ranks that
#' would have been given had no ties occurred. This results in an effect size of
#' reduced magnitude. A correction has been applied for Kendall's *W*.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals for the rank-biserial correlation (and Cliff's *delta*)
#' are estimated using the normal approximation (via Fisher's transformation).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @return A data frame with the effect size `r_rank_biserial` and its CI
#'   (`CI_low` and `CI_high`).
#'
#' @family effect size indices
#' @family standardized differences
#' @family rank-based effect sizes
#'
#' @examples
#' \donttest{
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$cyl <- factor(mtcars$cyl)
#'
#' # Two Independent Samples ----------
#' (rb <- rank_biserial(mpg ~ am, data = mtcars))
#' # Same as:
#' # rank_biserial("mpg", "am", data = mtcars)
#' # rank_biserial(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])
#' # cliffs_delta(mpg ~ am, data = mtcars)
#'
#' # More options:
#' rank_biserial(mpg ~ am, data = mtcars, mu = -5)
#' print(rb, append_CLES = TRUE)
#'
#'
#' # One Sample ----------
#' # from help("wilcox.test")
#' x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#' y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' depression <- data.frame(first = x, second = y, change = y - x)
#'
#' rank_biserial(change ~ 1, data = depression)
#'
#' # same as:
#' # rank_biserial("change", data = depression)
#' # rank_biserial(mtcars$wt)
#'
#' # More options:
#' rank_biserial(change ~ 1, data = depression, mu = -0.5)
#'
#'
#' # Paired Samples ----------
#' (rb <- rank_biserial(Pair(first, second) ~ 1, data = depression))
#'
#' # same as:
#' # rank_biserial(depression$first, depression$second, paired = TRUE)
#'
#' interpret_rank_biserial(0.78)
#' interpret(rb, rules = "funder2019")
#' }
#'
#' @references
#' - Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika, 21(3),
#' 287-290.
#'
#' - Glass, G. V. (1965). A ranking variable analogue of biserial correlation:
#' Implications for short-cut item analysis. Journal of Educational Measurement,
#' 2(1), 91-95.
#'
#' - Kerby, D. S. (2014). The simple difference formula: An approach to teaching
#' nonparametric correlation. Comprehensive Psychology, 3, 11-IT.
#'
#' - King, B. M., & Minium, E. W. (2008). Statistical reasoning in the
#' behavioral sciences. John Wiley & Sons Inc.
#'
#' - Cliff, N. (1993). Dominance statistics: Ordinal analyses to answer ordinal
#' questions. Psychological bulletin, 114(3), 494.
#'
#' - Tomczak, M., & Tomczak, E. (2014). The need to report effect size estimates
#' revisited. An overview of some recommended measures of effect size.
#'
#'
#' @export
rank_biserial <- function(x, y = NULL, data = NULL,
                          mu = 0, paired = FALSE,
                          ci = 0.95, alternative = "two.sided",
                          verbose = TRUE, ...) {
  alternative <- .match.alt(alternative)

  if (.is_htest_of_type(x, "Wilcoxon", "Wilcoxon-test")) {
    return(effectsize(x, verbose = verbose, type = "rb"))
  }

  ## Prep data
  out <- .get_data_2_samples(x, y, data,
    paired = paired,
    allow_ordered = TRUE,
    verbose = verbose, ...
  )
  x <- out[["x"]]
  y <- out[["y"]]
  paired <- out[["paired"]]

  if (is.null(y)) {
    y <- 0
    paired <- TRUE
  }

  ## Compute
  r_rbs <- .r_rbs(x, y, mu = mu, paired = paired, verbose = verbose)
  out <- data.frame(r_rank_biserial = r_rbs)

  ## CI
  if (.test_ci(ci)) {
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level

    rf <- atanh(r_rbs)
    if (paired) {
      nd <- sum((x - mu) != 0)
      maxw <- (nd^2 + nd) / 2

      # From: https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test#Historical_T_statistic/
      # wSE <- sqrt((n * (n + 1) * (2 * n + 1)) / 24)
      # Delta method for f(x) = w * 2 / (maxw) - 1
      # r_rbsSE <- wSE * sqrt(4 / (maxw)^2)
      # Delta method for z: z_rbsSE <- r_rbsSE / (1 - r_rbs^2)
      #   But simulations suggest that z_rbsSE is positively biased
      #   more than r_rbsSE is negatively biased, especially when r_rbs is large,
      #   so we use r_rbsSE instead
      rfSE <- sqrt((2 * nd^3 + 3 * nd^2 + nd) / 6) / maxw
    } else {
      n1 <- length(x)
      n2 <- length(y)

      # From: https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Normal_approximation_and_tie_correction/
      # wSE <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
      # Delta method for f(x) = 1 - 2 * w / (n1 * n2) * sign(diff)
      # r_rbsSE <- wSE * sqrt(4 / (n1 * n2)^2)
      # Delta method for z: z_rbsSE <- r_rbsSE / (1 - r_rbs^2)
      #   But simulations suggest that z_rbsSE is positively biased
      #   more than r_rbsSE is negatively biased, especially when r_rbs is large,
      #   so we use r_rbsSE instead
      rfSE <- sqrt((n1 + n2 + 1) / (3 * n1 * n2))
    }

    confint <- tanh(rf + c(-1, 1) * stats::qnorm(1 - alpha / 2) * rfSE)
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]
    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, -1, 1)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired") <- paired
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

#' @export
#' @rdname rank_biserial
cliffs_delta <- function(x, y = NULL, data = NULL,
                         mu = 0,
                         ci = 0.95, alternative = "two.sided",
                         verbose = TRUE, ...) {
  cl <- match.call()
  data <- .get_data_2_samples(x, y, data,
    verbose = verbose,
    allow_ordered = TRUE,
    ...
  )
  x <- data$x
  y <- data$y
  if (is.null(y) || isTRUE(match.call()$paired) || isTRUE(data[["paired"]])) {
    insight::format_error("This effect size is only applicable for two independent samples.")
  }

  cl[[1]] <- quote(effectsize::rank_biserial)
  cl$x <- x
  cl$y <- y
  eval.parent(cl)
}


# Utils -------------------------------------------------------------------

#' @keywords internal
.r_rbs <- function(x, y, mu, paired, verbose = FALSE) {
  if (paired) {
    Ry <- .safe_ranktransform((x - y) - mu, sign = TRUE, verbose = verbose)
    Ry <- stats::na.omit(Ry)

    n <- length(Ry)
    S <- (n * (n + 1) / 2)

    U1 <- sum(Ry[Ry > 0], na.rm = TRUE)
    U2 <- -sum(Ry[Ry < 0], na.rm = TRUE)
  } else {
    Ry <- .safe_ranktransform(c(x - mu, y), verbose = verbose)

    n1 <- as.double(length(x)) # see https://github.com/easystats/effectsize/issues/476
    n2 <- length(y)
    S <- (n1 * n2)

    U1 <- sum(Ry[seq_along(x)]) - n1 * (n1 + 1) / 2
    U2 <- sum(Ry[-seq_along(x)]) - n2 * (n2 + 1) / 2
  }

  u_ <- U1 / S
  f_ <- U2 / S
  return(u_ - f_)
}
