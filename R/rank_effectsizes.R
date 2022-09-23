#' Effect size for non-parametric (rank sum) tests
#'
#' Compute the rank-biserial correlation (\eqn{r_{rb}}{r_rb}), Cliff's *delta*
#' (\eqn{\delta}), rank epsilon squared (\eqn{\varepsilon^2}{\epsilon^2}),
#' Kendall's *W*, and Wilcoxon-Mann-Whitney Odds Ratio effect sizes for non-parametric (rank sum) tests.
#'
#' @inheritParams cohens_d
#' @param x Can be one of:
#'   - A numeric vector, or a character name of one in `data`.
#'   - A formula in to form of `DV ~ groups` (for `rank_biserial()` and
#'   `rank_epsilon_squared()`) or `DV ~ groups | blocks` (for `kendalls_w()`;
#'   See details for the `blocks` and `groups` terminology used here).
#'   - A list of vectors (for `rank_epsilon_squared()`).
#'   - A matrix of `blocks x groups` (for `kendalls_w()`) (or `groups x blocks`
#'   if `blocks_on_rows = FALSE`). See details for the `blocks` and `groups`
#'   terminology used here.
#' @param y An optional numeric vector of data values to compare to `x`, or a
#'   character name of one in `data`. Ignored if `x` is not a vector.
#' @param groups,blocks A factor vector giving the group / block for the
#'   corresponding elements of `x`, or a character name of one in `data`.
#'   Ignored if `x` is not a vector.
#' @param blocks_on_rows Are blocks on rows (`TRUE`) or columns (`FALSE`).
#' @param mu a number indicating the value around which (a-)symmetry (for
#'   one-sample or paired samples) or shift (for independent samples) is to be
#'   estimated. See [stats::wilcox.test].
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`. (Deprecated for
#'   `rank_biserial()`).
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"two.sided"` (two-sided CI; default for
#'   rank-biserial correlation and Cliff's *delta*), `"greater"` (default for
#'   rank epsilon squared and Kendall's *W*) or `"less"` (one-sided CI). Partial
#'   matching is allowed (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs*
#'   in [effectsize_CIs].
#'
#' @details
#' The rank-biserial correlation is appropriate for non-parametric tests of
#' differences - both for the one sample or paired samples case, that would
#' normally be tested with Wilcoxon's Signed Rank Test (giving the
#' **matched-pairs** rank-biserial correlation) and for two independent samples
#' case, that would normally be tested with Mann-Whitney's *U* Test (giving
#' **Glass'** rank-biserial correlation). See [stats::wilcox.test]. In both
#' cases, the correlation represents the difference between the proportion of
#' favorable and unfavorable pairs / signed ranks (Kerby, 2014). Values range
#' from `-1` (*all* values of the second sample are larger than *all* the values
#' of the first sample) to `+1` (*all* values of the second sample are smaller
#' than *all* the values of the first sample). Cliff's *delta* is an alias to
#' the rank-biserial correlation in the two sample case.
#' \cr\cr
#' The rank epsilon squared is appropriate for non-parametric tests of
#' differences between 2 or more samples (a rank based ANOVA). See
#' [stats::kruskal.test]. Values range from 0 to 1, with larger values
#' indicating larger differences between groups.
#' \cr\cr
#' Kendall's *W* is appropriate for non-parametric tests of differences between
#' 2 or more dependent samples (a rank based rmANOVA), where each `group` (e.g.,
#' experimental condition) was measured for each `block` (e.g., subject). This
#' measure is also common as a measure of reliability of the rankings of the
#' `groups` between raters (`blocks`). See [stats::friedman.test]. Values range
#' from 0 to 1, with larger values indicating larger differences between groups
#' / higher agreement between raters.
#'\cr\cr
#' Wilcoxon-Mann-Whitney odds (WMWodds) is an extension of Agresti's generalized
#' odds ratio, but accounts for ties (see below). In the two sample case, the
#' WMWodds statistic represents the odds that if a pair of observations are
#' randomly selected from two groups, the outcome in one group is higher than
#' the other (minus mu). In the one-sample (or paired) case, it is the odds of
#' single observation being greater than the mu parameter (default is zero).
#' The odds can be converted to a non-parametric version of the probability of
#' superiority (also referred to as the concordance probability) using the
#' `odds_to_probs` function. Like the rank-biserial  correlation, it is an
#' appropriate effect size to accompany a non-parametric tests of rank
#' differences (i.e., any result derived from the [stats::wilcox.test] function).
#'
#'
#' ## Ties
#' When tied values occur, they are each given the average of the ranks that
#' would have been given had no ties occurred. This results in an effect size of
#' reduced magnitude. A correction has been applied for Kendall's *W*.
#'
#' # Confidence Intervals
#' Confidence intervals for the rank-biserial correlation (and Cliff's *delta*)
#' are estimated using the normal approximation (via Fisher's transformation).
#' Confidence intervals for the WMWodds are estimated using a normal approximation
#' (Agresti's method for two-sample, and Wilson's score interval method
#' for the one-sample/paired case).
#' Confidence intervals for rank Epsilon squared, and Kendall's *W* are
#' estimated using the bootstrap method (using the `{boot}` package).
#'
#' @return A data frame with the effect size (`r_rank_biserial`,
#'   `rank_epsilon_squared` or `Kendalls_W`) and its CI (`CI_low` and
#'   `CI_high`).
#'
#' @family effect size indices
#'
#' @examples
#' \donttest{
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$cyl <- factor(mtcars$cyl)
#'
#' # Rank Biserial Correlation
#' # =========================
#'
#' # Two Independent Samples ----------
#' (rb <- rank_biserial(mpg ~ am, data = mtcars))
#' # Same as:
#' # rank_biserial(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])
#'
#' # More options:
#' rank_biserial(mpg ~ am, data = mtcars, mu = -5)
#' print(rb, append_CLES = TRUE)
#'
#'
#' # One Sample ----------
#' rank_biserial(wt ~ 1, data = mtcars, mu = 3)
#' # same as:
#' # rank_biserial("wt", data = mtcars, mu = 3)
#' # rank_biserial(mtcars$wt, mu = 3)
#'
#'
#' # Paired Samples ----------
#' dat <- data.frame(
#'   Cond1 = c(1.83, 0.5, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.3),
#'   Cond2 = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' )
#' (rb <- rank_biserial(Pair(Cond1, Cond2) ~ 1, data = dat, paired = TRUE))
#'
#' # same as:
#' # rank_biserial(dat$Cond1, dat$Cond2, paired = TRUE)
#'
#' interpret_rank_biserial(0.78)
#' interpret(rb, rules = "funder2019")
#'
#'
#' # Rank Epsilon Squared
#' # ====================
#'
#' rank_epsilon_squared(mpg ~ cyl, data = mtcars)
#'
#'
#'
#' # Kendall's W
#' # ===========
#' dat <- data.frame(
#'   cond = c("A", "B", "A", "B", "A", "B"),
#'   ID = c("L", "L", "M", "M", "H", "H"),
#'   y = c(44.56, 28.22, 24, 28.78, 24.56, 18.78)
#' )
#' (W <- kendalls_w(y ~ cond | ID, data = dat, verbose = FALSE))
#'
#' interpret_kendalls_w(0.11)
#' interpret(W, rules = "landis1977")
#' }
#'
#' # WMW Odds
#' # =========================
#'
#' # Two Independent Samples ----------
#' (wmw <- wmw_odds(mpg ~ am, data = mtcars))
#' # Same as:
#' # wmw_odds(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])
#'
#' # More options:
#' wmw_odds(mpg ~ am, data = mtcars, mu = -5)
#'
#' # One Sample ----------
#' wmw_odds(wt ~ 1, data = mtcars, mu = 3)
#' # same as:
#' # wmw_odds("wt", data = mtcars, mu = 3)
#' # wmw_odds(mtcars$wt, mu = 3)
#'
#'
#' # Paired Samples ----------
#' dat <- data.frame(
#'   Cond1 = c(1.83, 0.5, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.3),
#'   Cond2 = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' )
#' (wmw <- wmwm_odds(Pair(Cond1, Cond2) ~ 1, data = dat, paired = TRUE))
#'
#' # same as:
#' # wmw_odds(dat$Cond1, dat$Cond2, paired = TRUE)
#' # wmw_odds("Cond1", "Cond2", data = dat, paired = TRUE)
#'
#'
#' @references
#' - Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika, 21(3),
#' 287-290.
#'
#' - Glass, G. V. (1965). A ranking variable analogue of biserial correlation:
#' Implications for short-cut item analysis. Journal of Educational Measurement,
#' 2(1), 91-95.
#'
#' - Kendall, M.G. (1948) Rank correlation methods. London: Griffin.
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
#' - O’Brien, R. G., & Castelloe, J. (2006, March). Exploiting the link between
#' the Wilcoxon-Mann-Whitney test and a simple odds statistic.
#' In Proceedings of the Thirty-first Annual SAS Users Group International
#' Conference (pp. 209-31). Cary, NC: SAS Institute.
#'
#' - Agresti, A. (1980). Generalized odds ratios for ordinal data.
#' Biometrics, 59-67.
#'
#' @export
#' @importFrom stats na.omit complete.cases
rank_biserial <- function(x,
                          y = NULL,
                          data = NULL,
                          mu = 0,
                          ci = 0.95,
                          alternative = "two.sided",
                          paired = FALSE,
                          verbose = TRUE,
                          ...,
                          iterations) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  if (inherits(x, "htest")) {
    if (!grepl("Wilcoxon", x$method)) {
      stop("'x' is not a Wilcoxon-test!", call. = FALSE)
    }
    return(effectsize(x, verbose = verbose, type = "rb"))
  }

  if (!missing(iterations) && verbose) {
    warning(
      "'iterations' argument is deprecated. CIs are estimated using a parametric normal approximation.",
      immediate. = TRUE
    )
  }

  ## Prep data
  out <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- out$x
  y <- out$y

  if (is.null(y)) {
    y <- rep(0, length.out = length(x))
    paired <- TRUE
  }

  if (paired) {
    oo <- stats::complete.cases(x, y)
    x <- x[oo]
    y <- y[oo]
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)
  }

  ## Compute
  r_rbs <- .r_rbs(x, y, mu = mu, paired = paired, verbose = verbose)
  out <- data.frame(r_rank_biserial = r_rbs)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    # if (requireNamespace("boot", quietly = TRUE)) {
    #   out <- cbind(out, .rbs_ci_boot(
    #     y,
    #     mu = mu,
    #     paired = paired,
    #     ci = ci,
    #     iterations = iterations
    #   ))
    #
    #   ci_method <- list(method = "bootstrap", iterations = iterations)
    # } else {
    #   ci <- NULL
    #   warning("For CIs, the 'boot' package must be installed.", call. = FALSE)
    # }

    # Parametric method
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

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

    confint <- tanh(rf + c(-1, 1) * qnorm(1 - alpha / 2) * rfSE)
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]
    ci_method <- list(method = "normal")
    if (alternative == "less") {
      out$CI_low <- -1
    } else if (alternative == "greater") {
      out$CI_high <- 1
    }
  } else {
    alternative <- NULL
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
cliffs_delta <- function(x,
                         y = NULL,
                         data = NULL,
                         mu = 0,
                         ci = 0.95,
                         alternative = "two.sided",
                         verbose = TRUE,
                         ...) {
  rank_biserial(
    x, y,
    data = data,
    mu = mu,
    paired = FALSE,
    ci = ci,
    alternative = alternative,
    verbose = verbose,
    ...
  )
}


#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
rank_epsilon_squared <- function(x,
                                 groups,
                                 data = NULL,
                                 ci = 0.95,
                                 alternative = "greater",
                                 iterations = 200,
                                 ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "htest")) {
    if (!grepl("Kruskal-Wallis", x$method)) {
      stop("'x' is not a Kruskal-Wallis-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations, alternative = alternative))
  }

  ## pep data
  data <- .get_data_multi_group(x, groups, data, ...)
  data <- stats::na.omit(data)

  ## compute
  out <- data.frame(rank_epsilon_squared = .repsilon(data))

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .repsilon_ci(data, ci, alternative, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }
  if (is.null(ci)) alternative <- NULL

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

#' @rdname rank_biserial
#' @export
#' @importFrom stats na.omit
#' @importFrom insight check_if_installed
kendalls_w <- function(x,
                       groups,
                       blocks,
                       data = NULL,
                       blocks_on_rows = TRUE,
                       ci = 0.95,
                       alternative = "greater",
                       iterations = 200,
                       verbose = TRUE,
                       ...) {
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))

  if (inherits(x, "htest")) {
    if (!grepl("Friedman", x$method)) {
      stop("'x' is not a Friedman-test!", call. = FALSE)
    }
    return(effectsize(x, ci = ci, iterations = iterations, verbose = verbose, alternative = alternative))
  }

  ## prep data
  if (is.matrix(x) && !blocks_on_rows) x <- t(x)
  data <- .get_data_nested_groups(x, groups, blocks, data, ...)
  data <- stats::na.omit(data)

  ## compute
  W <- .kendalls_w(data, verbose = verbose)
  out <- data.frame(Kendalls_W = W)

  ## CI
  ci_method <- NULL
  if (is.numeric(ci)) {
    if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      out <- cbind(out, .kendalls_w_ci(data, ci, alternative, iterations))
      ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    } else {
      ci <- NULL
    }
  }
  if (is.null(ci)) alternative <- NULL

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

#' @rdname rank_biserial
#' @export

wmw_odds <- function(x,
                     y = NULL,
                     data = NULL,
                     mu = 0,
                     ci = 0.95,
                     alternative = "two.sided",
                     paired = FALSE,
                     verbose = TRUE,
                     ...) {


  alternative <- match.arg(alternative)
  if(!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
    stop("'mu' must be a single number")
  if (!is.numeric(ci)) {
    ci <- NULL
    ci_method <- NULL
  }
  if(is.numeric(ci)){
    if(!((length(ci) == 1L)
         && is.finite(ci)
         && (ci > 0)
         && (ci < 1)))
      stop("'ci' must be a single number between 0 and 1")
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1
    alpha <- 1 - ci.level
  }

  if (is.null(ci)) {
    alternative <- NULL
    #interval = c(NA,NA)

  } else {
    ci_method <- list(method = "normal")
  }

  ## Prep data
  out <- .get_data_2_samples(x, y, data, verbose, ...)
  x <- out$x
  y <- out$y

  if(is.ordered(x)){
    x = as.numeric(x)
  }
  if(!is.numeric(x)) {
    stop("'x' must be numeric or ordered factor")
  }
  if(!is.null(y)) {
    if(is.ordered(y)){
      y = as.numeric(y)
    }
    if(!is.numeric(y)) stop("'y' must be numeric or ordered factor")

    if(paired) {
      if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
      OK <- complete.cases(x, y)
      x <- x[OK] - y[OK]
      y <- NULL
    }
    else {
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
    }
  } else {

    x <- x[is.finite(x)]
  }

  if(length(x) < 1L)
    stop("not enough (finite) 'x' observations")
  CORRECTION <- 0
  if(is.null(y)) { ##------------------ 1-sample/paired case -------------------
    z <- x - mu

    n_x = as.double(length(x))
    n_a <- sum(z > 0) + 0.5*sum(z == 0)

    cstat = n_a / n_x
    #if(cstat == 0 || cstat == 1){
    #  stop("Odds ratio cannot be estimated. No overlap with zero/null.")
    #}
    odds = probs_to_odds(cstat)


    if(is.numeric(ci)){
      # Wilson interval for binomial prob
      # Then converted to odds
      zstar <- qnorm(1-alpha/2)
      zstar2 = zstar^2
      p1 <- cstat + 0.5 * zstar2/n_x
      p2 <- zstar * sqrt((cstat * (1 - cstat) + 0.25 * zstar2/n_x)/n_x)
      p3 <- 1 + zstar2/n_x
      lcl <- (p1 - p2)/p3
      ucl <- (p1 + p2)/p3
      interval <- probs_to_odds(c(lcl,ucl))
    }




  } else { ##------------------------ 2-sample case -------------------------
    if(length(y) < 1L)
      stop("not enough 'y' observations")
    #r <- rank(c(x - mu, y))
    #n_x <- as.double(length(x))
    #n_y <- as.double(length(y))
    x2 = x - mu

    # Get Mann-Whitney U
    #Ustat <-  sum(r[seq_along(x)]) - n_x * (n_x + 1) / 2
    # Calc c-index
    #cstat = Ustat / (n_x * n_y)
    #if(cstat == 0 || cstat == 1){
    #  stop("Odds ratio cannot be estimated. No overlap between groups so concordance is 0% or 100%!")
    #}
    response = c(y,x2)
    group = c(rep(1,length(y)),rep(2,length(x)))
    crosstab = as.matrix(table(response, group))
    N = sum(crosstab)
    p = crosstab/N
    Rt = p[, 2:1]
    Rs = .rs_mat(p)
    Rd = .rd_mat(p)
    Rs = Rs + (1 - 0.5) * Rt
    Rd = Rd + 0.5 * Rt
    Pc = sum(p * Rs)
    Pd = sum(p * Rd)

    odds = (Pc/Pd)

    #if(odds == 0 || odds == Inf){
    #  stop("Odds ratio cannot be estimated. No overlap between groups.")
    #}

    ### ----- ci: normal approx ----
    ### bootstrap removed
    if(is.numeric(ci)){

      #odds2 = (Pc/Pd)
      # Not to self: checks calcs from above match matrix calcs
      #if(round(odds2,7) != round(odds,7)){
      #  stop("Matrix broken; likely bug in code.")
      #}
      SEodds = 2/Pd * (sum(p * (odds * Rd - Rs)^2)/N)^0.5
      SElnodds = SEodds/odds
      interval = exp(qnorm(c(alpha/2, 1 - alpha/2), mean = log(odds),
                           sd = SElnodds))

    } #else if(ci_method == "percent"){
#
      #if (insight::check_if_installed("boot", "for estimating CIs", stop = FALSE)) {
      #  data2 = data.frame(
      #    response = c(x,y),
      #    group = c(rep("x", length(x)), rep("y", length(y)))
      #  )
      #  interval <-  .wmw_odds_ci(
      #    data = data2,
      #    ci = ci.level,
      #    alternative = alternative,
      #    iterations = iterations,
      #    mu = mu,
      #    sample = 2
      #  )
#
      #  ci_method <- list(method = "percentile bootstrap", iterations = iterations)
      #}


  }
  out = data.frame(odds = odds)
  if(!is.null(ci_method)){
    out$CI = ci.level
    out$CI_low = if (alternative == "less") 0 else interval[1]
    out$CI_high = if (alternative == "greater") Inf else interval[2]
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

# rank_eta_squared <- function(x, g, data = NULL, ci = 0.95, iterations = 200) {
#
#   data <- .get_data_multi_group(x, g, data)
#   data <- stats::na.omit(data)
#   x <- data$x
#   g <- data$g
#
#   model <- stats::kruskal.test(x, g)
#
#   H <- unname(model$statistic)
#   k <- length(unique(g)) # model$parameter + 1
#   n <- length(g)
#
#   E <- (H - k + 1) / (n - k)
#
#   out <- data.frame(rank_eta_squared = E)
#
#   if (is.numeric(ci)) {
#     warning("Nope. Not yet.", call. = FALSE)
#     out$CI <- ci
#     out$CI_low <- 0
#     out$CI_high <- 1
#   }
#
#   class(out) <- c("effectsize_table", class(out))
#   return(out)
# }


# Utils -------------------------------------------------------------------

## Get ----

#' @keywords internal
#' @importFrom stats na.omit
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

    n1 <- length(x)
    n2 <- length(y)
    S <- (n1 * n2)

    U1 <- sum(Ry[seq_along(x)]) - n1 * (n1 + 1) / 2
    U2 <- sum(Ry[-seq_along(x)]) - n2 * (n2 + 1) / 2
  }

  u_ <- U1 / S
  f_ <- U2 / S
  return(u_ - f_)
}

#' @keywords internal
#' @importFrom stats kruskal.test
.repsilon <- function(data) {
  model <- stats::kruskal.test(data$x, data$groups)

  H <- unname(model$statistic)
  n <- nrow(data)

  E <- H / ((n^2 - 1) / (n + 1))
}


#' @keywords internal
.kendalls_w <- function(data, verbose) {
  rankings <- apply(data, 1, .safe_ranktransform, verbose = verbose)
  rankings <- t(rankings) # keep dims

  n <- ncol(rankings) # items
  m <- nrow(rankings) # judges
  R <- colSums(rankings)

  no_ties <- apply(rankings, 1, function(x) length(x) == insight::n_unique(x))
  if (!all(no_ties)) {
    if (verbose) {
      warning(
        sprintf(
          "%d block(s) contain ties%s.",
          sum(!no_ties),
          ifelse(any(apply(as.data.frame(rankings)[!no_ties, ], 1, insight::n_unique) == 1),
            ", some containing only 1 unique ranking", ""
          )
        ),
        call. = FALSE
      )
    }

    Tj <- 0
    for (i in seq_len(m)) {
      rater <- table(rankings[i, ])
      ties <- rater[rater > 1]
      l <- as.numeric(ties)
      Tj <- Tj + sum(l^3 - l)
    }

    W <- (12 * sum(R^2) - 3 * (m^2) * n * ((n + 1)^2)) /
      (m^2 * (n^3 - n) - m * Tj)
  } else {
    S <- var(R) * (n - 1)
    W <- (12 * S) /
      (m^2 * (n^3 - n))
  }
  W
}

## CI ----

# #' @keywords internal
# #' @importFrom bayestestR ci
# .rbs_ci_boot <- function(x,
#                          y,
#                          mu = 0,
#                          paired = FALSE,
#                          ci = 0.95,
#                          iterations = 200) {
#   stopifnot(length(ci) == 1, ci < 1, ci > 0)
#
#   if (paired) {
#     data <- data.frame(x, y)
#     boot_rbs <- function(.data, .i) {
#       .data <- .data[.i, ]
#       .x <- .data$x
#       .y <- .data$y
#       .r_rbs(.x, .y, mu = mu, paired = TRUE, verbose = FALSE)
#     }
#   } else {
#     data <- data.frame(
#       i = seq_along(c(x, y))
#     )
#
#     boot_rbs <- function(.data, .i) {
#       .x <- sample(x, replace = TRUE)
#       .y <- sample(y, replace = TRUE)
#
#       .r_rbs(.x, .y, mu = mu, paired = FALSE, verbose = FALSE)
#     }
#   }
#
#   R <- boot::boot(
#     data = data,
#     statistic = boot_rbs,
#     R = iterations
#   )
#
#   out <- as.data.frame(
#     bayestestR::ci(na.omit(R$t), ci = ci, verbose = FALSE)
#   )
#   out$CI <- ci
#   out
# }

#' @importFrom utils tail
#' @keywords internal
.repsilon_ci <- function(data, ci, alternative, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)
  ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1


  boot_r_epsilon <- function(.data, .i) {
    split(.data$x, .data$groups) <- lapply(split(.data$x, .data$groups),
      sample,
      replace = TRUE
    )
    .repsilon(.data)
  }

  R <- boot::boot(
    data = data,
    statistic = boot_r_epsilon,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = if (alternative == "less") 0 else bCI[1],
    CI_high = if (alternative == "greater") 1 else bCI[2]
  )
}

#' @importFrom utils tail
#' @keywords internal
.kendalls_w_ci <- function(data, ci, alternative, iterations) {
  stopifnot(length(ci) == 1, ci < 1, ci > 0)
  ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

  boot_w <- function(.data, .i) {
    .kendalls_w(.data[.i, ], verbose = FALSE) # sample rows
  }

  R <- boot::boot(
    data = data,
    statistic = boot_w,
    R = iterations
  )

  bCI <- boot::boot.ci(R, conf = ci.level, type = "perc")$percent
  bCI <- tail(as.vector(bCI), 2)

  data.frame(
    CI = ci,
    CI_low = if (alternative == "less") 0 else bCI[1],
    CI_high = if (alternative == "greater") 1 else bCI[2]
  )
}


# Utils -------------------------------------------------------------------

.safe_ranktransform <- function(x, verbose = TRUE, ...) {
  if (insight::n_unique(x) == 1) {
    return(rep(mean(seq_along(x)), length(x)))
  }
  datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
}

.rd_mat = function(p) {

  nc = nrow(p) # Number of categories
  ng = ncol(p) # Number of groups
  minc = nc - 1

  Rd = matrix(0, nrow = nc, ncol = ng)

  for (c in 1:nc) {
    pc = c+1
    if(pc <= nc){
      for (i in pc:nc) {
        Rd[c, 2] = Rd[c, 2] + p[i, 1]
      }
    }

    if(c-1 >=1){
      for (j in 1:(c-1)) {
        Rd[c, 1] = Rd[c, 1] + p[j, 2]
      }
    }

  }

  return(Rd)
}

.rs_mat = function(p) {

  nc = nrow(p) # Number of categories
  ng = ncol(p) # Number of groups
  minc = nc - 1

  Rs = matrix(0, nrow = nc, ncol = ng)

  for (c in 1:nc) {
    pc = c+1
    if(pc <= nc){
      for (i in pc:nc) {

        Rs[c, 1] = Rs[c, 1] + p[i, 2]
      }
    }

    if(c-1 >=1){
      for (j in 1:(c-1)) {
        Rs[c, 2] = Rs[c, 2] + p[j, 1]
      }
    }

  }

  return(Rs)
}


