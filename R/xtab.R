#' Effect size for contingency tables
#'
#' Compute Cramer's *V*, phi (\eqn{\phi}), Cohen's *w* (an alias of phi), Odds
#' ratios, Risk ratios and Cohen's *g* for contingency tables or
#' goodness-of-fit. See details.
#'
#' @inheritParams stats::chisq.test
#' @param ci Confidence Interval (CI) level
#' @param adjust Should the effect size be bias-corrected? Defaults to `FALSE`.
#' @param CI Deprecated in favor of `ci`.
#' @param ... Arguments passed to [stats::chisq.test()], such as `p`. Ignored
#'   for `cohens_g()`.
#'
#' @details
#' Cramer's *V* and phi (\eqn{\phi}) are effect sizes for tests of independence
#' in 2D contingency tables, or for goodness-of-fit in 1D tables. For 2-by-2
#' tables, they are identical, and are equal to the simple correlation between
#' two dichotomous variables, ranging between  0 (no dependence) and 1 (perfect
#' dependence). For larger tables, Cramer's *V* should be used, as it is bounded
#' between 0-1, whereas phi can be larger than 1.
#' \cr\cr
#' For 2-by-2 contingency tables, Odds ratios and Risk ratios can also be
#' estimated. Note that these are computed with each **column** representing the
#' different groups, and the first column representing the treatment group and
#' the second column baseline (or control). Effects are given as `treatment /
#' control`. If you wish you use rows as groups you must pass a transposed
#' table, or switch the `x` and `y` arguments.
#' \cr\cr
#' Cohen's *g* is an effect size for dependent (paired) contingency tables
#' ranging between 0 (perfect symmetry) and 0.5 (perfect asymmetry) (see
#' [stats::mcnemar.test()]).
#'
#' # Confidence Intervals for g, OR and RR
#' For Cohen's *g*, confidence intervals are based on the proportion (\eqn{P = g
#' + 0.5}) confidence intervals returned by [stats::prop.test()] (minus 0.5),
#' which give a good close approximation.
#' \cr\cr
#' For Odds ratios and Risk ratios, confidence intervals are estimated using the
#' standard normal parametric method (see Katz et al., 1978; Szumilas, 2010).
#' \cr\cr
#' See *Confidence Intervals* and *CI Contains Zero* sections for *phi*, Cohen's
#' *w* and Cramer's *V*.
#'
#' @inheritSection cohens_d Confidence Intervals
#' @inheritSection chisq_to_phi CI Contains Zero
#'
#' @return A data frame with the effect size (`Cramers_v`, `phi` (possibly with
#'   the suffix `_adjusted`), `Odds_ratio`, `Risk_ratio` (possibly with the
#'   prefix `log_`), or `Cohens_g`) and its CIs (`CI_low` and `CI_high`).
#'
#' @seealso [chisq_to_phi()] for details regarding estimation and CIs.
#' @family effect size indices
#'
#' @examples
#' M <- rbind(
#'   c(150, 130, 35, 55),
#'   c(100, 50, 10, 40),
#'   c(165, 65, 2, 25)
#' )
#' dimnames(M) <- list(
#'   Study = c("Psych", "Econ", "Law"),
#'   Music = c("Pop", "Rock", "Jazz", "Classic")
#' )
#' M
#'
#' phi(M)
#'
#' cramers_v(M)
#'
#'
#'
#' ## 2-by-2 tables
#' ## -------------
#' RCT <- rbind(
#'   c(30, 71),
#'   c(100, 50)
#' )
#' dimnames(RCT) <- list(
#'   Diagnosis = c("Sick", "Recovered"),
#'   Group = c("Control", "Treatment")
#' )
#' RCT # note groups are COLUMNS
#'
#' oddsratio(RCT)
#'
#' riskratio(RCT)
#'
#'
#'
#' ## Dependent (Paired) Contingency Tables
#' ## -------------------------------------
#' Performance <- rbind(
#'   c(794, 86),
#'   c(150, 570)
#' )
#' dimnames(Performance) <- list(
#'   "1st Survey" = c("Approve", "Disapprove"),
#'   "2nd Survey" = c("Approve", "Disapprove")
#' )
#' Performance
#'
#' cohens_g(Performance)
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Katz, D. J. S. M., Baptista, J., Azen, S. P., & Pike, M. C. (1978). Obtaining confidence intervals for the risk ratio in cohort studies. Biometrics, 469-474.
#' - Szumilas, M. (2010). Explaining odds ratios. Journal of the Canadian academy of child and adolescent psychiatry, 19(3), 227.
#'
#' @importFrom stats chisq.test
#' @export
phi <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI, ...) {
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
          grepl("Chi-squared test for given probabilities", x$method)))
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    return(effectsize(x, type = "phi", adjust = adjust, ci = ci))
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  if (!is.null(dim(Exp))) {
    if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
      stop("Cannot have empty rows/columns in the contingency tables.", call. = FALSE)
    }
    nr <- nrow(Obs)
    nc <- ncol(Obs)
  } else {
    nr <- length(Obs)
    nc <- 1
  }

  chisq_to_phi(
    chisq = .chisq(Obs, Exp),
    n = sum(Obs),
    nrow = nr,
    ncol = nc,
    ci = ci,
    adjust = adjust
  )
}

#' @rdname phi
#' @export
cohens_w <- phi

#' @rdname phi
#' @importFrom stats chisq.test
#' @export
cramers_v <- function(x, y = NULL, ci = 0.95, adjust = FALSE, CI, ...) {
  if (!missing(CI)) {
    ci <- CI
    warning("'CI' argument is deprecated. Use 'ci' instead.")
  }

  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
          grepl("Chi-squared test for given probabilities", x$method)))
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    return(effectsize(x, type = "cramers_v", adjust = adjust, ci = ci))
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed
  Exp <- res$expected

  if (!is.null(dim(Exp))) {
    if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
      stop("Cannot have empty rows/columns in the contingency tables.", call. = FALSE)
    }
    nr <- nrow(Obs)
    nc <- ncol(Obs)
  } else {
    nr <- length(Obs)
    nc <- 1
  }

  chisq_to_cramers_v(
    chisq = .chisq(Obs, Exp),
    n = sum(Obs),
    nrow = nr,
    ncol = nc,
    ci = ci,
    adjust = adjust
  )
}


#' @rdname phi
#' @inheritParams oddsratio_to_d
#' @export
#' @importFrom stats chisq.test qnorm
oddsratio <- function(x, y = NULL, ci = 0.95, log = FALSE, ...) {
  if (inherits(x, "htest")) {
    if (grepl("Pearson's Chi-squared", x$method) ||
          grepl("Chi-squared test for given probabilities", x$method)) {
      return(effectsize(x, type = "or", log = log, ci = ci))
    } else if (grepl("Fisher's Exact", x$method)) {
      return(effectsize(x, ...))
    } else {
      stop("'x' is not a Chi-squared / Fisher's Exact test!", call. = FALSE)
    }
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    stop("Cannot have empty rows/columns in the contingency tables.", call. = FALSE)
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    stop("Odds ratio only available for 2-by-2 contingency tables", call. = FALSE)
  }

  OR <- (Obs[1, 1] / Obs[2, 1]) /
    (Obs[1, 2] / Obs[2, 2])

  res <- data.frame(Odds_ratio = OR)

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    alpha <- 1 - ci

    SE_logodds <- sqrt(sum(1 / Obs))
    Z_logodds <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    confs <- exp(log(OR) + c(-1, 1) * SE_logodds * Z_logodds)

    res$CI_low <- confs[1]
    res$CI_high <- confs[2]
  }

  if (log) {
    res[colnames(res) %in% c("Odds_ratio", "CI_low", "CI_high")] <-
      log(res[colnames(res) %in% c("Odds_ratio", "CI_low", "CI_high")])
    colnames(res)[1] <- "log_Odds_ratio"
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}

#' @rdname phi
#' @inheritParams oddsratio_to_d
#' @export
#' @importFrom stats chisq.test qnorm
riskratio <- function(x, y = NULL, ci = 0.95, log = FALSE, ...) {
  if (inherits(x, "htest")) {
    if (!(grepl("Pearson's Chi-squared", x$method) ||
          grepl("Chi-squared test for given probabilities", x$method)))
      stop("'x' is not a Chi-squared test!", call. = FALSE)
    return(effectsize(x, type = "rr", log = log, ci = ci))
  }

  res <- suppressWarnings(stats::chisq.test(x, y, ...))
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    stop("Cannot have empty rows/columns in the contingency tables.", call. = FALSE)
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    stop("Risk ratio only available for 2-by-2 contingency tables", call. = FALSE)
  }

  n1 <- sum(Obs[, 1])
  n2 <- sum(Obs[, 2])
  p1 <- Obs[1, 1] / n1
  p2 <- Obs[1, 2] / n2
  RR <- p1 / p2

  res <- data.frame(Risk_ratio = RR)

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    alpha <- 1 - ci

    SE_logRR <- sqrt(p1 / ((1 - p1) * n1)) + sqrt(p2 / ((1 - p2) * n2))
    Z_logRR <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    confs <- exp(log(RR) + c(-1, 1) * SE_logRR * Z_logRR)

    res$CI_low <- confs[1]
    res$CI_high <- confs[2]
  }

  if (log) {
    res[colnames(res) %in% c("Risk_ratio", "CI_low", "CI_high")] <-
      log(res[colnames(res) %in% c("Risk_ratio", "CI_low", "CI_high")])
    colnames(res)[1] <- "log_Risk_ratio"
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}


#' @rdname phi
#' @export
#' @importFrom stats complete.cases prop.test
cohens_g <- function(x, y = NULL, ci = 0.95, ...) {
  if (inherits(x, "htest")) {
    if (!grepl("McNemar", x$method))
      stop("'x' is not a McNemar test!", call. = FALSE)
    return(effectsize(x, ci = ci))
  }


  if (!is.matrix(x)) {
    if (is.null(y)) {
      stop("if 'x' is not a matrix, 'y' must be given")
    }
    if (length(x) != length(y)) {
      stop("'x' and 'y' must have the same length")
    }
    OK <- stats::complete.cases(x, y)
    x <- as.factor(x[OK])
    y <- as.factor(y[OK])
    if ((nlevels(x) < 2) || (nlevels(y) != nlevels(x))) {
      stop("'x' and 'y' must have the same number of levels (minimum 2)")
    }
    x <- table(x, y)
  } else {
    if ((nrow(x) < 2) || (ncol(x) != nrow(x))) {
      stop("'x' must be square with at least two rows and columns")
    }
  }


  b <- x[upper.tri(x)]
  c <- t(x)[upper.tri(x)]

  P <- sum(pmax(b, c)) / (sum(b) + sum(c))
  g <- P - 0.5

  out <- data.frame(Cohens_g = g)

  if (is.numeric(ci)) {
    n <- sum(b) + sum(c)
    k <- P * n

    res <- stats::prop.test(k, n,
      p = 0.5,
      conf.level = ci,
      correct = FALSE
    )

    out$CI <- ci
    out$CI_low <- res$conf.int[1] - 0.5
    out$CI_high <- res$conf.int[2] - 0.5
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  return(out)
}



#' @keywords internal
.chisq <- function(Obs, Exp) {
  sum(((Obs - Exp)^2) / Exp)
}
