#' Odds Ratios, Risk Ratios and Other Effect Sizes for 2-by-2 Contingency Tables
#'
#' Compute Odds Ratios, Risk Ratios, Cohen's *h*, Absolute Risk Reduction or
#' Number Needed to Treat. Report with any [`stats::chisq.test()`] or
#' [`stats::fisher.test()`].
#' \cr\cr
#' Note that these are computed with each **column** representing the different
#' groups (the *first* column representing the treatment group and the
#' *second* column the baseline or control group), and the *first* row
#' representing the "positive" level (the `k` in `p=k/n`).
#' Effects are given as _p_-treatment _over_ _p_-control.
#' If you wish you use rows as groups you must pass a transposed
#' table, or switch the `x` and `y` arguments.
#'
#'
#' @inheritParams oddsratio_to_d
#' @inheritParams phi
#' @param alternative a character string specifying the alternative hypothesis;
#'   Controls the type of CI returned: `"two.sided"` (default, two-sided CI),
#'   `"greater"` or `"less"` (one-sided CI). Partial matching is allowed (e.g.,
#'   `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in [effectsize_CIs].
#' @param ... Ignored
#'
#' @details
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals are estimated using the standard normal parametric
#' method (see Katz et al., 1978; Szumilas, 2010).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @return A data frame with the effect size (`Odds_ratio`, `log_Odds_ratio`,
#'   `Risk_ratio` `Cohens_h`, `ARR`, `NNT`) and its CIs (`CI_low` and
#'   `CI_high`).
#'
#' @family effect sizes for contingency table
#'
#'
#' @references
#' - Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd Ed.). New York: Routledge.
#' - Katz, D. J. S. M., Baptista, J., Azen, S. P., & Pike, M. C. (1978). Obtaining confidence intervals for the risk ratio in cohort studies. Biometrics, 469-474.
#' - Szumilas, M. (2010). Explaining odds ratios. Journal of the Canadian academy of child and adolescent psychiatry, 19(3), 227.
#'
#' @examples
#' data("RCT_table")
#' RCT_table # note groups are COLUMNS
#'
#' oddsratio(RCT_table)
#' oddsratio(RCT_table, alternative = "greater")
#'
#' riskratio(RCT_table)
#'
#' cohens_h(RCT_table)
#'
#' arr(RCT_table)
#'
#' nnt(RCT_table)
#'
#' @export
oddsratio <- function(x, y = NULL, ci = 0.95, alternative = "two.sided", log = FALSE, ...) {
  alternative <- .match.alt(alternative)

  if (.is_htest_of_type(x, "(Pearson's Chi-squared|Fisher's Exact)", "Chi-squared-test or Fisher's Exact test") ||
      inherits(x, c("datawizard_crosstabs", "datawizard_crosstab"))) {
    return(effectsize(x, type = "or", log = log, ci = ci, alternative = alternative))
  } else if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "or", log = log, ci = ci))
  }

  res <- .get_data_xtabs(x, y)
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    insight::format_error("Odds ratio only available for 2-by-2 contingency tables")
  }

  OR <- (Obs[1, 1] / Obs[2, 1]) /
    (Obs[1, 2] / Obs[2, 2])

  res <- data.frame(Odds_ratio = OR)

  if (.test_ci(ci)) {
    res$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level

    SE_logodds <- sqrt(sum(1 / Obs))
    Z_logodds <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    confs <- exp(log(OR) + c(-1, 1) * SE_logodds * Z_logodds)

    res$CI_low <- confs[1]
    res$CI_high <- confs[2]

    ci_method <- list(method = "normal")
    res <- .limit_ci(res, alternative, 0, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  if (log) {
    res[colnames(res) %in% c("Odds_ratio", "CI_low", "CI_high")] <-
      log(res[colnames(res) %in% c("Odds_ratio", "CI_low", "CI_high")])
    colnames(res)[1] <- "log_Odds_ratio"
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "approximate") <- FALSE
  attr(res, "alternative") <- alternative
  return(res)
}

#' @rdname oddsratio
#' @export
riskratio <- function(x, y = NULL, ci = 0.95, alternative = "two.sided", ...) {
  if ("log" %in% ...names() && isTRUE(list(...)$log)) {
    insight::format_warning("'log' argument has been deprecated.",
                            "Returning RR instead of log(RR)")
  }

  alternative <- .match.alt(alternative)

  if (.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test") ||
      inherits(x, c("datawizard_crosstabs", "datawizard_crosstab"))) {
    return(effectsize(x, type = "rr", ci = ci, alternative = alternative))
  } else if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "rr", ci = ci, ...))
  }

  res <- .get_data_xtabs(x, y)
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    insight::format_error("Risk ratio only available for 2-by-2 contingency tables")
  }

  n1 <- sum(Obs[, 1])
  n2 <- sum(Obs[, 2])
  p1 <- Obs[1, 1] / n1
  p2 <- Obs[1, 2] / n2
  RR <- p1 / p2

  res <- data.frame(Risk_ratio = RR)

  if (.test_ci(ci)) {
    res$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level

    SE_logRR <- sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
    Z_logRR <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    confs <- exp(log(RR) + c(-1, 1) * SE_logRR * Z_logRR)

    res$CI_low <- confs[1]
    res$CI_high <- confs[2]

    ci_method <- list(method = "normal")
    res <- .limit_ci(res, alternative, 0, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "approximate") <- FALSE
  attr(res, "alternative") <- alternative
  return(res)
}

#' @rdname oddsratio
#' @export
cohens_h <- function(x, y = NULL, ci = 0.95, alternative = "two.sided", ...) {
  alternative <- .match.alt(alternative)

  if (.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test") ||
      inherits(x, c("datawizard_crosstabs", "datawizard_crosstab"))) {
    return(effectsize(x, type = "cohens_h", ci = ci, alternative = alternative))
  } else if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "cohens_h", ci = ci, ...))
  }

  res <- .get_data_xtabs(x, y)
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    insight::format_error("Cohen's h only available for 2-by-2 contingency tables")
  }

  n1 <- sum(Obs[, 1])
  n2 <- sum(Obs[, 2])
  p1 <- Obs[1, 1] / n1
  p2 <- Obs[1, 2] / n2
  H <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))

  out <- data.frame(Cohens_h = H)

  if (.test_ci(ci)) {
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level

    se_arcsin <- 2 * sqrt(0.25 * (1 / n1 + 1 / n2))
    Zc <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    out$CI_low <- H - Zc * se_arcsin
    out$CI_high <- H + Zc * se_arcsin

    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, -pi, pi)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}


#' @rdname oddsratio
#' @export
arr <- function(x, y = NULL, ci = 0.95, alternative = "two.sided", ...) {
  alternative <- .match.alt(alternative)

  if (.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test") ||
      inherits(x, c("datawizard_crosstabs", "datawizard_crosstab"))) {
    return(effectsize(x, type = "arr", ci = ci, alternative = alternative))
  } else if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "arr", ci = ci, ...))
  }

  res <- .get_data_xtabs(x, y)
  Obs <- res$observed

  if (any(c(colSums(Obs), rowSums(Obs)) == 0L)) {
    insight::format_error("Cannot have empty rows/columns in the contingency tables.")
  }

  if (nrow(Obs) != 2 || ncol(Obs) != 2) {
    insight::format_error("This effect size is only available for 2-by-2 contingency tables")
  }

  n1 <- sum(Obs[, 1])
  n2 <- sum(Obs[, 2])
  p1 <- Obs[1, 1] / n1
  p2 <- Obs[1, 2] / n2
  ARR <- p1 - p2

  out <- data.frame(ARR)

  if (.test_ci(ci)) {
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level

    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    Zc <- stats::qnorm(alpha / 2, lower.tail = FALSE)
    out$CI_low <- ARR - Zc * se
    out$CI_high <- ARR + Zc * se

    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, -1, 1)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_table", "see_effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}


#' @rdname oddsratio
#' @export
nnt <- function(x, y = NULL, ci = 0.95, alternative = "two.sided", ...) {
  alternative <- .match.alt(alternative)

  flip_alt <- c(less = "greater", greater = "less", two.sided = "two.sided")
  alternative2 <- unname(flip_alt[alternative])

  if (.is_htest_of_type(x, "Pearson's Chi-squared", "Chi-squared-test") ||
      inherits(x, c("datawizard_crosstabs", "datawizard_crosstab"))) {
    return(effectsize(x, type = "nnt", ci = ci, alternative = alternative))
  } else if (.is_BF_of_type(x, "BFcontingencyTable", "Chi-squared")) {
    return(effectsize(x, type = "nnt", ci = ci, ...))
  }

  out <- arr(x, y = y, ci = ci, alternative = alternative2, ...)
  out[[1]] <- 1 / out[[1]]
  colnames(out)[1] <- "NNT"

  if ("CI" %in% colnames(out)) {
    ci_sign <- unlist(sign(out[c("CI_low", "CI_high")]))
    if (all(ci_sign == 1) || all(ci_sign == -1)) {
      out[c("CI_low", "CI_high")] <- 1 / out[c("CI_high", "CI_low")]
    } else {
      out[c("CI_low", "CI_high")] <- 1 / out[c("CI_low", "CI_high")]
    }

    out <- .limit_ci(out, alternative, -Inf, Inf)
  } else {
    alternative <- NULL
  }

  attr(out, "alternative") <- alternative
  return(out)
}
