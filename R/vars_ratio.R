#' Ratio of Variances
#'
#' Computes the ratio of two variances of independent or paired samples. For
#' paired data, this can also be thought of as the ratio of marginal (squared)
#' scales of a bivariate normal distribution. For independent samples, this is a
#' convenient wrapper around [stats::var.test()] (for a paired version of this
#' test - the Pitman-Morgan test, see `PairedData::Var.test()`).
#'
#' @inheritParams cohens_d
#' @inheritParams means_ratio
#'
#' @details
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' For independent samples, confidence intervals are estimated using
#' [stats::var.test()]. For paired samples, confidence intervals are estimated
#' using the standard parametric method (see Pitman, 1939; Morgan, 1929).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @return A data frame with the effect size (`Vars_ratio`) and its CIs
#'   (`CI_low` and `CI_high`).
#'
#' @seealso [means_ratio()], [stats::var.test()] and `PairedData::Var.test()`
#'   for the _Pitman-Morgan Test_ for paired samples.
#'
#' @references
#' - Morgan, W. A. (1939). A test for the significance of the difference between
#' the two variances in a sample from a normal bivariate population. Biometrika,
#' 31(1/2), 13-19.
#' - Pitman, E. J. G. (1939). A note on normal correlation. Biometrika, 31(1/2), 9-12.
#'
#' @examples
#' # Two Independent Samples ----------
#'
#' x <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
#' y <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
#'
#' variance_ratio(x, y)
#'
#' # More options:
#' variance_ratio(x, y, alternative = "less")
#' variance_ratio(x, y, log = TRUE)
#'
#' # The ratio is scale invariant
#' variance_ratio(x * 3, y * 3)
#'
#'
#'
#' # Paired Samples ----------
#'
#' sleep2 <- reshape(sleep,
#'   direction = "wide",
#'   idvar = "ID", timevar = "group"
#' )
#' variance_ratio(Pair(extra.1, extra.2) ~ 1, data = sleep2)
#'
#' @export
variance_ratio <- function(x, y = NULL, data = NULL,
                           paired = FALSE, log = FALSE,
                           ci = 0.95, alternative = "two.sided",
                           verbose = TRUE, ...) {
  alternative <- .match.alt(alternative)
  out <- .get_data_2_samples(x, y, data, paired = paired, verbose = verbose, ...)
  x <- out[["x"]]
  y <- out[["y"]]
  paired <- out[["paired"]]

  if (is.null(y)) {
    insight::format_error("'y' is missing.")
  }

  if (paired) {
    out <- .var_ratio_dep(x, y, ci, alternative)
  } else {
    out <- .var_ratio_ind(x, y, ci, alternative)
  }

  if (log) {
    i <- intersect(colnames(out), c("Vars_ratio", "CI_low", "CI_high"))
    out[i] <- sapply(out[i], log)
    colnames(out)[1] <- "log_Vars_ratio"
  }

  out
}

#' @keywords internal
.var_ratio_ind <- function(x, y, ci, alternative) {
  conf.level <- if (is.null(ci)) 0.95 else ci
  vt <- stats::var.test(x, y, alternative = alternative, conf.level = conf.level)
  pars <- parameters::model_parameters(vt)
  pars$CI <- ci
  out <- pars[intersect(colnames(pars), c("Estimate", "CI", "CI_low", "CI_high"))]
  if (is.null(ci)) {
    out$CI_low <- out$CI_high <- NULL
  }
  colnames(out)[1] <- "Vars_ratio"
  ci_method <- list(method = "F")

  class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
  .someattributes(out) <- .nlist(
    paired = FALSE, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}

#' @keywords internal
.var_ratio_dep <- function(x, y, ci, alternative) {
  v1 <- stats::var(x)
  v2 <- stats::var(y)
  w <- v1 / v2
  out <- data.frame(Vars_ratio = w)

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)
    alpha <- 1 - ci.level

    # Pitman 1939, pp 11
    n <- length(x)
    df <- n - 2
    r <- stats::cor(x, y)
    qs <- qt(alpha / 2, df = n - 2)
    K <- 1 + (2 * (1 - r^2) * qs^2) / (n - 2)
    CONF <- w * (K + c(-1, 1) * sqrt(K^2 - 1))

    out$CI_low <- CONF[1]
    out$CI_high <- CONF[2]
    ci_method <- list(method = "t")
    out <- .limit_ci(out, alternative, 0, Inf)
  } else {
    ci_method <- alternative <- NULL
  }


  class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
  .someattributes(out) <- .nlist(
    paired = TRUE, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}
