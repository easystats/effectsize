# TODO
# add to vignette
# effectsize::effectsize()???

#' Standardized Mean Differences for Repeated Measures
#'
#' Compute effect size indices for standardized mean differences in repeated
#' measures data. Pair with any reported `stats::t.test(paired = TRUE)`.
#' \cr\cr
#' In a repeated-measures design, the same subjects are measured in multiple
#' conditions or time points. Unlike the case of independent groups, there are
#' multiple sources of variation that can be used to standardized the
#' differences between the means of the conditions / times.
#'
#' @param x,y Paired numeric vectors, or names of ones in `data`. `x` can also
#'   be a formula:
#' - `Pair(x,y) ~ 1` for wide data.
#' - `y ~ condition | id` for long data, possibly with repetitions.
#' @param method Method of repeated measures standardized differences. See details.
#' @param adjust Apply Hedges' small-sample bias correction?
#' @inheritParams cohens_d
#'
#' @details
#'
#' # Standardized Mean Differences for Repeated Measures
#'
#' Unlike [Cohen's d][cohens_d()] for independent groups, where standardization
#' naturally is done by the (pooled) population standard deviation (cf. Glass’s
#' \eqn{\Delta}), when measured across two conditions are dependent, there are
#' many more options for what error term to standardize by. Additionally, some
#' options allow for data to be replicated (many measurements per condition per
#' individual), others require a single observation per condition per individual
#' (aka, paired data; so replications are aggregated).
#'
#' (It should be noted that all of these have awful and confusing notations.)
#'
#' Standardize by...

#' - **Difference Score Variance: \eqn{d_{z}}** (_Requires paired data_) - This
#' is akin to computing difference scores for each individual and then
#' computing a one-sample Cohen's _d_ (Cohen, 1988, pp. 48; see examples).
#' - **Within-Subject Variance: \eqn{d_{rm}}** (_Requires paired data_) - Cohen
#' suggested adjusting \eqn{d_{z}} to estimate the "standard" between-subjects
#' _d_ by a factor of \eqn{\sqrt{2(1-r)}}, where _r_ is the Pearson correlation
#' between the paired measures (Cohen, 1988, pp. 48).
#' - **Control Variance: \eqn{d_{b}} (aka Becker's _d_)** (_Requires paired
#' data_) - Standardized by the variance of the control condition (or in a pre-
#' post-treatment setting, the pre-treatment condition). This is akin to Glass'
#' _delta_ ([glass_delta()]) (Becker, 1988). Note that this is taken here as the
#' _second_ condition (`y`).
#' - **Average Variance: \eqn{d_{av}}** (_Requires paired data_) - Instead of
#' standardizing by the variance in the of the control (or pre) condition,
#' Cumming suggests standardizing by the average variance of the two paired
#' conditions (Cumming, 2013, pp. 291).
#' - **All Variance: \eqn{d_{d}}** - This is the same as computing a standard
#' independent-groups Cohen's _d_ (Cohen, 1988). Note that CIs _do_ account for
#' the dependence, and so are typically more narrow (see examples).
#' - **Residual Variance: \eqn{d_{r}}** (_Requires data with replications_) -
#' Divide by the pooled variance after all individual differences have been
#' partialled out (i.e., the residual/level-1 variance in an ANOVA or MLM
#' setting). In between-subjects designs where each subject contributes a single
#' response, this is equivalent to classical Cohen’s d. Priors in the
#' `BayesFactor` package are defined on this scale (Rouder et al., 2012).
#'
#' \cr\cr
#' Note that for paired data, when the two conditions have equal variance,
#' \eqn{d_{rm}}, \eqn{d_{av}}, \eqn{d_{b}} are equal to \eqn{d}.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals are estimated using the standard normal parametric
#' method (see Becker, 1988; Cooper et al., 2009; Hedges & Olkin, 1985;
#' Pustejovsky et al., 2014).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @note
#' `rm_d()` is an alias for `repeated_measures_d()`.
#'
#' @return A data frame with the effect size and their CIs (`CI_low` and
#'   `CI_high`).
#'
#' @family standardized differences
#' @seealso [cohens_d()], and `lmeInfo::g_mlm()` and `emmeans::effsize()` for
#'   more flexible methods.
#'
#' @references
#'
#' - Becker, B. J. (1988). Synthesizing standardized mean‐change measures.
#' British Journal of Mathematical and Statistical Psychology, 41(2), 257-278.
#' - Cohen, J. (1988). Statistical power analysis for the behavioral
#' sciences (2nd Ed.). New York: Routledge.
#' - Cooper, H., Hedges, L., & Valentine, J. (2009). Handbook of research
#' synthesis and meta-analysis. Russell Sage Foundation, New York.
#' - Cumming, G. (2013). Understanding the new statistics: Effect sizes,
#' confidence intervals, and meta-analysis. Routledge.
#' - Hedges, L. V. & Olkin, I. (1985). Statistical methods for
#' meta-analysis. Orlando, FL: Academic Press.
#' - Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
#' Design-comparable effect sizes in multiple baseline designs: A general
#' modeling framework. Journal of Educational and Behavioral Statistics, 39(5),
#' 368-393.
#' - Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012).
#' Default Bayes factors for ANOVA designs. Journal of mathematical psychology,
#' 56(5), 356-374.
#'
#' @examples
#' # Paired data -------
#'
#' data("sleep")
#' sleep2 <- reshape(sleep, direction = "wide",
#'                   idvar = "ID", timevar = "group")
#'
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2)
#'
#' # Same as:
#' # repeated_measures_d(sleep$extra[sleep$group==1],
#' #                     sleep$extra[sleep$group==2])
#' # repeated_measures_d(extra ~ group | ID, data = sleep)
#'
#'
#' # More options:
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, mu = -1)
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, alternative = "less")
#'
#' # Other methods
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "av")
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "b")
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "d")
#' repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "z", adjust = FALSE)
#'
#' # d_z is the same as Cohen's d for one sample (of individual difference):
#' cohens_d(extra.1 - extra.2 ~ 1, data = sleep2)
#'
#'
#'
#' # Repetition data -----------
#'
#' data("rouder2016")
#'
#' # For rm, ad, z, b, data is aggregated
#' repeated_measures_d(rt ~ cond | id, data = rouder2016)
#'
#' # same as:
#' rouder2016_wide <- tapply(rouder2016[["rt"]], rouder2016[1:2], mean)
#' repeated_measures_d(rouder2016_wide[, 1], rouder2016_wide[, 2])
#'
#' # For r or d, data is not aggragated:
#' repeated_measures_d(rt ~ cond | id, data = rouder2016, method = "r")
#' repeated_measures_d(rt ~ cond | id, data = rouder2016, method = "d", adjust = FALSE)
#'
#' # d is the same as Cohen's d for two independent groups:
#' cohens_d(rt ~ cond, data = rouder2016, ci = NULL)
#'
#' @export
repeated_measures_d <- function(x, y,
                                data = NULL,
                                mu = 0, method = c("rm", "av", "z", "b", "d", "r"),
                                adjust = TRUE,
                                ci = 0.95, alternative = "two.sided",
                                verbose = TRUE, ...) {

  alternative <- .match.alt(alternative)
  method <- match.arg(method)
  data <- .get_data_paired(x, y, data = data, method = method, verbose = verbose, ...)

  if (method %in% c("d", "r")) {
    values <- .replication_d(data, mu = mu, method = method)
  } else {
    values <- .paired_d(data, mu = mu, method = method)
  }

  out <- data.frame(d = values[["d"]])

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level
    probs <- c(alpha / 2, 1 - alpha / 2)
    qs <- stats::qnorm(probs)

    confint <- out[["d"]] + qs * values[["se"]]
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]

    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }


  if (adjust) {
    J <- .J(values[["df"]])

    out[, colnames(out) %in% c("d", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("d", "CI_low", "CI_high")] * J

    attr(out, "table_footer") <- "Adjusted for small sample bias."
  }

  # rename column to method
  colnames(out)[1] <- switch(method,
                             d = "Cohens_d",
                             b = "Beckers_d",
                             paste0("d_", method))

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    mu, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}

#' @rdname repeated_measures_d
#' @export
rm_d <- repeated_measures_d

#' @keywords internal
.paired_d <- function(data, mu, method) {
  x <- data[["x"]]
  y <- data[["y"]]

  m <- mean(x - y)
  n <- length(x)
  df <- n - 1
  r <- stats::cor(x, y)

  if (method == "rm") {
    f <- 2 * (1 - r)

    s <- stats::sd(x - y) / sqrt(f)
    d <- (m - mu) / s

    # Cooper et al., 2009, eq 12.21
    se <- sqrt(((1 / n) + (d ^ 2) / (2 * n)) * f)
  } else if (method == "av") {
    s <- sqrt((stats::var(x) + stats::var(y)) / 2)
    d <- (m - mu) / s

    # TODO fix? validate?
    se <- sqrt((2 / n) + (d ^ 2) / (4 * n))
  } else if (method == "z") {
    s <- stats::sd(x - y)
    d <- (m - mu) / s

    # Hedges and Olkin, 1985, page 86, eq 14
    se <- sqrt((1 / n) + (d ^ 2) / (2 * n))
  } else if (method == "b") {
    s <- stats::sd(y)
    d <- (m - mu) / s

    # Becker 1988, eq. 6
    se <- sqrt((2 * (1 - r) / n) + (d ^ 2) / (2 * n))
  }

  .nlist(d, se, df)
}

#' @keywords internal
.replication_d <- function(data, mu, method) {
  if (method == "r") {
    # for r - need to make sure there are replications!
    cell_ns <- tapply(data[[1]], data[3:2], function(v) length(stats::na.omit(v)))
    all(cell_ns > 1L)
  }

  mod <- suppressWarnings(
    stats::aov(y ~ condition + Error(id / condition), data = data,
               contrasts = list(condition = contr.treatment))
  )
  m <- -unname(coef(mod[["id:condition"]]))
  m_V <- unname(vcov(mod[["id:condition"]])[1])

  pars <- parameters::model_parameters(mod)

  if (method == "d") {
    e <- as.data.frame(pars[pars$Parameter == "Residuals",])
  } else if (method == "r") {
    e <- as.data.frame(pars[pars$Group == "Within",])
  }

  s <- sqrt(sum(e[["Sum_Squares"]]) / sum(e[["df"]]))
  df <- sum(e[["df"]])

  d <- (m - mu) / s
  se <- sqrt(
    (df / (df - 2)) * (m_V / (s ^ 2)) +
      (d ^ 2) * (8 * df ^ 2 - df + 2) / (16 * (df - 2) * ((df - 1) ^ 2))
  )

  .nlist(d, se, df)
}
