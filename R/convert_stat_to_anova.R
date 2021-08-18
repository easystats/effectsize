#' Convert test statistics (F, t) to indices of **partial** variance explained
#' (**partial** Eta / Omega / Epsilon squared and Cohen's f)
#'
#' These functions are convenience functions to convert F and t test statistics
#' to **partial** Eta- (\eqn{\eta}), Omega- (\eqn{\omega}) Epsilon-
#' (\eqn{\epsilon}) squared (an alias for the adjusted Eta squared) and Cohen's
#' f. These are useful in cases where the various Sum of Squares and Mean
#' Squares are not easily available or their computation is not straightforward
#' (e.g., in liner mixed models, contrasts, etc.). For test statistics derived
#' from `lm` and `aov` models, these functions give exact results. For all other
#' cases, they return close approximations.
#' \cr
#' See [Effect Size from Test Statistics vignette.](https://easystats.github.io/effectsize/articles/from_test_statistics.html)
#'
#' @param t,f The t or the F statistics.
#' @param df,df_error Degrees of freedom of numerator or of the error estimate
#'   (i.e., the residuals).
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size(s) between 0-1 (`Eta2_partial`,
#'   `Epsilon2_partial`, `Omega2_partial`, `Cohens_f_partial` or
#'   `Cohens_f2_partial`), and their CIs (`CI_low` and `CI_high`). (Note that
#'   for \eqn{\omega_p^2}{Omega2p} and \eqn{\epsilon_p^2}{Epsilon2p} it is possible to compute a
#'   negative number; even though this doesn't make any practical sense, it is
#'   recommended to report the negative number and not a 0).
#'
#' @details These functions use the following formulae:
#' \cr
#' \deqn{\eta_p^2 = \frac{F \times df_{num}}{F \times df_{num} + df_{den}}}{\eta^2_p = F * df1 / (F * df1 + df2)}
#' \cr
#' \deqn{\epsilon_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den}}}{\epsilon^2_p = (F - 1) * df1 / (F * df1 + df2)}
#' \cr
#' \deqn{\omega_p^2 = \frac{(F - 1) \times df_{num}}{F \times df_{num} + df_{den} + 1}}{\omega^2_p=(F - 1) * df1 / (F * df1 + df2 + 1)}
#' \cr
#' \deqn{f_p = \sqrt{\frac{\eta_p^2}{1-\eta_p^2}}}{f = \eta^2 / (1 - \eta^2)}
#' \cr\cr
#' For *t*, the conversion is based on the equality of \eqn{t^2 = F} when \eqn{df_{num}=1}{df1 = 1}.
#'
#' ## Choosing an Un-Biased Estimate
#' Both Omega and Epsilon are unbiased estimators of the population Eta. But
#' which to choose? Though Omega is the more popular choice, it should be noted
#' that:
#' 1. The formula given above for Omega is only an approximation for complex
#' designs.
#' 2. Epsilon has been found to be less biased (Carroll & Nordholm, 1975).
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @note Adjusted (partial) Eta-squared is an alias for (partial) Epsilon-squared.
#'
#' @seealso [eta_squared()] for more details.
#' @family effect size from test statistic
#'
#' @examples
#' \donttest{
#' if (require("afex")) {
#'   data(md_12.1)
#'   aov_ez("id", "rt", md_12.1,
#'     within = c("angle", "noise"),
#'     anova_table = list(correction = "none", es = "pes")
#'   )
#' }
#' # compare to:
#' (etas <- F_to_eta2(
#'   f = c(40.72, 33.77, 45.31),
#'   df = c(2, 1, 2),
#'   df_error = c(18, 9, 18)
#' ))
#'
#' if (require(see)) plot(etas)
#'
#'
#' if (require("lmerTest")) { # for the df_error
#'   fit <- lmer(extra ~ group + (1 | ID), sleep)
#'   # anova(fit)
#'   # #> Type III Analysis of Variance Table with Satterthwaite's method
#'   # #>       Sum Sq Mean Sq NumDF DenDF F value   Pr(>F)
#'   # #> group 12.482  12.482     1     9  16.501 0.002833 **
#'   # #> ---
#'   # #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#'
#'   F_to_eta2(16.501, 1, 9)
#'   F_to_omega2(16.501, 1, 9)
#'   F_to_epsilon2(16.501, 1, 9)
#'   F_to_f(16.501, 1, 9)
#' }
#'
#'
#' ## Use with emmeans based contrasts
#' ## --------------------------------
#' if (require(emmeans)) {
#'   warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
#'
#'   jt <- joint_tests(warp.lm, by = "wool")
#'   F_to_eta2(jt$F.ratio, jt$df1, jt$df2)
#' }
#' }
#' @references
#' - Albers, C., & Lakens, D. (2018). When power analyses based on pilot data
#' are biased: Inaccurate effect size estimators and follow-up bias. Journal of
#' experimental social psychology, 74, 187-195. \doi{10.31234/osf.io/b7z4q}
#'
#' - Carroll, R. M., & Nordholm, L. A. (1975). Sampling Characteristics of
#' Kelley's epsilon and Hays' omega. Educational and Psychological Measurement,
#' 35(3), 541-554.
#'
#' - Cumming, G., & Finch, S. (2001). A primer on the understanding, use, and
#' calculation of confidence intervals that are based on central and noncentral
#' distributions. Educational and Psychological Measurement, 61(4), 532-574.
#'
#' - Friedman, H. (1982). Simplified determinations of statistical power,
#' magnitude of effect and research sample sizes. Educational and Psychological
#' Measurement, 42(2), 521-526. \doi{10.1177/001316448204200214}
#'
#' - Mordkoff, J. T. (2019). A Simple Method for Removing Bias From a Popular
#' Measure of Standardized Effect Size: Adjusted Partial Eta Squared. Advances
#' in Methods and Practices in Psychological Science, 2(3), 228-232.
#' \doi{10.1177/2515245919855053}
#'
#' - Morey, R. D., Hoekstra, R., Rouder, J. N., Lee, M. D., & Wagenmakers, E. J.
#' (2016). The fallacy of placing confidence in confidence intervals.
#' Psychonomic bulletin & review, 23(1), 103-123.
#'
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
#' and tests of close fit in the analysis of variance and contrast analysis.
#' Psychological Methods, 9, 164-182.
#'
#' @export
F_to_eta2 <- function(f, df, df_error, ci = 0.95, alternative = "greater", ...) {
  .F_to_pve(f, df, df_error, ci = ci, alternative = alternative, es = "eta2")
}

#' @rdname F_to_eta2
#' @export
t_to_eta2 <- function(t, df_error, ci = 0.95, alternative = "greater", ...) {
  F_to_eta2(t^2, 1, df_error, ci = ci)
}

#' @rdname F_to_eta2
#' @export
F_to_epsilon2 <- function(f, df, df_error, ci = 0.95, alternative = "greater", ...) {
  .F_to_pve(f, df, df_error, ci = ci, alternative = alternative, es = "epsilon2")
}

#' @rdname F_to_eta2
#' @export
t_to_epsilon2 <- function(t, df_error, ci = 0.95, alternative = "greater", ...) {
  F_to_epsilon2(t^2, 1, df_error, ci = ci)
}

#' @rdname F_to_eta2
#' @export
F_to_eta2_adj <- F_to_epsilon2

#' @rdname F_to_eta2
#' @export
t_to_eta2_adj <- t_to_epsilon2

#' @rdname F_to_eta2
#' @export
F_to_omega2 <- function(f, df, df_error, ci = 0.95, alternative = "greater", ...) {
  .F_to_pve(f, df, df_error, ci = ci, alternative = alternative, es = "omega2")
}

#' @rdname F_to_eta2
#' @export
t_to_omega2 <- function(t, df_error, ci = 0.95, alternative = "greater", ...) {
  F_to_omega2(t^2, 1, df_error, ci = ci)
}


#' @rdname F_to_eta2
#' @param squared Return Cohen's *f* or Cohen's *f*-squared?
#' @export
F_to_f <- function(f, df, df_error, ci = 0.95, alternative = "greater", squared = FALSE, ...) {
  res_eta <- F_to_eta2(f, df, df_error, ci = ci, alternative = alternative)

  res <- data.frame(
    Cohens_f2_partial =
      res_eta$Eta2_partial / (1 - res_eta$Eta2_partial)
  )

  ci_method <- NULL
  if (is.numeric(ci)) {
    res$CI <- res_eta$CI
    res$CI_low <- res_eta$CI_low / (1 - res_eta$CI_low)
    res$CI_high <- res_eta$CI_high / (1 - res_eta$CI_high)

    ci_method <- list(method = "ncp", distribution = "F")
  }

  if (!squared) {
    i <- colnames(res) %in% c("Cohens_f2_partial", "CI_low", "CI_high")
    res[i] <- sqrt(res[i])
    colnames(res)[colnames(res) == "Cohens_f2_partial"] <- "Cohens_f_partial"
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- if (is.numeric(ci)) alternative
  return(res)
}


#' @rdname F_to_eta2
#' @export
t_to_f <- function(t, df_error, ci = 0.95, alternative = "greater", squared = FALSE, ...) {
  F_to_f(t^2, 1, df_error, ci = ci, alternative = alternative, squared = squared)
}

#' @rdname F_to_eta2
#' @export
F_to_f2 <- function(f, df, df_error, ci = 0.95, alternative = "greater", squared = TRUE, ...) {
  F_to_f(f, df = df, df_error = df_error, ci = ci, alternative = alternative, squared = squared)
}

#' @rdname F_to_eta2
#' @export
t_to_f2 <- function(t, df_error, ci = 0.95, alternative = "greater", squared = TRUE, ...) {
  F_to_f(t^2, 1, df_error, ci = ci, alternative = alternative, squared = squared)
}


#' @keywords internal
.F_to_pve <- function(f, df, df_error, ci = 0.95, alternative = "greater", es = "eta2") {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  res <- switch(tolower(es),
    eta2 = data.frame(Eta2_partial = (f * df) / (f * df + df_error)),
    epsilon2 = data.frame(Epsilon2_partial = ((f - 1) * df) / (f * df + df_error)),
    omega2 = data.frame(Omega2_partial = ((f - 1) * df) / (f * df + df_error + 1)),
    stop("'es' must be 'eta2', 'epsilon2', or 'omega2'.")
  )

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    # based on MBESS::ci.R2
    f <- pmax(0, (res[[1]] / df) / ((1 - res[[1]]) / df_error))
    fs <- t(mapply(.get_ncp_F, f, df, df_error, ci.level))

    # This really is a generic F_to_R2
    res$CI_low <- F_to_eta2(fs[, 1], df, df_error, ci = NULL)[[1]]
    res$CI_high <- F_to_eta2(fs[, 2], df, df_error, ci = NULL)[[1]]

    ci_method <- list(method = "ncp", distribution = "F")
    if (alternative == "less") {
      res$CI_low <- 0
    } else if (alternative == "greater") {
      res$CI_high <- 1
    }
  } else {
    alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}
