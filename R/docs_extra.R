#' Confidence Intervals
#'
#' More information regarding Confidence Intervals and how they are computed in
#' `effectsize`.
#'
#' @section Confidence Intervals:
#' Unless stated otherwise, confidence intervals are estimated using the
#' Noncentrality parameter method; These methods searches for a the best
#' non-central parameters (`ncp`s) of the noncentral t-, F- or Chi-squared
#' distribution for the desired tail-probabilities, and then convert these
#' `ncp`s to the corresponding effect sizes. (See full [effectsize-CIs] for
#' more.)
#'
#'
#' @section CI Contains Zero:
#' Keep in mind that confidence intervals used here are inverted test
#' statistics, and only inform us about which values are not significantly
#' different than our sample estimate. (They do *not* inform us about which
#' values are plausible, likely or compatible with our data.) Thus, when CIs
#' contain the value 0, this should not be taken to mean that a null effect size
#' is supported by the data - instead this merely reflects a non-significant
#' test statistic - i.e. *p*-value is greater than alpha (Morey et al., 2016).
#' \cr\cr
#' For positive only effect sizes (Eta squared, Cramer's V, etc.; Effect sizes
#' associated with Chi-squared and F distributions), this applies also to cases
#' where the lower bound of the CI is equal to 0. Even more care should be taken
#' when the *upper* bound is equal to 0 - this merely reflects that the
#' *p*-value is greater than {1−alpha/2} making the upper bound unestimatable,
#' and is arbitrarily sets to 0 (Steiger, 2004). For example:
#' ```{r}
#' eta_squared(aov(mpg ~ factor(gear) + factor(cyl), mtcars[1:7, ]))
#' ```
#'
#'
#' @section CI Does Not Contain the Estimate:
#' For very large sample sizes, the width of the CI can be smaller than the
#' tolerance of the optimizer, resulting in CIs of width 0. This can also,
#' result in the estimated CIs excluding the point estimate. For example:
#' ```{r}
#' chisq_to_cramers_v(13223.73, n = 76227, nrow = 6, ncol = 1)
#'
#' t_to_d(80, df_error = 4555555)
#' ```
#'
#' @references
#' - Morey, R. D., Hoekstra, R., Rouder, J. N., Lee, M. D., & Wagenmakers, E. J. (2016). The fallacy of placing confidence in confidence intervals. Psychonomic bulletin & review, 23(1), 103-123.
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'
#' @rdname effectsize-CIs
#' @name effectsize-CIs
NULL
