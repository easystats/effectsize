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
#' Keep in mind that `ncp` confidence intervals are inverted significance tests,
#' and only inform us about which values are not significantly different than
#' our sample estimate. (They do *not* inform us about which values are
#' plausible, likely or compatible with our data.) Thus, when CIs contain the
#' value 0, this should *not* be taken to mean that a null effect size is
#' supported by the data; Instead this merely reflects a non-significant test
#' statistic - i.e. the *p*-value is greater than alpha (Morey et al., 2016).
#' \cr\cr
#' For positive only effect sizes (Eta squared, Cramer's V, etc.; Effect sizes
#' associated with Chi-squared and F distributions), this applies also to cases
#' where the lower bound of the CI is equal to 0. Even more care should be taken
#' when the *upper* bound is equal to 0 - this occurs when *p*-value is greater
#' than 1-alpha/2 making, the upper bound cannot be estimated, and the upper
#' bound is arbitrarily set to 0 (Steiger, 2004). For example:
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
#' @section Choosing the Appropriate CI Level:
#' "Confidence intervals on measures of effect size convey all the information
#' in a hypothesis test, and more" (Steiger, 2004). Essentially, a hypothesis
#' test can be preformed by inspecting the CI - if it excludes the null
#' hypothesized value, then we can conclude that the effect size is
#' significantly different from this value. For 2-sided tests, such as those
#' involving *t*- or *z*-statistics, the CI level should complement the
#' significance level. So if \eqn{\alpha} = 5%, then the confidence level should
#' be 1-\eqn{\alpha} = 1-5% = 95%.
#' \cr\cr
#' However, one-sided hypothesis tests, such as those involving *F*- or
#' \eqn{\chi^2}-statistics (or one-tailed *t*- or *z*-tests), a two sided CI can
#' include the null value even when the test itself is significant. One common
#' solution is to use a two sided CI with a confidence level of 1-2\eqn{\alpha}
#' = 1-2*5% = 90%. This is why across `effectsize`, effect sizes associated with
#' *F*- or \eqn{\chi^2}-statistics (Cramer's *V*, \eqn{\eta^2}, ...) default to
#' a 90% CI.
#' \cr\cr
#' The problem with this solution is that it doesn't actually give 95% coverage
#' on the estimated effect size. To this end we can use a one-sided 95% CI. This
#' can be done by setting the upper bound of the CI to `Inf`. For example:
#' ```{r}
#' data("hardlyworking")
#' fit <- lm(salary ~ n_comps + age, data = hardlyworking)
#' etasq <- eta_squared(fit, ci = 0.90)
#' etasq[["CI"]] <- 0.95
#' etasq[["CI_high"]] <- Inf
#' etasq
#' ```
#'
#' @references
#' - Morey, R. D., Hoekstra, R., Rouder, J. N., Lee, M. D., & Wagenmakers, E. J. (2016). The fallacy of placing confidence in confidence intervals. Psychonomic bulletin & review, 23(1), 103-123.
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'
#' @rdname effectsize-CIs
#' @name effectsize-CIs
NULL
