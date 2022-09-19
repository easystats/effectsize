#' Confidence (Compatibility) Intervals
#'
#' More information regarding Confidence (Compatibiity) Intervals and how
#' they are computed in *effectsize*.
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Unless stated otherwise, confidence (compatibility) intervals (CIs) are
#' estimated using the noncentrality parameter method (also called the "pivot
#' method"). This method finds the noncentrality parameter ("*ncp*") of a
#' noncentral *t*, *F*, or \eqn{\chi^2} distribution that places the observed
#' *t*, *F*, or \eqn{\chi^2} test statistic at the desired probability point of
#' the distribution. For example, if the observed *t* statistic is 2.0, with 50
#' degrees of freedom, for which cumulative noncentral *t* distribution is *t* =
#' 2.0 the .025 quantile (answer: the noncentral *t* distribution with *ncp* =
#' .04)? After estimating these confidence bounds on the *ncp*, they are
#' converted into the effect size metric to obtain a confidence interval for the
#' effect size (Steiger, 2004).
#' \cr\cr
#' For additional details on estimation and troubleshooting, see [effectsize_CIs].
#'
#' # CIs and Significance Tests
#' "Confidence intervals on measures of effect size convey all the information
#' in a hypothesis test, and more." (Steiger, 2004). Confidence (compatibility)
#' intervals and p values are complementary summaries of parameter uncertainty
#' given the observed data. A dichotomous hypothesis test could be performed
#' with either a CI or a p value. The 100 (1 - \eqn{\alpha})% confidence
#' interval contains all of the parameter values for which *p* > \eqn{\alpha}
#' for the current data and model. For example, a 95% confidence interval
#' contains all of the values for which p > .05.
#' \cr\cr
#' Note that a confidence interval including 0 *does not* indicate that the null
#' (no effect) is true. Rather, it suggests that the observed data together with
#' the model and its assumptions combined do not provided clear evidence against
#' a parameter value of 0 (same as with any other value in the interval), with
#' the level of this evidence defined by the chosen \eqn{\alpha} level (Rafi &
#' Greenland, 2020; Schweder & Hjort, 2016; Xie & Singh, 2013). To infer no
#' effect, additional judgments about what parameter values are "close enough"
#' to 0 to be negligible are needed ("equivalence testing"; Bauer & Kiesser,
#' 1996).
#'
#' # Bootstrapped CIs
#' Some effect sizes are directionless--they do have a minimum value that would
#' be interpreted as "no effect", but they cannot cross it. For example, a null
#' value of [Kendall's W][kendalls_w()] is 0, indicating no difference between
#' groups, but it can never have a negative value. Same goes for
#' [U2][cohens_u2()] and [Overlap][p_overlap()]: the null value of \eqn{U_2} is
#' 0.5, but it can never be smaller than 0.5; am *Overlap* of 1 means "full
#' overlap" (no difference), but it cannot be larger than 1.
#' \cr\cr
#' When bootstrapping CIs for such effect sizes, the bounds of the CIs will
#' never cross (and often will never cover) the null. Therefore, these CIs
#' should not be used for statistical inference.
#'
#' # One-Sided CIs
#' Typically, CIs are constructed as two-tailed intervals, with an equal
#' proportion of the cumulative probability distribution above and below the
#' interval. CIs can also be constructed as *one-sided* intervals,
#' giving only a lower bound or upper bound. This is analogous to computing a
#' 1-tailed *p* value or conducting a 1-tailed hypothesis test.
#' \cr\cr
#' Significance tests conducted using CIs (whether a value is inside the interval)
#' and using *p* values (whether p < alpha for that value) are only guaranteed
#' to agree when both are constructed using the same number of sides/tails.
#' \cr\cr
#' Most effect sizes are not bounded by zero (e.g., *r*, *d*, *g*), and as such
#' are generally tested using 2-tailed tests and 2-sided CIs.
#' \cr\cr
#' Some effect sizes are strictly positive--they do have a minimum value, of 0.
#' For example, \eqn{R^2}, \eqn{\eta^2}, and other variance-accounted-for effect
#' sizes, as well as Cramer's *V* and multiple *R*, range from 0 to 1. These
#' typically involve *F*- or \eqn{\chi^2}-statistics and are generally tested
#' using *1-tailed* tests which test whether the estimated effect size is
#' *larger* than the hypothesized null value (e.g., 0). In order for a CI to
#' yield the same significance decision it must then by a *1-sided* CI,
#' estimating only a lower bound. This is the default CI computed by
#' *effectsize* for these effect sizes, where `alternative = "greater"` is set.
#' \cr\cr
#' This lower bound interval indicates the smallest effect size that is not
#' significantly different from the observed effect size. That is, it is the
#' minimum effect size compatible with the observed data, background model
#' assumptions, and \eqn{\alpha} level. This type of interval does not indicate
#' a maximum effect size value; anything up to the maximum possible value of the
#' effect size (e.g., 1) is in the interval.
#' \cr\cr
#' One-sided CIs can also be used to test against a maximum effect size value
#' (e.g., is \eqn{R^2} significantly smaller than a perfect correlation of 1.0?)
#' can by setting `alternative = "less"`. This estimates a CI with only an
#' *upper* bound; anything from the minimum possible value of the effect size
#' (e.g., 0) up to this upper bound is in the interval.
#' \cr\cr
#' We can also obtain a 2-sided interval by setting `alternative = "two.sided"`.
#' These intervals can be interpreted in the same way as other 2-sided
#' intervals, such as those for *r*, *d*, or *g*.
#' \cr\cr
#' An alternative approach to aligning significance tests using CIs and 1-tailed
#' *p* values that can often be found in the literature is to construct a
#' 2-sided CI at a lower confidence level (e.g., 100(1-2\eqn{\alpha})% = 100 -
#' 2*5% = 90%. This estimates the lower bound and upper bound for the above
#' 1-sided intervals simultaneously. These intervals are commonly reported when
#' conducting **equivalence tests**. For example, a 90% 2-sided interval gives
#' the bounds for an equivalence test with \eqn{\alpha} = .05. However, be aware
#' that this interval does not give 95% coverage for the underlying effect size
#' parameter value. For that, construct a 95% 2-sided CI.
#'
#' ```{r}
#' data("hardlyworking")
#' fit <- lm(salary ~ n_comps + age, data = hardlyworking)
#' eta_squared(fit) # default, ci = 0.95, alternative = "greater"
#' eta_squared(fit, alternative = "less") # Test is eta is smaller than some value
#' eta_squared(fit, alternative = "two.sided") # 2-sided bounds for alpha = .05
#' eta_squared(fit, ci = 0.9, alternative = "two.sided") # both 1-sided bounds for alpha = .05
#' ```
#'
#' # CI Does Not Contain the Estimate
#' For very large sample sizes or effect sizes, the width of the CI can be
#' smaller than the tolerance of the optimizer, resulting in CIs of width 0.
#' This can also result in the estimated CIs excluding the point estimate.
#'
#' For example:
#' ```{r}
#' t_to_d(80, df_error = 4555555)
#' ```
#'
#' In these cases, consider an alternative optimizer, or an alternative method
#' for computing CIs, such as the bootstrap.
#'
#'
#' @references
#' Bauer, P., & Kieser, M. (1996).
#' A unifying approach for confidence intervals and testing of equivalence and difference.
#' _Biometrika, 83_(4), 934-–937.
#' \doi{10.1093/biomet/83.4.934}
#'
#' Rafi, Z., & Greenland, S. (2020).
#' Semantic and cognitive tools to aid statistical science: Replace confidence and significance by compatibility and surprise.
#' _BMC Medical Research Methodology, 20_(1), Article 244.
#' \doi{10.1186/s12874-020-01105-9}
#'
#' Schweder, T., & Hjort, N. L. (2016).
#' _Confidence, likelihood, probability: Statistical inference with confidence distributions._
#' Cambridge University Press.
#' \doi{10.1017/CBO9781139046671}
#'
#' Steiger, J. H. (2004).
#' Beyond the _F_ test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis.
#' _Psychological Methods, 9_(2), 164--182.
#' \doi{10.1037/1082-989x.9.2.164}
#'
#' Xie, M., & Singh, K. (2013).
#' Confidence distribution, the frequentist distribution estimator of a parameter: A review.
#' _International Statistical Review, 81_(1), 3–-39.
#' \doi{10.1111/insr.12000}
#'
#' @rdname effectsize_CIs
#' @name effectsize_CIs
NULL


#' `effectsize` API
#'
#' Read the [*Support functions for model extensions*](https://easystats.github.io/effectsize/articles/effectsize_API.html) vignette.
#'
#' @rdname effectsize_API
#' @name effectsize_API
NULL
