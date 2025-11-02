# Cohen's *d* and Other Standardized Differences

Compute effect size indices for standardized mean differences: Cohen's
*d*, Hedges' *g* and Glass’s *delta* (\\\Delta\\). (This function
returns the **population** estimate.) Pair with any reported
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html).  
  
Both Cohen's *d* and Hedges' *g* are the estimated the standardized
difference between the means of two populations. Hedges' *g* provides a
correction for small-sample bias (using the exact method) to Cohen's
*d*. For sample sizes \> 20, the results for both statistics are roughly
equivalent. Glass’s *delta* is appropriate when the standard deviations
are significantly different between the populations, as it uses only the
reference group's standard deviation.

## Usage

``` r
cohens_d(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  reference = NULL,
  adjust = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

hedges_g(
  x,
  y = NULL,
  data = NULL,
  pooled_sd = TRUE,
  mu = 0,
  paired = FALSE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

glass_delta(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  adjust = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)
```

## Arguments

- x, y:

  A numeric vector, or a character name of one in `data`. Any missing
  values (`NA`s) are dropped from the resulting vector. `x` can also be
  a formula (see
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html)), in which
  case `y` is ignored.

- data:

  An optional data frame containing the variables.

- pooled_sd:

  If `TRUE` (default), a
  [`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)
  is used (assuming equal variance). Else the mean SD from both groups
  is used instead.

- mu:

  a number indicating the true value of the mean (or difference in means
  if you are performing a two sample test).

- paired:

  If `TRUE`, the values of `x` and `y` are considered as paired. This
  produces an effect size that is equivalent to the one-sample effect
  size on `x - y`. See also
  [`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)
  for more options.

- reference:

  (Optional) character value of the "group" used as the reference. By
  default, the *second* group is the reference group.

- adjust:

  Should the effect size be adjusted for small-sample bias using Hedges'
  method? Note that `hedges_g()` is an alias for
  `cohens_d(adjust = TRUE)`.

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- verbose:

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to or from other methods. When `x` is a formula,
  these can be `subset` and `na.action`.

## Value

A data frame with the effect size ( `Cohens_d`, `Hedges_g`,
`Glass_delta`) and their CIs (`CI_low` and `CI_high`).

## Details

Set `pooled_sd = FALSE` for effect sizes that are to accompany a Welch's
*t*-test (Delacre et al, 2021).

## Note

The indices here give the population estimated standardized difference.
Some statistical packages give the sample estimate instead (without
applying Bessel's correction).

## Confidence (Compatibility) Intervals (CIs)

Unless stated otherwise, confidence (compatibility) intervals (CIs) are
estimated using the noncentrality parameter method (also called the
"pivot method"). This method finds the noncentrality parameter ("*ncp*")
of a noncentral *t*, *F*, or \\\chi^2\\ distribution that places the
observed *t*, *F*, or \\\chi^2\\ test statistic at the desired
probability point of the distribution. For example, if the observed *t*
statistic is 2.0, with 50 degrees of freedom, for which cumulative
noncentral *t* distribution is *t* = 2.0 the .025 quantile (answer: the
noncentral *t* distribution with *ncp* = .04)? After estimating these
confidence bounds on the *ncp*, they are converted into the effect size
metric to obtain a confidence interval for the effect size (Steiger,
2004).  
  
For additional details on estimation and troubleshooting, see
[effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

## CIs and Significance Tests

"Confidence intervals on measures of effect size convey all the
information in a hypothesis test, and more." (Steiger, 2004). Confidence
(compatibility) intervals and p values are complementary summaries of
parameter uncertainty given the observed data. A dichotomous hypothesis
test could be performed with either a CI or a p value. The 100 (1 -
\\\alpha\\)% confidence interval contains all of the parameter values
for which *p* \> \\\alpha\\ for the current data and model. For example,
a 95% confidence interval contains all of the values for which p \>
.05.  
  
Note that a confidence interval including 0 *does not* indicate that the
null (no effect) is true. Rather, it suggests that the observed data
together with the model and its assumptions combined do not provided
clear evidence against a parameter value of 0 (same as with any other
value in the interval), with the level of this evidence defined by the
chosen \\\alpha\\ level (Rafi & Greenland, 2020; Schweder & Hjort, 2016;
Xie & Singh, 2013). To infer no effect, additional judgments about what
parameter values are "close enough" to 0 to be negligible are needed
("equivalence testing"; Bauer & Kiesser, 1996).

## Plotting with `see`

The `see` package contains relevant plotting functions. See the
[plotting vignette in the `see`
package](https://easystats.github.io/see/articles/effectsize.html).

## References

- Algina, J., Keselman, H. J., & Penfield, R. D. (2006). Confidence
  intervals for an effect size when variances are not equal. Journal of
  Modern Applied Statistical Methods, 5(1), 2.

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021, May 7).
  Why Hedges’ g\*s based on the non-pooled standard deviation should be
  reported with Welch's t-test.
  [doi:10.31234/osf.io/tu6mp](https://doi.org/10.31234/osf.io/tu6mp)

- Hedges, L. V. & Olkin, I. (1985). Statistical methods for
  meta-analysis. Orlando, FL: Academic Press.

- Hunter, J. E., & Schmidt, F. L. (2004). Methods of meta-analysis:
  Correcting error and bias in research findings. Sage.

## See also

[`rm_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md),
[`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md),
[`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md),
[`r_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)

Other standardized differences:
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md),
[`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md),
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

## Examples

``` r
# \donttest{
data(mtcars)
mtcars$am <- factor(mtcars$am)

# Two Independent Samples ----------

(d <- cohens_d(mpg ~ am, data = mtcars))
#> Cohen's d |         95% CI
#> --------------------------
#> -1.48     | [-2.27, -0.67]
#> 
#> - Estimated using pooled SD.
# Same as:
# cohens_d("mpg", "am", data = mtcars)
# cohens_d(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])

# More options:
cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
#> Cohen's d |         95% CI
#> --------------------------
#> -1.41     | [-2.26, -0.53]
#> 
#> - Estimated using un-pooled SD.
cohens_d(mpg ~ am, data = mtcars, mu = -5)
#> Cohen's d |        95% CI
#> -------------------------
#> -0.46     | [-1.17, 0.26]
#> 
#> - Deviation from a difference of -5.
#> - Estimated using pooled SD.
cohens_d(mpg ~ am, data = mtcars, alternative = "less")
#> Cohen's d |        95% CI
#> -------------------------
#> -1.48     | [-Inf, -0.80]
#> 
#> - Estimated using pooled SD.
#> - One-sided CIs: lower bound fixed at [-Inf].
hedges_g(mpg ~ am, data = mtcars)
#> Hedges' g |         95% CI
#> --------------------------
#> -1.44     | [-2.21, -0.65]
#> 
#> - Estimated using pooled SD.
glass_delta(mpg ~ am, data = mtcars)
#> Glass' delta (adj.) |         95% CI
#> ------------------------------------
#> -1.10               | [-1.80, -0.37]


# One Sample ----------

cohens_d(wt ~ 1, data = mtcars)
#> Cohen's d |       95% CI
#> ------------------------
#> 3.29      | [2.40, 4.17]

# same as:
# cohens_d("wt", data = mtcars)
# cohens_d(mtcars$wt)

# More options:
cohens_d(wt ~ 1, data = mtcars, mu = 3)
#> Cohen's d |        95% CI
#> -------------------------
#> 0.22      | [-0.13, 0.57]
#> 
#> - Deviation from a difference of 3.
hedges_g(wt ~ 1, data = mtcars, mu = 3)
#> Hedges' g |        95% CI
#> -------------------------
#> 0.22      | [-0.13, 0.56]
#> 
#> - Deviation from a difference of 3.


# Paired Samples ----------

data(sleep)

cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep)
#> For paired samples, 'repeated_measures_d()' provides more options.
#> Cohen's d |         95% CI
#> --------------------------
#> -1.28     | [-2.12, -0.41]

# same as:
# cohens_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], paired = TRUE)
# cohens_d(sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2])
# rm_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], method = "z", adjust = FALSE)

# More options:
cohens_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, mu = -1, verbose = FALSE)
#> Cohen's d |        95% CI
#> -------------------------
#> -0.47     | [-1.12, 0.20]
#> 
#> - Deviation from a difference of -1.
hedges_g(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, verbose = FALSE)
#> Hedges' g |         95% CI
#> --------------------------
#> -1.17     | [-1.94, -0.38]


# Interpretation -----------------------
interpret_cohens_d(-1.48, rules = "cohen1988")
#> [1] "large"
#> (Rules: cohen1988)
#> 
interpret_hedges_g(-1.48, rules = "sawilowsky2009")
#> [1] "very large"
#> (Rules: sawilowsky2009)
#> 
interpret_glass_delta(-1.48, rules = "gignac2016")
#> [1] "large"
#> (Rules: gignac2016)
#> 
# Or:
interpret(d, rules = "sawilowsky2009")
#> Cohen's d |         95% CI | Interpretation
#> -------------------------------------------
#> -1.48     | [-2.27, -0.67] |     very large
#> 
#> - Estimated using pooled SD.
#> - Interpretation rule: sawilowsky2009

# Common Language Effect Sizes
d_to_u3(1.48)
#> [1] 0.9305634
# Or:
print(d, append_CLES = TRUE)
#> Cohen's d |         95% CI
#> --------------------------
#> -1.48     | [-2.27, -0.67]
#> 
#> - Estimated using pooled SD.
#> 
#> ## Common Language Effect Sizes:
#> Name            | CLES |       95% CI
#> -------------------------------------
#> Pr(superiority) | 0.15 | [0.05, 0.32]
#> Cohen's U1      | 0.70 | [0.42, 0.85]
#> Cohen's U2      | 0.77 | [0.63, 0.87]
#> Cohen's U3      | 0.07 | [0.01, 0.25]
#> Overlap         | 0.46 | [0.26, 0.74]
# }
```
