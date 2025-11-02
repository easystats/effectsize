# Mahalanobis' *D* (a multivariate Cohen's *d*)

Compute effect size indices for standardized difference between two
normal multivariate distributions or between one multivariate
distribution and a defined point. This is the standardized effect size
for Hotelling's \\T^2\\ test (e.g., `DescTools::HotellingsT2Test()`).
*D* is computed as:  
  
\$\$D = \sqrt{(\bar{X}\_1-\bar{X}\_2-\mu)^T \Sigma_p^{-1}
(\bar{X}\_1-\bar{X}\_2-\mu)}\$\$  
  
Where \\\bar{X}\_i\\ are the column means, \\\Sigma_p\\ is the *pooled*
covariance matrix, and \\\mu\\ is a vector of the null differences for
each variable. When there is only one variate, this formula reduces to
Cohen's *d*.

## Usage

``` r
mahalanobis_d(
  x,
  y = NULL,
  data = NULL,
  pooled_cov = TRUE,
  mu = 0,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)
```

## Arguments

- x, y:

  A data frame or matrix. Any incomplete observations (with `NA` values)
  are dropped. `x` can also be a formula (see details) in which case `y`
  is ignored.

- data:

  An optional data frame containing the variables.

- pooled_cov:

  Should equal covariance be assumed? Currently only `pooled_cov = TRUE`
  is supported.

- mu:

  A named list/vector of the true difference in means for each variable.
  Can also be a vector of length 1, which will be recycled.

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

  Not used.

## Value

A data frame with the `Mahalanobis_D` and potentially its CI (`CI_low`
and `CI_high`).

## Details

To specify a `x` as a formula:

- Two sample case: `DV1 + DV2 ~ group` or `cbind(DV1, DV2) ~ group`

- One sample case: `DV1 + DV2 ~ 1` or `cbind(DV1, DV2) ~ 1`

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

- Del Giudice, M. (2017). Heterogeneity coefficients for Mahalanobis' D
  as a multivariate effect size. Multivariate Behavioral Research,
  52(2), 216-221.

- Mahalanobis, P. C. (1936). On the generalized distance in statistics.
  National Institute of Science of India.

- Reiser, B. (2001). Confidence intervals for the Mahalanobis distance.
  Communications in Statistics-Simulation and Computation, 30(1), 37-45.

## See also

[`stats::mahalanobis()`](https://rdrr.io/r/stats/mahalanobis.html),
[`cov_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)

Other standardized differences:
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md),
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

## Examples

``` r
## Two samples --------------
mtcars_am0 <- subset(mtcars, am == 0,
  select = c(mpg, hp, cyl)
)
mtcars_am1 <- subset(mtcars, am == 1,
  select = c(mpg, hp, cyl)
)

mahalanobis_d(mtcars_am0, mtcars_am1)
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 2.14           | [1.22, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

# Or
mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars)
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 2.14           | [1.22, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars, alternative = "two.sided")
#> Mahalanobis' D |       95% CI
#> -----------------------------
#> 2.14           | [1.07, 2.90]

# Different mu:
mahalanobis_d(mpg + hp + cyl ~ am,
  data = mtcars,
  mu = c(mpg = -4, hp = 15, cyl = 0)
)
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 1.90           | [1.00, Inf]
#> 
#> - Deviation from a difference of 15.5242.
#> - One-sided CIs: upper bound fixed at [Inf].


# D is a multivariate d, so when only 1 variate is provided:
mahalanobis_d(hp ~ am, data = mtcars)
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 0.49           | [0.00, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

cohens_d(hp ~ am, data = mtcars)
#> Cohen's d |        95% CI
#> -------------------------
#> 0.49      | [-0.23, 1.21]
#> 
#> - Estimated using pooled SD.


# One sample ---------------------------
mahalanobis_d(mtcars[, c("mpg", "hp", "cyl")])
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 12.59          | [9.49, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

# Or
mahalanobis_d(mpg + hp + cyl ~ 1,
  data = mtcars,
  mu = c(mpg = 15, hp = 5, cyl = 3)
)
#> Mahalanobis' D |      95% CI
#> ----------------------------
#> 5.31           | [3.97, Inf]
#> 
#> - Deviation from a difference of 16.0935.
#> - One-sided CIs: upper bound fixed at [Inf].
```
