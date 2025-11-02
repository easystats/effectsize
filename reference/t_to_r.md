# Convert *t*, *z*, and *F* to Cohen's *d* or **partial**-*r*

These functions are convenience functions to convert t, z and F test
statistics to Cohen's d and **partial** r. These are useful in cases
where the data required to compute these are not easily available or
their computation is not straightforward (e.g., in liner mixed models,
contrasts, etc.).  
See [Effect Size from Test Statistics
vignette.](https://easystats.github.io/effectsize/articles/from_test_statistics.html)

## Usage

``` r
t_to_d(t, df_error, paired = FALSE, ci = 0.95, alternative = "two.sided", ...)

z_to_d(z, n, paired = FALSE, ci = 0.95, alternative = "two.sided", ...)

F_to_d(
  f,
  df,
  df_error,
  paired = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  ...
)

t_to_r(t, df_error, ci = 0.95, alternative = "two.sided", ...)

z_to_r(z, n, ci = 0.95, alternative = "two.sided", ...)

F_to_r(f, df, df_error, ci = 0.95, alternative = "two.sided", ...)
```

## Arguments

- t, f, z:

  The t, the F or the z statistics.

- paired:

  Should the estimate account for the t-value being testing the
  difference between dependent means?

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- ...:

  Arguments passed to or from other methods.

- n:

  The number of observations (the sample size).

- df, df_error:

  Degrees of freedom of numerator or of the error estimate (i.e., the
  residuals).

## Value

A data frame with the effect size(s)(`r` or `d`), and their CIs
(`CI_low` and `CI_high`).

## Details

These functions use the following formulae to approximate *r* and *d*:  
  
\$\$r\_{partial} = t / \sqrt{t^2 + df\_{error}}\$\$  
  
\$\$r\_{partial} = z / \sqrt{z^2 + N}\$\$  
  
\$\$d = 2 \* t / \sqrt{df\_{error}}\$\$  
  
\$\$d_z = t / \sqrt{df\_{error}}\$\$  
  
\$\$d = 2 \* z / \sqrt{N}\$\$

The resulting `d` effect size is an *approximation* to Cohen's *d*, and
assumes two equal group sizes. When possible, it is advised to directly
estimate Cohen's *d*, with
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`emmeans::eff_size()`](https://rvlenth.github.io/emmeans/reference/eff_size.html),
or similar functions.

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

- Friedman, H. (1982). Simplified determinations of statistical power,
  magnitude of effect and research sample sizes. Educational and
  Psychological Measurement, 42(2), 521-526.
  [doi:10.1177/001316448204200214](https://doi.org/10.1177/001316448204200214)

- Wolf, F. M. (1986). Meta-analysis: Quantitative methods for research
  synthesis (Vol. 59). Sage.

- Rosenthal, R. (1994) Parametric measures of effect size. In H. Cooper
  and L.V. Hedges (Eds.). The handbook of research synthesis. New York:
  Russell Sage Foundation.

- Steiger, J. H. (2004). Beyond the F test: Effect size confidence
  intervals and tests of close fit in the analysis of variance and
  contrast analysis. Psychological Methods, 9, 164-182.

- Cumming, G., & Finch, S. (2001). A primer on the understanding, use,
  and calculation of confidence intervals that are based on central and
  noncentral distributions. Educational and Psychological Measurement,
  61(4), 532-574.

## See also

[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)

Other effect size from test statistic:
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md),
[`chisq_to_phi()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)

## Examples

``` r
## t Tests
res <- t.test(1:10, y = c(7:20), var.equal = TRUE)
t_to_d(t = res$statistic, res$parameter)
#> d     |         95% CI
#> ----------------------
#> -2.19 | [-3.23, -1.12]
t_to_r(t = res$statistic, res$parameter)
#> r     |         95% CI
#> ----------------------
#> -0.74 | [-0.85, -0.49]
t_to_r(t = res$statistic, res$parameter, alternative = "less")
#> r     |         95% CI
#> ----------------------
#> -0.74 | [-1.00, -0.54]
#> 
#> - One-sided CIs: lower bound fixed at [-1.00].

res <- with(sleep, t.test(extra[group == 1], extra[group == 2], paired = TRUE))
t_to_d(t = res$statistic, res$parameter, paired = TRUE)
#> d     |         95% CI
#> ----------------------
#> -1.35 | [-2.23, -0.44]
t_to_r(t = res$statistic, res$parameter)
#> r     |         95% CI
#> ----------------------
#> -0.80 | [-0.91, -0.40]
t_to_r(t = res$statistic, res$parameter, alternative = "greater")
#> r     |        95% CI
#> ---------------------
#> -0.80 | [-0.90, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

## Linear Regression
model <- lm(rating ~ complaints + critical, data = attitude)
(param_tab <- parameters::model_parameters(model))
#> Parameter   | Coefficient |    SE |         95% CI | t(27) |      p
#> -------------------------------------------------------------------
#> (Intercept) |       14.25 | 11.17 | [-8.67, 37.18] |  1.28 | 0.213 
#> complaints  |        0.75 |  0.10 | [ 0.55,  0.96] |  7.46 | < .001
#> critical    |    1.91e-03 |  0.14 | [-0.28,  0.28] |  0.01 | 0.989 
#> 
#> Uncertainty intervals (equal-tailed) and p-values (two-tailed)
#>   computed using a Wald t-distribution approximation.

(rs <- t_to_r(param_tab$t[2:3], param_tab$df_error[2:3]))
#> r        |        95% CI
#> ------------------------
#> 0.82     | [ 0.67, 0.89]
#> 2.70e-03 | [-0.35, 0.36]

# How does this compare to actual partial correlations?
correlation::correlation(attitude,
  select = "rating",
  select2 = c("complaints", "critical"),
  partial = TRUE
)
#> # Correlation Matrix (pearson-method)
#> 
#> Parameter1 | Parameter2 |        r |        95% CI | t(28) |         p
#> ----------------------------------------------------------------------
#> rating     | complaints |     0.82 | [ 0.65, 0.91] |  7.60 | < .001***
#> rating     |   critical | 2.70e-03 | [-0.36, 0.36] |  0.01 | 0.989    
#> 
#> p-value adjustment method: Holm (1979)
#> Observations: 30
```
