# Convert \\\chi^2\\ to \\\phi\\ and Other Correlation-like Effect Sizes

Convert between \\\chi^2\\ (chi-square), \\\phi\\ (phi), Cramer's \\V\\,
Tschuprow's \\T\\, Cohen's \\w\\, פ (Fei) and Pearson's \\C\\ for
contingency tables or goodness of fit.

## Usage

``` r
chisq_to_phi(
  chisq,
  n,
  nrow = 2,
  ncol = 2,
  adjust = TRUE,
  ci = 0.95,
  alternative = "greater",
  ...
)

chisq_to_cohens_w(
  chisq,
  n,
  nrow,
  ncol,
  p,
  ci = 0.95,
  alternative = "greater",
  ...
)

chisq_to_cramers_v(
  chisq,
  n,
  nrow,
  ncol,
  adjust = TRUE,
  ci = 0.95,
  alternative = "greater",
  ...
)

chisq_to_tschuprows_t(
  chisq,
  n,
  nrow,
  ncol,
  adjust = TRUE,
  ci = 0.95,
  alternative = "greater",
  ...
)

chisq_to_fei(chisq, n, nrow, ncol, p, ci = 0.95, alternative = "greater", ...)

chisq_to_pearsons_c(
  chisq,
  n,
  nrow,
  ncol,
  ci = 0.95,
  alternative = "greater",
  ...
)

phi_to_chisq(phi, n, ...)
```

## Arguments

- chisq:

  The \\\chi^2\\ (chi-square) statistic.

- n:

  Total sample size.

- nrow, ncol:

  The number of rows/columns in the contingency table.

- adjust:

  Should the effect size be corrected for small-sample bias? Defaults to
  `TRUE`; Advisable for small samples and large tables.

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"greater"` (default) or `"less"` (one-sided CI),
  or `"two.sided"` (two-sided CI). Partial matching is allowed (e.g.,
  `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- ...:

  Arguments passed to or from other methods.

- p:

  Vector of expected values. See
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

- phi:

  The \\\phi\\ (phi) statistic.

## Value

A data frame with the effect size(s), and confidence interval(s). See
[`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md).

## Details

These functions use the following formulas:

\$\$\phi = w = \sqrt{\chi^2 / n}\$\$ \$\$\textrm{Cramer's } V = \phi /
\sqrt{\min(\textit{nrow}, \textit{ncol}) - 1}\$\$

\$\$\textrm{Tschuprow's } T = \phi / \sqrt\[4\]{(\textit{nrow} - 1)
\times (\textit{ncol} - 1)}\$\$

\$\$פ = \phi / \sqrt{\[1 / \min(p_E)\] - 1}\$\$ Where \\p_E\\ are the
expected probabilities.

\$\$\textrm{Pearson's } C = \sqrt{\chi^2 / (\chi^2 + n)}\$\$

For versions adjusted for small-sample bias of \\\phi\\, \\V\\, and
\\T\\, see [Bergsma,
2013](https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V#Bias_correction).

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

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Cumming, G., & Finch, S. (2001). A primer on the understanding, use,
  and calculation of confidence intervals that are based on central and
  noncentral distributions. Educational and Psychological Measurement,
  61(4), 532-574.

- Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M.,
  Lüdecke, D. (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical
  Data That Use the Chi‑Squared Statistic. Mathematics, 11, 1982.
  [doi:10.3390/math11091982](https://doi.org/10.3390/math11091982)

- Bergsma, W. (2013). A bias-correction for Cramer's V and
  Tschuprow's T. Journal of the Korean Statistical Society, 42(3),
  323-328.

- Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
  effect size for chi-squared and likelihood-ratio goodness-of-fit
  tests. Perceptual and motor skills, 103(2), 412-414.

- Rosenberg, M. S. (2010). A generalized formula for converting
  chi-square tests to effect sizes for meta-analysis. PloS one, 5(4),
  e10059.

## See also

[`phi()`](https://easystats.github.io/effectsize/reference/phi.md) for
more details.

Other effect size from test statistic:
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md),
[`t_to_d()`](https://easystats.github.io/effectsize/reference/t_to_r.md)

## Examples

``` r

data("Music_preferences")

# chisq.test(Music_preferences)
#>
#>   Pearson's Chi-squared test
#>
#> data:  Music_preferences
#> X-squared = 95.508, df = 6, p-value < 2.2e-16
#>

chisq_to_cohens_w(95.508,
  n = sum(Music_preferences),
  nrow = nrow(Music_preferences),
  ncol = ncol(Music_preferences)
)
#> Cohen's w |       95% CI
#> ------------------------
#> 0.34      | [0.27, 1.41]
#> 
#> - One-sided CIs: upper bound fixed at [1.41~].




data("Smoking_FASD")

# chisq.test(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#>
#>   Chi-squared test for given probabilities
#>
#> data:  Smoking_FASD
#> X-squared = 7.8521, df = 2, p-value = 0.01972

chisq_to_fei(
  7.8521,
  n = sum(Smoking_FASD),
  nrow = 1,
  ncol = 3,
  p = c(0.015, 0.010, 0.975)
)
#> Fei  |       95% CI
#> -------------------
#> 0.01 | [0.00, 1.00]
#> 
#> - Adjusted for uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].
```
