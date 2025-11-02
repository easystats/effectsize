# \\\phi\\ and Other Contingency Tables Correlations

Compute phi (\\\phi\\), Cramer's *V*, Tschuprow's *T*, Cohen's *w*, פ
(Fei), Pearson's contingency coefficient for contingency tables or
goodness-of-fit. Pair with any reported
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

## Usage

``` r
phi(x, y = NULL, adjust = TRUE, ci = 0.95, alternative = "greater", ...)

cramers_v(x, y = NULL, adjust = TRUE, ci = 0.95, alternative = "greater", ...)

tschuprows_t(
  x,
  y = NULL,
  adjust = TRUE,
  ci = 0.95,
  alternative = "greater",
  ...
)

cohens_w(
  x,
  y = NULL,
  p = rep(1, length(x)),
  ci = 0.95,
  alternative = "greater",
  ...
)

fei(x, p = rep(1, length(x)), ci = 0.95, alternative = "greater", ...)

pearsons_c(
  x,
  y = NULL,
  p = rep(1, length(x)),
  ci = 0.95,
  alternative = "greater",
  ...
)
```

## Arguments

- x:

  a numeric vector or matrix. `x` and `y` can also both be factors.

- y:

  a numeric vector; ignored if `x` is a matrix. If `x` is a factor, `y`
  should be a factor of the same length.

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

  Ignored.

- p:

  a vector of probabilities of the same length as `x`. An error is given
  if any entry of `p` is negative.

## Value

A data frame with the effect size (`Cramers_v`, `phi` (possibly with the
suffix `_adjusted`), `Cohens_w`, `Fei`) and its CIs (`CI_low` and
`CI_high`).

## Details

phi (\\\phi\\), Cramer's *V*, Tschuprow's *T*, Cohen's *w*, and
Pearson's *C* are effect sizes for tests of independence in 2D
contingency tables. For 2-by-2 tables, phi, Cramer's *V*, Tschuprow's
*T*, and Cohen's *w* are identical, and are equal to the simple
correlation between two dichotomous variables, ranging between 0 (no
dependence) and 1 (perfect dependence).  
  
For larger tables, Cramer's *V*, Tschuprow's *T* or Pearson's *C* should
be used, as they are bounded between 0-1. (Cohen's *w* can also be used,
but since it is not bounded at 1 (can be larger) its interpretation is
more difficult.) For square table, Cramer's *V* and Tschuprow's *T* give
the same results, but for non-square tables Tschuprow's *T* is more
conservative: while *V* will be 1 if either columns are fully dependent
on rows (for each column, there is only one non-0 cell) *or* rows are
fully dependent on columns, *T* will only be 1 if both are true.  
  
For goodness-of-fit in 1D tables Cohen's *W*, פ (Fei) or Pearson's *C*
can be used. Cohen's *w* has no upper bound (can be arbitrarily large,
depending on the expected distribution). *Fei* is an adjusted Cohen's
*w*, accounting for the expected distribution, making it bounded between
0-1 (Ben-Shachar et al, 2023). Pearson's *C* is also bounded between
0-1.  
  
To summarize, for correlation-like effect sizes, we recommend:

- For a 2x2 table, use `phi()`

- For larger tables, use `cramers_v()`

- For goodness-of-fit, use `fei()`

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

- Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M.,
  Lüdecke, D. (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical
  Data That Use the Chi‑Squared Statistic. Mathematics, 11, 1982.
  [doi:10.3390/math11091982](https://doi.org/10.3390/math11091982)

- Johnston, J. E., Berry, K. J., & Mielke Jr, P. W. (2006). Measures of
  effect size for chi-squared and likelihood-ratio goodness-of-fit
  tests. Perceptual and motor skills, 103(2), 412-414.

- Rosenberg, M. S. (2010). A generalized formula for converting
  chi-square tests to effect sizes for meta-analysis. PloS one, 5(4),
  e10059.

## See also

[`chisq_to_phi()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)
for details regarding estimation and CIs.

Other effect sizes for contingency table:
[`cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.md),
[`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md)

## Examples

``` r

## 2-by-2 tables
## -------------
data("RCT_table")
RCT_table # note groups are COLUMNS
#>            Group
#> Diagnosis   Treatment Control
#>   Sick             71      30
#>   Recovered        50     100

phi(RCT_table)
#> Phi (adj.) |       95% CI
#> -------------------------
#> 0.36       | [0.25, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
pearsons_c(RCT_table)
#> Pearson's C |       95% CI
#> --------------------------
#> 0.34        | [0.25, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].



## Larger tables
## -------------
data("Music_preferences")
Music_preferences
#>       Pop Rock Jazz Classic
#> Psych 150  100  165     130
#> Econ   50   65   35      10
#> Law     2   55   40      25

cramers_v(Music_preferences)
#> Cramer's V (adj.) |       95% CI
#> --------------------------------
#> 0.23              | [0.18, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

cohens_w(Music_preferences)
#> Cohen's w |       95% CI
#> ------------------------
#> 0.34      | [0.27, 1.41]
#> 
#> - One-sided CIs: upper bound fixed at [1.41~].

pearsons_c(Music_preferences)
#> Pearson's C |       95% CI
#> --------------------------
#> 0.32        | [0.26, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].



## Goodness of fit
## ---------------
data("Smoking_FASD")
Smoking_FASD
#>  FAS PFAS   TD 
#>   17   11  640 

fei(Smoking_FASD)
#> Fei  |       95% CI
#> -------------------
#> 0.94 | [0.89, 1.00]
#> 
#> - Adjusted for non-uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].

cohens_w(Smoking_FASD)
#> Cohen's w |       95% CI
#> ------------------------
#> 1.33      | [1.26, 1.41]
#> 
#> - One-sided CIs: upper bound fixed at [1.41~].

pearsons_c(Smoking_FASD)
#> Pearson's C |       95% CI
#> --------------------------
#> 0.80        | [0.78, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

# Use custom expected values:
fei(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#> Fei  |       95% CI
#> -------------------
#> 0.01 | [0.00, 1.00]
#> 
#> - Adjusted for uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].

cohens_w(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#> Cohen's w |       95% CI
#> ------------------------
#> 0.11      | [0.03, 9.95]
#> 
#> - One-sided CIs: upper bound fixed at [9.95~].

pearsons_c(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#> Pearson's C |       95% CI
#> --------------------------
#> 0.11        | [0.03, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
```
