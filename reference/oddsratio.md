# Odds Ratios, Risk Ratios and Other Effect Sizes for 2-by-2 Contingency Tables

Compute Odds Ratios, Risk Ratios, Cohen's *h*, Absolute Risk Reduction
or Number Needed to Treat. Report with any
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) or
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).  
  
Note that these are computed with each **column** representing the
different groups (the *first* column representing the treatment group
and the *second* column the baseline or control group), and the *first*
row representing the "positive" level (the `k` in `p=k/n`). Effects are
given as *p*-treatment *over* *p*-control. If you wish you use rows as
groups you must pass a transposed table, or switch the `x` and `y`
arguments.

## Usage

``` r
oddsratio(x, y = NULL, ci = 0.95, alternative = "two.sided", log = FALSE, ...)

riskratio(x, y = NULL, ci = 0.95, alternative = "two.sided", ...)

cohens_h(x, y = NULL, ci = 0.95, alternative = "two.sided", ...)

arr(x, y = NULL, ci = 0.95, alternative = "two.sided", ...)

nnt(x, y = NULL, ci = 0.95, alternative = "two.sided", ...)
```

## Arguments

- x:

  a numeric vector or matrix. `x` and `y` can also both be factors.

- y:

  a numeric vector; ignored if `x` is a matrix. If `x` is a factor, `y`
  should be a factor of the same length.

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- log:

  Take in or output the log of the ratio (such as in logistic models),
  e.g. when the desired input or output are log odds ratios instead odds
  ratios.

- ...:

  Ignored

## Value

A data frame with the effect size (`Odds_ratio`, `log_Odds_ratio`,
`Risk_ratio` `Cohens_h`, `ARR`, `NNT`) and its CIs (`CI_low` and
`CI_high`).

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals are estimated using the standard normal parametric
method (see Katz et al., 1978; Szumilas, 2010).

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

- Katz, D. J. S. M., Baptista, J., Azen, S. P., & Pike, M. C. (1978).
  Obtaining confidence intervals for the risk ratio in cohort studies.
  Biometrics, 469-474.

- Szumilas, M. (2010). Explaining odds ratios. Journal of the Canadian
  academy of child and adolescent psychiatry, 19(3), 227.

## See also

Other effect sizes for contingency table:
[`cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.md),
[`phi()`](https://easystats.github.io/effectsize/reference/phi.md)

## Examples

``` r
data("RCT_table")
RCT_table # note groups are COLUMNS
#>            Group
#> Diagnosis   Treatment Control
#>   Sick             71      30
#>   Recovered        50     100

oddsratio(RCT_table)
#> Odds ratio |       95% CI
#> -------------------------
#> 4.73       | [2.74, 8.17]
oddsratio(RCT_table, alternative = "greater")
#> Odds ratio |      95% CI
#> ------------------------
#> 4.73       | [3.00, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

riskratio(RCT_table)
#> Risk ratio |       95% CI
#> -------------------------
#> 2.54       | [1.80, 3.60]

cohens_h(RCT_table)
#> Cohen's h |       95% CI
#> ------------------------
#> 0.74      | [0.50, 0.99]

arr(RCT_table)
#> ARR  |       95% CI
#> -------------------
#> 0.36 | [0.24, 0.47]

nnt(RCT_table)
#> NNT  |       95% CI
#> -------------------
#> 2.81 | [2.13, 4.13]
```
