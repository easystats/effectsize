# Semi-Partial (Part) Correlation Squared (\\\Delta R^2\\)

Compute the semi-partial (part) correlation squared (also known as
\\\Delta R^2\\). Currently, only
[`lm()`](https://rdrr.io/r/stats/lm.html) models are supported.

## Usage

``` r
r2_semipartial(
  model,
  type = c("terms", "parameters"),
  ci = 0.95,
  alternative = "greater",
  ...
)
```

## Arguments

- model:

  An `lm` model.

- type:

  Type, either `"terms"`, or `"parameters"`.

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

## Value

A data frame with the effect size.

## Details

This is similar to the last column of the "Conditional Dominance
Statistics" section of the
[`parameters::dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
output. For each term, the model is refit *without* the columns on the
[model matrix](https://rdrr.io/r/stats/model.matrix.html) that
correspond to that term. The \\R^2\\ of this *sub*-model is then
subtracted from the \\R^2\\ of the *full* model to yield the \\\Delta
R^2\\. (For `type = "parameters"`, this is done for each column in the
model matrix.)

**Note** that this is unlike
[`parameters::dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html),
where term deletion is done via the formula interface, and therefore may
lead to different results.

For other, non-[`lm()`](https://rdrr.io/r/stats/lm.html) models, as well
as more verbose information and options, please see the documentation
for
[`parameters::dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html).

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals are based on the normal approximation as provided
by Alf and Graf (1999). An adjustment to the lower bound of the CI is
used, to improve the coverage properties of the CIs, according to Algina
et al (2008): If the *F* test associated with the \\sr^2\\ is
significant (at `1-ci` level), but the lower bound of the CI is 0, it is
set to a small value (arbitrarily to a 10th of the estimated \\sr^2\\);
if the *F* test is not significant, the lower bound is set to 0.
(Additionally, lower and upper bound are "fixed" so that they cannot be
smaller than 0 or larger than 1.)

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

- Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for
  the difference between two squared multiple correlations: A simplified
  approach. *Psychological Methods, 4*(1), 70-75.
  [doi:10.1037/1082-989X.4.1.70](https://doi.org/10.1037/1082-989X.4.1.70)

- Algina, J., Keselman, H. J., & Penfield, R. D. (2008). Confidence
  intervals for the squared multiple semipartial correlation
  coefficient. *Journal of Modern Applied Statistical Methods, 7*(1),
  2-10.
  [doi:10.22237/jmasm/1209614460](https://doi.org/10.22237/jmasm/1209614460)

## See also

[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md),
[`cohens_f()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
for comparing two models,
[`parameters::dominance_analysis()`](https://easystats.github.io/parameters/reference/dominance_analysis.html)
and
[`parameters::standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html).

## Examples

``` r
data("hardlyworking")

m <- lm(salary ~ factor(n_comps) + xtra_hours * seniority, data = hardlyworking)

r2_semipartial(m)
#> Term                 |      sr2 |       95% CI
#> ----------------------------------------------
#> factor(n_comps)      |     0.15 | [0.12, 1.00]
#> xtra_hours           |     0.06 | [0.05, 1.00]
#> seniority            | 2.07e-03 | [0.00, 1.00]
#> xtra_hours:seniority | 4.85e-04 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

r2_semipartial(m, type = "parameters")
#> Parameter            |      sr2 |       95% CI
#> ----------------------------------------------
#> factor(n_comps)1     |     0.04 | [0.03, 1.00]
#> factor(n_comps)2     |     0.12 | [0.10, 1.00]
#> factor(n_comps)3     |     0.07 | [0.05, 1.00]
#> xtra_hours           |     0.06 | [0.05, 1.00]
#> seniority            | 2.07e-03 | [0.00, 1.00]
#> xtra_hours:seniority | 4.85e-04 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].



# Compare to `eta_squared()`
# --------------------------
npk.aov <- lm(yield ~ N + P + K, npk)

# When predictors are orthogonal,
# eta_squared(partial = FALSE) gives the same effect size:
performance::check_collinearity(npk.aov)
#> # Check for Multicollinearity
#> 
#> Low Correlation
#> 
#>  Term VIF      VIF  CI adj. VIF Tolerance
#>     N   1 [1.00, 1.00]        1         1
#>     P   1 [1.00, 1.00]        1         1
#>     K   1 [1.00, 1.00]        1         1

eta_squared(npk.aov, partial = FALSE)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter |     Eta2 |       95% CI
#> -----------------------------------
#> N         |     0.22 | [0.01, 1.00]
#> P         | 9.59e-03 | [0.00, 1.00]
#> K         |     0.11 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

r2_semipartial(npk.aov)
#> Term |      sr2 |       95% CI
#> ------------------------------
#> N    |     0.22 | [0.00, 1.00]
#> P    | 9.59e-03 | [0.00, 1.00]
#> K    |     0.11 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

if (FALSE) { # interactive()
# Compare to `dominance_analysis()`
# ---------------------------------
m_full <- lm(salary ~ ., data = hardlyworking)

r2_semipartial(m_full)

# Compare to last column of "Conditional Dominance Statistics":
parameters::dominance_analysis(m_full)
}
```
