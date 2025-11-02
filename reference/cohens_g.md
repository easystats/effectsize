# Effect Size for Paired Contingency Tables

Cohen's *g* is an effect size of asymmetry (or marginal heterogeneity)
for dependent (paired) contingency tables ranging between 0 (perfect
symmetry) and 0.5 (perfect asymmetry) (see
[`stats::mcnemar.test()`](https://rdrr.io/r/stats/mcnemar.test.html)).
(Note this is not *not* a measure of (dis)agreement between the pairs,
but of (a)symmetry.)

## Usage

``` r
cohens_g(x, y = NULL, ci = 0.95, alternative = "two.sided", ...)
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

- ...:

  Ignored

## Value

A data frame with the effect size (`Cohens_g`, `Risk_ratio` (possibly
with the prefix `log_`), `Cohens_h`) and its CIs (`CI_low` and
`CI_high`).

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals are based on the proportion (\\P = g + 0.5\\)
confidence intervals returned by
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) (minus
0.5), which give a good close approximation.

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

## See also

Other effect sizes for contingency table:
[`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`phi()`](https://easystats.github.io/effectsize/reference/phi.md)

## Examples

``` r

data("screening_test")

phi(screening_test$Diagnosis, screening_test$Test1)
#> Phi (adj.) |       95% CI
#> -------------------------
#> 0.85       | [0.81, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

phi(screening_test$Diagnosis, screening_test$Test2)
#> Phi (adj.) |       95% CI
#> -------------------------
#> 0.85       | [0.81, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

# Both tests seem comparable - but are the tests actually different?

(tests <- table(Test1 = screening_test$Test1, Test2 = screening_test$Test2))
#>        Test2
#> Test1   "Neg" "Pos"
#>   "Neg"   794    86
#>   "Pos"   150   570

mcnemar.test(tests)
#> 
#>  McNemar's Chi-squared test with continuity correction
#> 
#> data:  tests
#> McNemar's chi-squared = 16.818, df = 1, p-value = 4.115e-05
#> 

cohens_g(tests)
#> Cohen's g |       95% CI
#> ------------------------
#> 0.14      | [0.07, 0.19]

# Test 2 gives a negative result more than test 1!
```
