# Cohen's *U*s and Other Common Language Effect Sizes (CLES)

Cohen's \\U_1\\, \\U_2\\, and \\U_3\\, probability of superiority,
proportion of overlap, Wilcoxon-Mann-Whitney odds, and Vargha and
Delaney's *A* are CLESs. These are effect sizes that represent
differences between two (independent) distributions in probabilistic
terms (See details). Pair with any reported
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) or
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html).

## Usage

``` r
p_superiority(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  paired = FALSE,
  parametric = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

cohens_u1(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  parametric = TRUE,
  ci = 0.95,
  alternative = "two.sided",
  iterations = 200,
  verbose = TRUE,
  ...
)

cohens_u2(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  parametric = TRUE,
  ci = 0.95,
  alternative = "two.sided",
  iterations = 200,
  verbose = TRUE,
  ...
)

cohens_u3(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  parametric = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  iterations = 200,
  verbose = TRUE,
  ...
)

p_overlap(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  parametric = TRUE,
  ci = 0.95,
  alternative = "two.sided",
  iterations = 200,
  verbose = TRUE,
  ...
)

vd_a(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

wmw_odds(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  paired = FALSE,
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

- mu:

  a number indicating the true value of the mean (or difference in means
  if you are performing a two sample test).

- paired:

  If `TRUE`, the values of `x` and `y` are considered as paired. This
  produces an effect size that is equivalent to the one-sample effect
  size on `x - y`.

- parametric:

  Use parametric estimation (see
  [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md))
  or non-parametric estimation (see
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)).
  See details.

- reference:

  (Optional) character value of the "group" used as the reference. By
  default, the *second* group is the reference group.

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

- iterations:

  The number of bootstrap replicates for computing confidence intervals.
  Only applies when `ci` is not `NULL` and `parametric = FALSE`.

## Value

A data frame containing the common language effect sizes (and optionally
their CIs).

## Details

These measures of effect size present group differences in probabilistic
terms:

- **Probability of superiority** is the probability that, when sampling
  an observation from each of the groups at random, that the observation
  from the second group will be larger than the sample from the first
  group. For the one-sample (or paired) case, it is the probability that
  the sample (or difference) is larger than *mu*. (Vargha and Delaney's
  *A* is an alias for the non-parametric *probability of superiority*.)

- **Cohen's \\U_1\\** is the proportion of the total of both
  distributions that does not overlap.

- **Cohen's \\U_2\\** is the proportion of one of the groups that
  exceeds *the same proportion* in the other group.

- **Cohen's \\U_3\\** is the proportion of the second group that is
  smaller than the median of the first group.

- **Overlap** (OVL) is the proportional overlap between the
  distributions. (When `parametric = FALSE`,
  [`bayestestR::overlap()`](https://easystats.github.io/bayestestR/reference/overlap.html)
  is used.)

Wilcoxon-Mann-Whitney odds are the *odds* of non-parametric superiority
(via
[`probs_to_odds()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md)),
that is the odds that, when sampling an observation from each of the
groups at random, that the observation from the second group will be
larger than the sample from the first group.

Where \\U_1\\, \\U_2\\, and *Overlap* are agnostic to the direction of
the difference between the groups, \\U_3\\ and probability of
superiority are not (this can be controlled with the `reference`
argument).

The parametric version of these effects assumes normality of both
populations and homoscedasticity. If those are not met, the non
parametric versions should be used.

## Note

If `mu` is not 0, the effect size represents the difference between the
first *shifted sample* (by `mu`) and the second sample.

## Confidence (Compatibility) Intervals (CIs)

For parametric CLES, the CIs are transformed CIs for Cohen's *d* (see
[`d_to_u3()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)).
For non-parametric (`parametric = FALSE`) CLES, the CI of
*Pr(superiority)* is a transformed CI of the rank-biserial correlation
([`rb_to_p_superiority()`](https://easystats.github.io/effectsize/reference/diff_to_cles.md)),
while for all others, confidence intervals are estimated using the
bootstrap method (using the `{boot}` package).

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

## Bootstrapped CIs

Some effect sizes are directionless–they do have a minimum value that
would be interpreted as "no effect", but they cannot cross it. For
example, a null value of [Kendall's
W](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)
is 0, indicating no difference between groups, but it can never have a
negative value. Same goes for U2 and Overlap: the null value of \\U_2\\
is 0.5, but it can never be smaller than 0.5; am *Overlap* of 1 means
"full overlap" (no difference), but it cannot be larger than 1.  
  
When bootstrapping CIs for such effect sizes, the bounds of the CIs will
never cross (and often will never cover) the null. Therefore, these CIs
should not be used for statistical inference.

## Plotting with `see`

The `see` package contains relevant plotting functions. See the
[plotting vignette in the `see`
package](https://easystats.github.io/see/articles/effectsize.html).

## References

- Cohen, J. (1977). Statistical power analysis for the behavioral
  sciences. New York: Routledge.

- Reiser, B., & Faraggi, D. (1999). Confidence intervals for the
  overlapping coefficient: the normal equal variance case. Journal of
  the Royal Statistical Society, 48(3), 413-418.

- Ruscio, J. (2008). A probability-based measure of effect size:
  robustness to base rates and other factors. Psychological methods,
  13(1), 19–30.

- Vargha, A., & Delaney, H. D. (2000). A critique and improvement of the
  CL common language effect size statistics of McGraw and Wong. Journal
  of Educational and Behavioral Statistics, 25(2), 101-132.

- O’Brien, R. G., & Castelloe, J. (2006, March). Exploiting the link
  between the Wilcoxon-Mann-Whitney test and a simple odds statistic. In
  Proceedings of the Thirty-first Annual SAS Users Group International
  Conference (pp. 209-31). Cary, NC: SAS Institute.

- Agresti, A. (1980). Generalized odds ratios for ordinal data.
  Biometrics, 59-67.

## See also

[`sd_pooled()`](https://easystats.github.io/effectsize/reference/sd_pooled.md)

Other standardized differences:
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md),
[`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

Other rank-based effect sizes:
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
[`rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)

## Examples

``` r
cohens_u2(mpg ~ am, data = mtcars)
#> Cohen's U2 |       95% CI
#> -------------------------
#> 0.77       | [0.63, 0.87]

p_superiority(mpg ~ am, data = mtcars, parametric = FALSE)
#> Pr(superiority) |       95% CI
#> ------------------------------
#> 0.17            | [0.08, 0.32]
#> 
#> - Non-parametric CLES

wmw_odds(mpg ~ am, data = mtcars)
#> WMW Odds |       95% CI
#> -----------------------
#> 0.20     | [0.09, 0.47]
#> 
#> - Non-parametric CLES

x <- c(1.83, 0.5, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.3)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

p_overlap(x, y)
#> Overlap |       95% CI
#> ----------------------
#> 0.78    | [0.45, 1.00]
p_overlap(y, x) # direction of effect does not matter
#> Overlap |       95% CI
#> ----------------------
#> 0.78    | [0.45, 1.00]

cohens_u3(x, y)
#> Cohen's U3 |       95% CI
#> -------------------------
#> 0.72       | [0.35, 0.93]
cohens_u3(y, x) # direction of effect does matter
#> Cohen's U3 |       95% CI
#> -------------------------
#> 0.28       | [0.07, 0.65]
```
