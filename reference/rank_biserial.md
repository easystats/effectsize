# Dominance Effect Sizes for Rank Based Differences

Compute the rank-biserial correlation (\\r\_{rb}\\) and Cliff's *delta*
(\\\delta\\) effect sizes for non-parametric (rank sum) differences.
These effect sizes of dominance are closely related to the [Common
Language Effect
Sizes](https://easystats.github.io/effectsize/reference/p_superiority.md).
Pair with any reported
[`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html).

## Usage

``` r
rank_biserial(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  paired = FALSE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

cliffs_delta(
  x,
  y = NULL,
  data = NULL,
  mu = 0,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)
```

## Arguments

- x, y:

  A numeric or ordered vector, or a character name of one in `data`. Any
  missing values (`NA`s) are dropped from the resulting vector. `x` can
  also be a formula (see
  [`stats::wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)),
  in which case `y` is ignored.

- data:

  An optional data frame containing the variables.

- mu:

  a number indicating the value around which (a-)symmetry (for
  one-sample or paired samples) or shift (for independent samples) is to
  be estimated. See
  [stats::wilcox.test](https://rdrr.io/r/stats/wilcox.test.html).

- paired:

  If `TRUE`, the values of `x` and `y` are considered as paired. This
  produces an effect size that is equivalent to the one-sample effect
  size on `x - y`.

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

## Value

A data frame with the effect size `r_rank_biserial` and its CI (`CI_low`
and `CI_high`).

## Details

The rank-biserial correlation is appropriate for non-parametric tests of
differences - both for the one sample or paired samples case, that would
normally be tested with Wilcoxon's Signed Rank Test (giving the
**matched-pairs** rank-biserial correlation) and for two independent
samples case, that would normally be tested with Mann-Whitney's *U* Test
(giving **Glass'** rank-biserial correlation). See
[stats::wilcox.test](https://rdrr.io/r/stats/wilcox.test.html). In both
cases, the correlation represents the difference between the proportion
of favorable and unfavorable pairs / signed ranks (Kerby, 2014). Values
range from `-1` complete dominance of the second sample (*all* values of
the second sample are larger than *all* the values of the first sample)
to `+1` complete dominance of the fist sample (*all* values of the
second sample are smaller than *all* the values of the first sample).  
  
Cliff's *delta* is an alias to the rank-biserial correlation in the two
sample case.

## Ties

When tied values occur, they are each given the average of the ranks
that would have been given had no ties occurred. This results in an
effect size of reduced magnitude. A correction has been applied for
Kendall's *W*.

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals for the rank-biserial correlation (and Cliff's
*delta*) are estimated using the normal approximation (via Fisher's
transformation).

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

- Cureton, E. E. (1956). Rank-biserial correlation. Psychometrika,
  21(3), 287-290.

- Glass, G. V. (1965). A ranking variable analogue of biserial
  correlation: Implications for short-cut item analysis. Journal of
  Educational Measurement, 2(1), 91-95.

- Kerby, D. S. (2014). The simple difference formula: An approach to
  teaching nonparametric correlation. Comprehensive Psychology, 3,
  11-IT.

- King, B. M., & Minium, E. W. (2008). Statistical reasoning in the
  behavioral sciences. John Wiley & Sons Inc.

- Cliff, N. (1993). Dominance statistics: Ordinal analyses to answer
  ordinal questions. Psychological bulletin, 114(3), 494.

- Tomczak, M., & Tomczak, E. (2014). The need to report effect size
  estimates revisited. An overview of some recommended measures of
  effect size.

## See also

Other standardized differences:
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md),
[`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md),
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

Other rank-based effect sizes:
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)

## Examples

``` r
# \donttest{
data(mtcars)
mtcars$am <- factor(mtcars$am)
mtcars$cyl <- factor(mtcars$cyl)

# Two Independent Samples ----------
(rb <- rank_biserial(mpg ~ am, data = mtcars))
#> r (rank biserial) |         95% CI
#> ----------------------------------
#> -0.66             | [-0.84, -0.36]
# Same as:
# rank_biserial("mpg", "am", data = mtcars)
# rank_biserial(mtcars$mpg[mtcars$am=="0"], mtcars$mpg[mtcars$am=="1"])
# cliffs_delta(mpg ~ am, data = mtcars)

# More options:
rank_biserial(mpg ~ am, data = mtcars, mu = -5)
#> r (rank biserial) |        95% CI
#> ---------------------------------
#> -0.21             | [-0.56, 0.20]
#> 
#> - Deviation from a difference of -5.
print(rb, append_CLES = TRUE)
#> r (rank biserial) |         95% CI
#> ----------------------------------
#> -0.66             | [-0.84, -0.36]
#> 
#> 
#> ## Common Language Effect Sizes:
#> Pr(superiority) |       95% CI
#> ------------------------------
#> 0.17            | [0.08, 0.32]
#> 
#> - Non-parametric CLES


# One Sample ----------
# from help("wilcox.test")
x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
depression <- data.frame(first = x, second = y, change = y - x)

rank_biserial(change ~ 1, data = depression)
#> r (rank biserial) |         95% CI
#> ----------------------------------
#> -0.78             | [-0.94, -0.30]

# same as:
# rank_biserial("change", data = depression)
# rank_biserial(mtcars$wt)

# More options:
rank_biserial(change ~ 1, data = depression, mu = -0.5)
#> r (rank biserial) |        95% CI
#> ---------------------------------
#> 0.16              | [-0.52, 0.71]
#> 
#> - Deviation from a difference of -0.5.


# Paired Samples ----------
(rb <- rank_biserial(Pair(first, second) ~ 1, data = depression))
#> r (rank biserial) |       95% CI
#> --------------------------------
#> 0.78              | [0.30, 0.94]

# same as:
# rank_biserial(depression$first, depression$second, paired = TRUE)

interpret_rank_biserial(0.78)
#> [1] "very large"
#> (Rules: funder2019)
#> 
interpret(rb, rules = "funder2019")
#> r (rank biserial) |       95% CI | Interpretation
#> -------------------------------------------------
#> 0.78              | [0.30, 0.94] |     very large
#> 
#> - Interpretation rule: funder2019
# }
```
