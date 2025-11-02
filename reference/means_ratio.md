# Ratio of Means

Computes the ratio of two means (also known as the "response ratio"; RR)
of **variables on a ratio scale** (with an absolute 0). Pair with any
reported [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html).

## Usage

``` r
means_ratio(
  x,
  y = NULL,
  data = NULL,
  paired = FALSE,
  adjust = TRUE,
  log = FALSE,
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

- paired:

  If `TRUE`, the values of `x` and `y` are considered as paired. The
  correlation between these variables will affect the CIs.

- adjust:

  Should the effect size be adjusted for small-sample bias? Defaults to
  `TRUE`; Advisable for small samples.

- log:

  Should the log-ratio be returned? Defaults to `FALSE`. Normally
  distributed and useful for meta-analysis.

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

A data frame with the effect size (`Means_ratio` or
`Means_ratio_adjusted`) and their CIs (`CI_low` and `CI_high`).

## Details

The Means Ratio ranges from 0 to \\\infty\\, with values smaller than 1
indicating that the mean of the reference group is larger, values larger
than 1 indicating that the mean of the reference group is smaller, and
values of 1 indicating that the means are equal.

## Note

The small-sample bias corrected response ratio reported from this
function is derived from Lajeunesse (2015).

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals are estimated as described by Lajeunesse (2011 &
2015) using the log-ratio standard error assuming a normal distribution.
By this method, the log is taken of the ratio of means, which makes this
outcome measure symmetric around 0 and yields a corresponding sampling
distribution that is closer to normality.

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

Lajeunesse, M. J. (2011). On the meta-analysis of response ratios for
studies with correlated and multi-group designs. Ecology, 92(11),
2049-2055. [doi:10.1890/11-0423.1](https://doi.org/10.1890/11-0423.1)

Lajeunesse, M. J. (2015). Bias and correction for the log response ratio
in ecological meta-analysis. Ecology, 96(8), 2056-2063.
[doi:10.1890/14-2402.1](https://doi.org/10.1890/14-2402.1)

Hedges, L. V., Gurevitch, J., & Curtis, P. S. (1999). The meta-analysis
of response ratios in experimental ecology. Ecology, 80(4), 1150–1156.
[doi:10.1890/0012-9658(1999)080\[1150:TMAORR\]2.0.CO;2](https://doi.org/10.1890/0012-9658%281999%29080%5B1150%3ATMAORR%5D2.0.CO%3B2)

## See also

Other standardized differences:
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md),
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md),
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)

## Examples

``` r
x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
means_ratio(x, y)
#> Means Ratio (adj.) |       95% CI
#> ---------------------------------
#> 1.31               | [0.82, 2.10]
means_ratio(x, y, adjust = FALSE)
#> Means Ratio |       95% CI
#> --------------------------
#> 1.32        | [0.82, 2.13]

means_ratio(x, y, log = TRUE)
#> log(Means Ratio, adj.) |        95% CI
#> --------------------------------------
#> 0.27                   | [-0.20, 0.74]


# The ratio is scale invariant, making it a standardized effect size
means_ratio(3 * x, 3 * y)
#> Means Ratio (adj.) |       95% CI
#> ---------------------------------
#> 1.31               | [0.82, 2.10]
```
