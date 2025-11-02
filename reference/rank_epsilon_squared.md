# Effect Size for Rank Based ANOVA

Compute rank epsilon squared (\\E^2_R\\) or rank eta squared
(\\\eta^2_H\\) (to accompany
[`stats::kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html)),
and Kendall's *W* (to accompany
[`stats::friedman.test()`](https://rdrr.io/r/stats/friedman.test.html))
effect sizes for non-parametric (rank sum) one-way ANOVAs.

## Usage

``` r
rank_epsilon_squared(
  x,
  groups,
  data = NULL,
  ci = 0.95,
  alternative = "greater",
  iterations = 200,
  verbose = TRUE,
  ...
)

rank_eta_squared(
  x,
  groups,
  data = NULL,
  ci = 0.95,
  alternative = "greater",
  iterations = 200,
  verbose = TRUE,
  ...
)

kendalls_w(
  x,
  groups,
  blocks,
  data = NULL,
  blocks_on_rows = TRUE,
  ci = 0.95,
  alternative = "greater",
  iterations = 200,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  Can be one of:

  - A numeric or ordered vector, or a character name of one in `data`.

  - A list of vectors (for `rank_eta/epsilon_squared()`).

  - A matrix of `blocks x groups` (for `kendalls_w()`) (or
    `groups x blocks` if `blocks_on_rows = FALSE`). See details for the
    `blocks` and `groups` terminology used here.

  - A formula in the form of:

    - `DV ~ groups` for `rank_eta/epsilon_squared()`.

    - `DV ~ groups | blocks` for `kendalls_w()` (See details for the
      `blocks` and `groups` terminology used here).

- groups, blocks:

  A factor vector giving the group / block for the corresponding
  elements of `x`, or a character name of one in `data`. Ignored if `x`
  is not a vector.

- data:

  An optional data frame containing the variables.

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"two.sided"` (default, two-sided CI),
  `"greater"` or `"less"` (one-sided CI). Partial matching is allowed
  (e.g., `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- iterations:

  The number of bootstrap replicates for computing confidence intervals.
  Only applies when `ci` is not `NULL`.

- verbose:

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to or from other methods. When `x` is a formula,
  these can be `subset` and `na.action`.

- blocks_on_rows:

  Are blocks on rows (`TRUE`) or columns (`FALSE`).

## Value

A data frame with the effect size and its CI.

## Details

The rank epsilon squared and rank eta squared are appropriate for
non-parametric tests of differences between 2 or more samples (a rank
based ANOVA). See
[stats::kruskal.test](https://rdrr.io/r/stats/kruskal.test.html). Values
range from 0 to 1, with larger values indicating larger differences
between groups.  
  
Kendall's *W* is appropriate for non-parametric tests of differences
between 2 or more dependent samples (a rank based rmANOVA), where each
`group` (e.g., experimental condition) was measured for each `block`
(e.g., subject). This measure is also common as a measure of reliability
of the rankings of the `groups` between raters (`blocks`). See
[stats::friedman.test](https://rdrr.io/r/stats/friedman.test.html).
Values range from 0 to 1, with larger values indicating larger
differences between groups / higher agreement between raters.

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals for \\E^2_R\\, \\\eta^2_H\\, and Kendall's *W* are
estimated using the bootstrap method (using the `{boot}` package).

## Ties

When tied values occur, they are each given the average of the ranks
that would have been given had no ties occurred. This results in an
effect size of reduced magnitude. A correction has been applied for
Kendall's *W*.

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
example, a null value of Kendall's W is 0, indicating no difference
between groups, but it can never have a negative value. Same goes for
[U2](https://easystats.github.io/effectsize/reference/p_superiority.md)
and
[Overlap](https://easystats.github.io/effectsize/reference/p_superiority.md):
the null value of \\U_2\\ is 0.5, but it can never be smaller than 0.5;
am *Overlap* of 1 means "full overlap" (no difference), but it cannot be
larger than 1.  
  
When bootstrapping CIs for such effect sizes, the bounds of the CIs will
never cross (and often will never cover) the null. Therefore, these CIs
should not be used for statistical inference.

## Plotting with `see`

The `see` package contains relevant plotting functions. See the
[plotting vignette in the `see`
package](https://easystats.github.io/see/articles/effectsize.html).

## References

- Kendall, M.G. (1948) Rank correlation methods. London: Griffin.

- Tomczak, M., & Tomczak, E. (2014). The need to report effect size
  estimates revisited. An overview of some recommended measures of
  effect size. Trends in sport sciences, 1(21), 19-25.

## See also

Other rank-based effect sizes:
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)

Other effect sizes for ANOVAs:
[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)

## Examples

``` r
# \donttest{
# Rank Eta/Epsilon Squared
# ========================

rank_eta_squared(mpg ~ cyl, data = mtcars)
#> Eta2 (rank) |       95% CI
#> --------------------------
#> 0.82        | [0.77, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

rank_epsilon_squared(mpg ~ cyl, data = mtcars)
#> Epsilon2 (rank) |       95% CI
#> ------------------------------
#> 0.83            | [0.78, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].



# Kendall's W
# ===========
dat <- data.frame(
  cond = c("A", "B", "A", "B", "A", "B"),
  ID = c("L", "L", "M", "M", "H", "H"),
  y = c(44.56, 28.22, 24, 28.78, 24.56, 18.78)
)
(W <- kendalls_w(y ~ cond | ID, data = dat, verbose = FALSE))
#> Kendall's W |       95% CI
#> --------------------------
#> 0.11        | [0.11, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

interpret_kendalls_w(0.11)
#> [1] "slight agreement"
#> (Rules: landis1977)
#> 
interpret(W, rules = "landis1977")
#> Kendall's W |       95% CI |   Interpretation
#> ---------------------------------------------
#> 0.11        | [0.11, 1.00] | slight agreement
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
#> - Interpretation rule: landis1977
# }
```
