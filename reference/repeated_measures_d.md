# Standardized Mean Differences for Repeated Measures

Compute effect size indices for standardized mean differences in
repeated measures data. Pair with any reported
`stats::t.test(paired = TRUE)`.  
  
In a repeated-measures design, the same subjects are measured in
multiple conditions or time points. Unlike the case of independent
groups, there are multiple sources of variation that can be used to
standardized the differences between the means of the conditions /
times.

## Usage

``` r
repeated_measures_d(
  x,
  y,
  data = NULL,
  mu = 0,
  method = c("rm", "av", "z", "b", "d", "r"),
  adjust = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)

rm_d(
  x,
  y,
  data = NULL,
  mu = 0,
  method = c("rm", "av", "z", "b", "d", "r"),
  adjust = TRUE,
  reference = NULL,
  ci = 0.95,
  alternative = "two.sided",
  verbose = TRUE,
  ...
)
```

## Arguments

- x, y:

  Paired numeric vectors, or names of ones in `data`. `x` can also be a
  formula:

  - `Pair(x,y) ~ 1` for wide data.

  - `y ~ condition | id` for long data, possibly with repetitions.

- data:

  An optional data frame containing the variables.

- mu:

  a number indicating the true value of the mean (or difference in means
  if you are performing a two sample test).

- method:

  Method of repeated measures standardized differences. See details.

- adjust:

  Apply Hedges' small-sample bias correction? See
  [`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md).

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

A data frame with the effect size and their CIs (`CI_low` and
`CI_high`).

## Note

`rm_d()` is an alias for `repeated_measures_d()`.

## Standardized Mean Differences for Repeated Measures

Unlike [Cohen's
d](https://easystats.github.io/effectsize/reference/cohens_d.md) for
independent groups, where standardization naturally is done by the
(pooled) population standard deviation (cf. Glass’s \\\Delta\\), when
measured across two conditions are dependent, there are many more
options for what error term to standardize by. Additionally, some
options allow for data to be replicated (many measurements per condition
per individual), others require a single observation per condition per
individual (aka, paired data; so replications are aggregated).

(It should be noted that all of these have awful and confusing
notations.)

Standardize by...

- **Difference Score Variance: \\d\_{z}\\** (*Requires paired data*) -
  This is akin to computing difference scores for each individual and
  then computing a one-sample Cohen's *d* (Cohen, 1988, pp. 48; see
  examples).

- **Within-Subject Variance: \\d\_{rm}\\** (*Requires paired data*) -
  Cohen suggested adjusting \\d\_{z}\\ to estimate the "standard"
  between-subjects *d* by a factor of \\\sqrt{2(1-r)}\\, where *r* is
  the Pearson correlation between the paired measures (Cohen, 1988, pp.
  48).

- **Control Variance: \\d\_{b}\\ (aka Becker's *d*)** (*Requires paired
  data*) - Standardized by the variance of the control condition (or in
  a pre- post-treatment setting, the pre-treatment condition). This is
  akin to Glass' *delta*
  ([`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md))
  (Becker, 1988). Note that this is taken here as the *second* condition
  (`y`).

- **Average Variance: \\d\_{av}\\** (*Requires paired data*) - Instead
  of standardizing by the variance in the of the control (or pre)
  condition, Cumming suggests standardizing by the average variance of
  the two paired conditions (Cumming, 2013, pp. 291).

- **All Variance: Just \\d\\** - This is the same as computing a
  standard independent-groups Cohen's *d* (Cohen, 1988). Note that CIs
  *do* account for the dependence, and so are typically more narrow (see
  examples).

- **Residual Variance: \\d\_{r}\\** (*Requires data with
  replications*) - Divide by the pooled variance after all individual
  differences have been partialled out (i.e., the residual/level-1
  variance in an ANOVA or MLM setting). In between-subjects designs
  where each subject contributes a single response, this is equivalent
  to classical Cohen’s d. Priors in the `BayesFactor` package are
  defined on this scale (Rouder et al., 2012).  
    
  Note that for paired data, when the two conditions have equal
  variance, \\d\_{rm}\\, \\d\_{av}\\, \\d\_{b}\\ are equal to \\d\\.

## Confidence (Compatibility) Intervals (CIs)

Confidence intervals are estimated using the standard normal parametric
method (see Algina & Keselman, 2003; Becker, 1988; Cooper et al., 2009;
Hedges & Olkin, 1985; Pustejovsky et al., 2014).

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

- Algina, J., & Keselman, H. J. (2003). Approximate confidence intervals
  for effect sizes. Educational and Psychological Measurement, 63(4),
  537-553.

- Becker, B. J. (1988). Synthesizing standardized mean‐change measures.
  British Journal of Mathematical and Statistical Psychology, 41(2),
  257-278.

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Cooper, H., Hedges, L., & Valentine, J. (2009). Handbook of research
  synthesis and meta-analysis. Russell Sage Foundation, New York.

- Cumming, G. (2013). Understanding the new statistics: Effect sizes,
  confidence intervals, and meta-analysis. Routledge.

- Hedges, L. V. & Olkin, I. (1985). Statistical methods for
  meta-analysis. Orlando, FL: Academic Press.

- Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
  Design-comparable effect sizes in multiple baseline designs: A general
  modeling framework. Journal of Educational and Behavioral Statistics,
  39(5), 368-393.

- Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M.
  (2012). Default Bayes factors for ANOVA designs. Journal of
  mathematical psychology, 56(5), 356-374.

## See also

[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
and `lmeInfo::g_mlm()` and `emmeans::effsize()` for more flexible
methods.

Other standardized differences:
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md),
[`means_ratio()`](https://easystats.github.io/effectsize/reference/means_ratio.md),
[`p_superiority()`](https://easystats.github.io/effectsize/reference/p_superiority.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)

## Examples

``` r
# Paired data -------

data("preferences2025")
# Is chocolate preferred over... poop?

repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025)
#> d (rm) |       95% CI
#> ---------------------
#> 5.58   | [5.01, 6.15]
#> 
#> - Adjusted for small sample bias.

# More options:
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    mu = 3.4)
#> d (rm) |       95% CI
#> ---------------------
#> 1.53   | [1.32, 1.73]
#> 
#> - Adjusted for small sample bias.
#> - Deviation from a difference of 3.4.
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    alternative = "greater")
#> d (rm) |      95% CI
#> --------------------
#> 5.58   | [5.11, Inf]
#> 
#> - Adjusted for small sample bias.
#> - One-sided CIs: upper bound fixed at [Inf].

# Other methods
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    method = "av")
#> d (av) |       95% CI
#> ---------------------
#> 5.57   | [5.43, 5.71]
#> 
#> - Adjusted for small sample bias.
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    method = "b")
#> Becker's d |       95% CI
#> -------------------------
#> 5.17       | [4.82, 5.53]
#> 
#> - Adjusted for small sample bias.
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    method = "d")
#> Cohen's d |       95% CI
#> ------------------------
#> 5.58      | [5.29, 5.86]
#> 
#> - Adjusted for small sample bias.
repeated_measures_d(Pair(chocolate, poop) ~ 1, data = preferences2025,
                    method = "z")
#> d (z) |       95% CI
#> --------------------
#> 3.56  | [3.32, 3.80]
#> 
#> - Adjusted for small sample bias.

# d_z is the same as Cohen's d for one sample (of individual difference):
cohens_d(chocolate - poop ~ 1, data = preferences2025)
#> Cohen's d |       95% CI
#> ------------------------
#> 3.56      | [3.32, 3.80]




# Repetition data -----------

data("rouder2016")

# For rm, ad, z, b, data is aggregated
repeated_measures_d(rt ~ cond | id, data = rouder2016)
#> The rm standardized difference requires paired data,
#>   but data contains more than one observation per design cell.
#>   Aggregating data using `mean()`.
#> d (rm) |         95% CI
#> -----------------------
#> -0.80  | [-1.06, -0.53]
#> 
#> - Adjusted for small sample bias.

# same as:
rouder2016_wide <- tapply(rouder2016[["rt"]], rouder2016[1:2], mean)
repeated_measures_d(rouder2016_wide[, 1], rouder2016_wide[, 2])
#> d (rm) |         95% CI
#> -----------------------
#> -0.80  | [-1.06, -0.53]
#> 
#> - Adjusted for small sample bias.

# For r or d, data is not aggragated:
repeated_measures_d(rt ~ cond | id, data = rouder2016, method = "r")
#> d (r) |         95% CI
#> ----------------------
#> -0.26 | [-0.33, -0.18]
#> 
#> - Adjusted for small sample bias.
repeated_measures_d(rt ~ cond | id, data = rouder2016, method = "d", adjust = FALSE)
#> Cohen's d |         95% CI
#> --------------------------
#> -0.25     | [-0.32, -0.18]

# d is the same as Cohen's d for two independent groups:
cohens_d(rt ~ cond, data = rouder2016, ci = NULL)
#> Cohen's d
#> ---------
#> -0.25    
#> 
#> - Estimated using pooled SD.
```
