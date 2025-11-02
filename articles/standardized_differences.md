# Standardized Differences

This vignette provides a review of effect sizes for comparisons of
groups, which are typically achieved with the
[`t.test()`](https://rdrr.io/r/stats/t.test.html) and
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) functions.

``` r

library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)
```

## Standardized Differences

For *t*-tests, it is common to report an effect size representing a
standardized difference between the two compared samples’ means. These
measures range from -\infty to +\infty, with negative values indicating
the second group’s mean is larger (and vice versa).

### Two Independent Samples

For two independent samples, the difference between the means is
standardized based on the pooled standard deviation of both samples
(assumed to be equal in the population):

``` r

t.test(mpg ~ am, data = mtcars, var.equal = TRUE)
```

    > 
    >   Two Sample t-test
    > 
    > data:  mpg by am
    > t = -4, df = 30, p-value = 3e-04
    > alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    > 95 percent confidence interval:
    >  -10.85  -3.64
    > sample estimates:
    > mean in group 0 mean in group 1 
    >            17.1            24.4

``` r

cohens_d(mpg ~ am, data = mtcars)
```

    > Cohen's d |         95% CI
    > --------------------------
    > -1.48     | [-2.27, -0.67]
    > 
    > - Estimated using pooled SD.

Hedges’ *g* provides a small-sample bias correction (for small sample
sizes, N \< 20).

``` r

hedges_g(mpg ~ am, data = mtcars)
```

    > Hedges' g |         95% CI
    > --------------------------
    > -1.44     | [-2.21, -0.65]
    > 
    > - Estimated using pooled SD.

If variances cannot be assumed to be equal, it is possible to get
estimates that are not based on the pooled standard deviation:

``` r

t.test(mpg ~ am, data = mtcars, var.equal = FALSE)
```

    > 
    >   Welch Two Sample t-test
    > 
    > data:  mpg by am
    > t = -4, df = 18, p-value = 0.001
    > alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    > 95 percent confidence interval:
    >  -11.28  -3.21
    > sample estimates:
    > mean in group 0 mean in group 1 
    >            17.1            24.4

``` r

cohens_d(mpg ~ am, data = mtcars, pooled_sd = FALSE)
```

    > Cohen's d |         95% CI
    > --------------------------
    > -1.41     | [-2.26, -0.53]
    > 
    > - Estimated using un-pooled SD.

``` r

hedges_g(mpg ~ am, data = mtcars, pooled_sd = FALSE)
```

    > Hedges' g |         95% CI
    > --------------------------
    > -1.35     | [-2.17, -0.51]
    > 
    > - Estimated using un-pooled SD.

In cases where the differences between the variances are substantial, it
is also common to standardize the difference based only on the standard
deviation of one of the groups (usually the “control” group); this
effect size is known as Glass’ \Delta (delta) (Note that the standard
deviation is taken from the *second* sample).

``` r

glass_delta(mpg ~ am, data = mtcars)
```

    > Glass' Δ (adj.) |         95% CI
    > --------------------------------
    > -1.10           | [-1.80, -0.37]

For a one-sided hypothesis, it is also possible to construct one-sided
confidence intervals:

``` r

t.test(mpg ~ am, data = mtcars, var.equal = TRUE, alternative = "less")
```

    > 
    >   Two Sample t-test
    > 
    > data:  mpg by am
    > t = -4, df = 30, p-value = 1e-04
    > alternative hypothesis: true difference in means between group 0 and group 1 is less than 0
    > 95 percent confidence interval:
    >   -Inf -4.25
    > sample estimates:
    > mean in group 0 mean in group 1 
    >            17.1            24.4

``` r

cohens_d(mpg ~ am, data = mtcars, pooled_sd = TRUE, alternative = "less")
```

    > Cohen's d |        95% CI
    > -------------------------
    > -1.48     | [-Inf, -0.80]
    > 
    > - Estimated using pooled SD.
    > - One-sided CIs: lower bound fixed at [-Inf].

### One Sample

In the case of a one-sample test, the effect size represents the
standardized distance of the mean of the sample from the null value.

``` r

t.test(mtcars$wt, mu = 2.7)
```

    > 
    >   One Sample t-test
    > 
    > data:  mtcars$wt
    > t = 3, df = 31, p-value = 0.005
    > alternative hypothesis: true mean is not equal to 2.7
    > 95 percent confidence interval:
    >  2.86 3.57
    > sample estimates:
    > mean of x 
    >      3.22

``` r

cohens_d(mtcars$wt, mu = 2.7)
```

    > Cohen's d |       95% CI
    > ------------------------
    > 0.53      | [0.15, 0.90]
    > 
    > - Deviation from a difference of 2.7.

``` r

hedges_g(mtcars$wt, mu = 2.7)
```

    > Hedges' g |       95% CI
    > ------------------------
    > 0.52      | [0.15, 0.87]
    > 
    > - Deviation from a difference of 2.7.

### Paired Samples

In a repeated-measures design, the same subjects are measured in
multiple conditions or time points. Unlike the case of independent
groups, there are multiple sources of variation that can be used to
standardized the differences between the means of the conditions /
times, and each provides a unique standardized mean difference.

The most basic option is compute from the paired samples difference
scores and compute a one-sample effect size. This effect size, known as
Cohen’s d_z, represents the difference in terms of its homogeneity (a
small but stable difference will have a large d_z).

``` r

sleep_wide <- datawizard::data_to_wide(sleep,
  id_cols = "ID",
  values_from = "extra",
  names_from = "group",
  names_prefix = "extra_"
)

t.test(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], paired = TRUE)
```

    > 
    >   Paired t-test
    > 
    > data:  sleep_wide[["extra_1"]] and sleep_wide[["extra_2"]]
    > t = -4, df = 9, p-value = 0.003
    > alternative hypothesis: true mean difference is not equal to 0
    > 95 percent confidence interval:
    >  -2.46 -0.70
    > sample estimates:
    > mean difference 
    >           -1.58

``` r

repeated_measures_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], method = "z")
```

    > d (z) |         95% CI
    > ----------------------
    > -1.17 | [-1.94, -0.41]
    > 
    > - Adjusted for small sample bias.

``` r

# same as:
hedges_g(sleep_wide[["extra_1"]] - sleep_wide[["extra_2"]])
```

    > Hedges' g |         95% CI
    > --------------------------
    > -1.17     | [-1.94, -0.38]

Other options try to get close to the value that would have been reached
if the samples were independant (see more info in the documentation of
[`repeated_measures_d()`](https://easystats.github.io/effectsize/reference/repeated_measures_d.md)):

``` r

repeated_measures_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]])
```

    > dᵣₘ   |         95% CI
    > ----------------------
    > -0.75 | [-1.17, -0.33]
    > 
    > - Adjusted for small sample bias.

``` r

repeated_measures_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], method = "av")
```

    > dₐᵥ   |         95% CI
    > ----------------------
    > -0.76 | [-1.13, -0.39]
    > 
    > - Adjusted for small sample bias.

``` r

repeated_measures_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], method = "b")
```

    > Becker's d |         95% CI
    > ---------------------------
    > -0.72      | [-1.20, -0.24]
    > 
    > - Adjusted for small sample bias.

``` r

repeated_measures_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], method = "d")
```

    > Cohen's d |         95% CI
    > --------------------------
    > -0.80     | [-1.29, -0.30]
    > 
    > - Adjusted for small sample bias.

``` r

# all closer to:
cohens_d(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]], ci = NULL)
```

    > Cohen's d
    > ---------
    > -0.83    
    > 
    > - Estimated using pooled SD.

For data containing repetition in each condition/subject, another effect
size (residual *d*) is also available:

``` r

data("rouder2016")

head(rouder2016)
```

    >   id cond    rt
    > 1  1    1 0.560
    > 2  1    1 0.930
    > 3  1    1 0.795
    > 4  1    1 0.615
    > 5  1    1 1.028
    > 6  1    1 0.845

``` r

repeated_measures_d(rt ~ cond | id, data = rouder2016, method = "r")
```

    > dᵣ    |         95% CI
    > ----------------------
    > -0.26 | [-0.33, -0.18]
    > 
    > - Adjusted for small sample bias.

### For a Bayesian *t*-test

A Bayesian estimate of Cohen’s *d* can also be provided based on
`BayesFactor`’s version of a *t*-test via the
[`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
function:

``` r

library(BayesFactor)
BFt <- ttestBF(formula = mpg ~ am, data = mtcars)

effectsize(BFt, type = "d")
```

    > Cohen's d |         95% CI
    > --------------------------
    > -1.29     | [-2.11, -0.52]
    > 
    > - Estimated using pooled SD.

### (Multivariate) Standardized Distances

When examining multivariate differences (e.g., with Hotelling’s T^2
test), Mahalanobis’ *D* can be used as the multivariate equivalent for
Cohen’s *d*. Unlike Cohen’s *d* which is a measure of standardized
*differences*, Mahalanobis’ *D* is a measure of standardized
*distances*. As such, it cannot be negative, and ranges from 0 (no
distance between the multivariate distributions) to +\infty.

``` r

mahalanobis_d(mpg + hp + cyl ~ am, data = mtcars)
```

    > Mahalanobis' D |      95% CI
    > ----------------------------
    > 2.14           | [1.22, Inf]
    > 
    > - One-sided CIs: upper bound fixed at [Inf].

### Means Ratio

Instead of the *difference* between means, we can also look at the
*ratio* between means. This effect size is only applicable to **ratio
scale** outcomes (variables with an absolute zero).

Lucky for us, miles-per-gallon is on a ratio scale!

``` r

means_ratio(mpg ~ am, data = mtcars)
```

    > Means Ratio (adj.) |       95% CI
    > ---------------------------------
    > 0.70               | [0.59, 0.83]

Values range between 0 and \infty, with values smaller than 1 indicating
that the second mean is larger than the first, values larger than 1
indicating that the second mean is smaller than the first, and values of
1 indicating that the means are equal.

## Dominance Effect Sizes

The rank-biserial correlation (r\_{rb}) is a measure of dominance:
larger values indicate that more of *X* is larger than more of *Y*, with
a value of (-1) indicates that *all* observations in the second group
are larger than the first, and a value of (+1) indicates that *all*
observations in the first group are larger than the second.

These effect sizes should be reported with the Wilcoxon (Mann-Whitney)
test or the signed-rank test (both available in
[`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html)).

### Two Independent Samples

``` r

A <- c(48, 48, 77, 86, 85, 85)
B <- c(14, 34, 34, 77)

wilcox.test(A, B, exact = FALSE) # aka Mann–Whitney U test
```

    > 
    >   Wilcoxon rank sum test with continuity correction
    > 
    > data:  A and B
    > W = 22, p-value = 0.05
    > alternative hypothesis: true location shift is not equal to 0

``` r

rank_biserial(A, B)
```

    > r (rank biserial) |       95% CI
    > --------------------------------
    > 0.79              | [0.30, 0.95]

### One Sample

For one sample, r\_{rb} measures the symmetry around \mu (mu; the null
value), with 0 indicating perfect symmetry, (-1) indicates that all
observations fall below \mu, and (+1) indicates that all observations
fall above \mu.

``` r

x <- c(1.15, 0.88, 0.90, 0.74, 1.21, 1.36, 0.89)

wilcox.test(x, mu = 1) # aka Signed-Rank test
```

    > 
    >   Wilcoxon signed rank exact test
    > 
    > data:  x
    > V = 16, p-value = 0.8
    > alternative hypothesis: true location is not equal to 1

``` r

rank_biserial(x, mu = 1)
```

    > r (rank biserial) |        95% CI
    > ---------------------------------
    > 0.14              | [-0.59, 0.75]
    > 
    > - Deviation from a difference of 1.

### Paired Samples

For paired samples, r\_{rb} measures the symmetry of the (paired)
*differences* around \mu as for the one sample case.

``` r

x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.88, 0.65, 0.60, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(x, y, paired = TRUE) # aka Signed-Rank test
```

    > 
    >   Wilcoxon signed rank exact test
    > 
    > data:  x and y
    > V = 40, p-value = 0.04
    > alternative hypothesis: true location shift is not equal to 0

``` r

rank_biserial(x, y, paired = TRUE)
```

    > r (rank biserial) |       95% CI
    > --------------------------------
    > 0.78              | [0.30, 0.94]

## Common Language Effect Sizes

Related effect sizes are the *common language effect sizes* which
present information about group differences in terms of probability.

### Two Independent Samples

#### Measures of (Non)Overlap

These measures indicate the degree two independent distributions
overlap: Cohen’s U_1 is the proportion of the total of both
distributions that does not overlap, while *Overlap (OVL)* is the
proportional overlap between the distributions.

``` r

cohens_u1(mpg ~ am, data = mtcars)
```

    > Cohen's U1 |       95% CI
    > -------------------------
    > 0.70       | [0.42, 0.85]

``` r

p_overlap(mpg ~ am, data = mtcars)
```

    > Overlap |       95% CI
    > ----------------------
    > 0.46    | [0.26, 0.74]

Note the by default, these functions return the parametric versions of
these effect sizes: these assume equal normal variance in both
populations. When these assumptions are not met, the values produced
will be biased in unknown ways. In such cases, we should use the
non-parametric versions (U_1 is not defined):

``` r

p_overlap(mpg ~ am, data = mtcars, parametric = FALSE)
```

    > Overlap |       95% CI
    > ----------------------
    > 0.69    | [0.30, 1.00]
    > 
    > - Non-parametric CLES

#### Probabilistic Measures

*Probability of superiority* is the probability that, when sampling an
observation from each of the groups at random, that the observation from
the second group will be larger than the sample from the first group.

``` r

p_superiority(mpg ~ am, data = mtcars)
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.15            | [0.05, 0.32]

Here, this indicates that if we were to randomly draw a sample from
`am==0` and from `am==1`, 15% of the time, the first will have a larger
`mpg` values than the second.

Cohen’s U_2 is the proportion of one of the groups that exceeds the same
proportion in the other group, and Cohen’s U_3 is the proportion of the
second group that is smaller than the median of the first group.

``` r

cohens_u2(mpg ~ am, data = mtcars)
```

    > Cohen's U2 |       95% CI
    > -------------------------
    > 0.77       | [0.63, 0.87]

``` r

cohens_u3(mpg ~ am, data = mtcars)
```

    > Cohen's U3 |       95% CI
    > -------------------------
    > 0.07       | [0.01, 0.25]

Here too we have a non-parametric versions when the assumptions of equal
variance of normal populations:

``` r

p_superiority(mpg ~ am, data = mtcars, parametric = FALSE)
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.17            | [0.08, 0.32]
    > 
    > - Non-parametric CLES

``` r

cohens_u2(mpg ~ am, data = mtcars, parametric = FALSE)
```

    > Cohen's U2 |       95% CI
    > -------------------------
    > 0.80       | [0.50, 1.00]
    > 
    > - Non-parametric CLES

``` r

cohens_u3(mpg ~ am, data = mtcars, parametric = FALSE)
```

    > Cohen's U3 |       95% CI
    > -------------------------
    > 0.15       | [0.00, 0.37]
    > 
    > - Non-parametric CLES

### One Sample and Paired Samples

For one sample, *probability of superiority* is the probability that,
when sampling an observation at random, it will be larger than \mu.

``` r

p_superiority(mtcars$wt, mu = 2.75)
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.63            | [0.53, 0.72]

``` r

p_superiority(mtcars$wt, mu = 2.75, parametric = FALSE)
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.74            | [0.57, 0.86]
    > 
    > - Non-parametric CLES

For paired samples, *probability of superiority* is the probability
that, when sampling an observation at random, its *difference* will be
larger than \mu.

``` r

p_superiority(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]],
  paired = TRUE, mu = -1
)
```

    > For paired samples, 'repeated_measures_d()' provides more options.

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.37            | [0.22, 0.56]

``` r

p_superiority(sleep_wide[["extra_1"]], sleep_wide[["extra_2"]],
  paired = TRUE, mu = -1,
  parametric = FALSE
)
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.19            | [0.06, 0.49]
    > 
    > - Non-parametric CLES

### For a Bayesian *t*-test

A Bayesian estimate of (the parametric version of) these effect sizes
can also be provided based on `BayesFactor`’s version of a *t*-test via
the
[`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
function:

``` r

effectsize(BFt, type = "p_superiority")
```

    > Pr(superiority) |       95% CI
    > ------------------------------
    > 0.18            | [0.07, 0.36]

``` r

effectsize(BFt, type = "u1")
```

    > Cohen's U1 |       95% CI
    > -------------------------
    > 0.65       | [0.32, 0.83]

``` r

effectsize(BFt, type = "u2")
```

    > Cohen's U2 |       95% CI
    > -------------------------
    > 0.74       | [0.60, 0.85]

``` r

effectsize(BFt, type = "u3")
```

    > Cohen's U3 |       95% CI
    > -------------------------
    > 0.10       | [0.02, 0.31]

``` r

effectsize(BFt, type = "overlap")
```

    > Overlap |       95% CI
    > ----------------------
    > 0.52    | [0.29, 0.80]

## References
