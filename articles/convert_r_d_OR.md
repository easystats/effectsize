# Converting Between r, d, and Odds Ratios

The `effectsize` package contains function to convert among indices of
effect size. This can be useful for meta-analyses, or any comparison
between different types of statistical analyses.

## Converting Between *d* and *r*

The most basic conversion is between *r* values, a measure of
standardized association between two continuous measures, and *d* values
(such as Cohen’s *d*), a measure of standardized differences between two
groups / conditions.

Let’s looks at some (simulated) data:

``` r

library(effectsize)
data("hardlyworking")
head(hardlyworking)
```

    >   salary xtra_hours n_comps age seniority is_senior
    > 1  19745       4.16       1  32         3     FALSE
    > 2  11302       1.62       0  34         3     FALSE
    > 3  20636       1.19       3  33         5      TRUE
    > 4  23047       7.19       1  35         3     FALSE
    > 5  27342      11.26       0  33         4     FALSE
    > 6  25657       3.63       2  30         5      TRUE

We can compute Cohen’s *d* between the two groups:

``` r

cohens_d(salary ~ is_senior, data = hardlyworking)
```

    > Cohen's d |         95% CI
    > --------------------------
    > -0.72     | [-0.90, -0.53]
    > 
    > - Estimated using pooled SD.

But we can also compute a point-biserial correlation, which is Pearson’s
*r* when treating the 2-level `is_senior` variable as a numeric binary
variable:

``` r

correlation::cor_test(hardlyworking, "salary", "is_senior")
```

    > Parameter1 | Parameter2 |    r |       95% CI | t(498) |         p
    > ------------------------------------------------------------------
    > salary     |  is_senior | 0.34 | [0.26, 0.41] |   7.95 | < .001***
    > 
    > Observations: 500

But what if we only have summary statistics? Say, we only have d=-0.72
and we want to know what the *r* would have been? We can approximate *r*
using the following formula (Borenstein et al. 2009):

r \approx \frac{d}{\sqrt{d^2 + 4}} And indeed, if we use
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
we get a pretty decent approximation:

``` r

d_to_r(-0.72)
```

    > [1] -0.339

(Which also works in the other way, with `r_to_d(0.12)` gives 0.723)

As we can see, these are rough approximations, but they can be useful
when we don’t have the raw data on hand.

### In multiple regression

Although not exactly a classic Cohen’s d, we can also approximate a
partial-*d* value (that is, the standardized difference between two
groups / conditions, with variance from other predictors partilled out).
For example:

``` r

fit <- lm(salary ~ is_senior + xtra_hours, data = hardlyworking)

parameters::model_parameters(fit)
```

    > Parameter     | Coefficient |     SE |               95% CI | t(497) |      p
    > -----------------------------------------------------------------------------
    > (Intercept)   |    14258.87 | 238.71 | [13789.86, 14727.87] |  59.73 | < .001
    > is seniorTRUE |     1683.65 | 316.85 | [ 1061.12,  2306.17] |   5.31 | < .001
    > xtra hours    |     1257.75 |  40.33 | [ 1178.51,  1336.99] |  31.19 | < .001

    > 
    > Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    >   using a Wald t-distribution approximation.

``` r

# A couple of ways to get partial-d:
1683.65 / sigma(fit)
```

    > [1] 0.495

``` r

t_to_d(5.31, df_error = 497)[[1]]
```

    > [1] 0.476

We can convert these semi-*d* values to *r* values, but in this case
these represent the *partial* correlation:

``` r

t_to_r(5.31, df_error = 497)
```

    > r    |       95% CI
    > -------------------
    > 0.23 | [0.15, 0.31]

``` r

correlation::correlation(hardlyworking[, c("salary", "xtra_hours", "is_senior")],
  include_factors = TRUE,
  partial = TRUE
)[2, ]
```

    > # Correlation Matrix (pearson-method)
    > 
    > Parameter1 | Parameter2 |    r |       95% CI | t(498) |         p
    > ------------------------------------------------------------------
    > salary     |  is_senior | 0.23 | [0.15, 0.31] |   5.32 | < .001***
    > 
    > p-value adjustment method: Holm (1979)
    > Observations: 500

``` r

# all close to:
d_to_r(0.47)
```

    > [1] 0.229

## Converting Between *OR* and *d*

In binomial regression (more specifically in logistic regression), Odds
ratios (OR) are themselves measures of effect size; they indicate the
expected change in the odds of a some event.

In some fields, it is common to dichotomize outcomes in order to be able
to analyze them with logistic models. For example, if the outcome is the
count of white blood cells, it can be more useful (medically) to predict
the crossing of the threshold rather than the raw count itself. And so,
where some scientists would maybe analyze the above data with a *t*-test
and present Cohen’s *d*, others might analyze it with a logistic
regression model on the dichotomized outcome, and present OR. So the
question can be asked: given such a OR, what would Cohen’s *d* have
been?

Fortunately, there is a formula to approximate this (Sánchez-Meca et al.
2003):

d = log(OR) \times \frac{\sqrt{3}}{\pi}

which is implemented in the
[`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
function.

Let’s give it a try:

``` r

# 1. Set a threshold
thresh <- 22500

# 2. dichotomize the outcome
hardlyworking$salary_low <- factor(hardlyworking$salary < thresh,
  labels = c("high", "low")
)

# 3. Fit a logistic regression:
fit <- glm(salary_low ~ is_senior,
  data = hardlyworking,
  family = binomial()
)

parameters::model_parameters(fit)
```

    > Parameter     | Log-Odds |   SE |         95% CI |     z |      p
    > -----------------------------------------------------------------
    > (Intercept)   |     1.55 | 0.16 | [ 1.25,  1.87] |  9.86 | < .001
    > is seniorTRUE |    -1.22 | 0.21 | [-1.63, -0.82] | -5.86 | < .001

    > 
    > Uncertainty intervals (profile-likelihood) and p-values (two-tailed)
    >   computed using a Wald z-distribution approximation.

    > 
    > The model has a log- or logit-link. Consider using `exponentiate =
    >   TRUE` to interpret coefficients as ratios.

``` r

# Convert log(OR) (the coefficient) to d
oddsratio_to_d(-1.22, log = TRUE)
```

    > [1] -0.673

That’s very close to Cohen’s *d* we got above (d=-0.72).

We can get an even closer estimate by accounting for the rate of low
salaries in the reference group.

``` r

proportions(
  table(
    is_senior = hardlyworking$is_senior,
    salary_low = hardlyworking$salary_low
  ),
  margin = 1
)
```

    >          salary_low
    > is_senior  high   low
    >     FALSE 0.175 0.825
    >     TRUE  0.418 0.582

``` r

# Or
odds_to_probs(1.55, log = TRUE)
```

    > [1] 0.825

As we can see, 82.5% of non-senior workers have a low salary. We can
plug that in to
[`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md):

``` r

oddsratio_to_d(-1.22, p0 = 0.825, log = TRUE)
```

    > [1] -0.728

We have successfully recovered the standardized mean difference between
seniors and non-senior’ salaries by only observing a dichotomize salary
(“low/high salary”).

## References

Borenstein, Michael, Larry V Hedges, JPT Higgins, and Hannah R
Rothstein. 2009. “Converting Among Effect Sizes.” *Introduction to
Meta-Analysis*, 45–49.

Sánchez-Meca, Julio, Fulgencio Marı́n-Martı́nez, and Salvador
Chacón-Moscoso. 2003. “Effect-Size Indices for Dichotomized Outcomes in
Meta-Analysis.” *Psychological Methods* 8 (4): 448.
