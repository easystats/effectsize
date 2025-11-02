# Effect Size from Test Statistics

## Introduction

In many real world applications there are no straightforward ways of
obtaining standardized effect sizes. However, it is possible to get
approximations of most of the effect size indices (d, r, \eta^2_p…) with
the use of test statistics. These conversions are based on the idea that
**test statistics are a function of effect size and sample size**. Thus
information about samples size (or more often of degrees of freedom) is
used to reverse-engineer indices of effect size from test statistics.
This idea and these functions also power our [***Effect Sizes From Test
Statistics*** *shiny
app*](https://easystats4u.shinyapps.io/statistic2effectsize/).

The measures discussed here are, in one way or another, ***signal to
noise ratios***, with the “noise” representing the unaccounted variance
in the outcome variable[^1].

The indices are:

- Percent variance explained (\eta^2_p, \omega^2_p, \epsilon^2_p).

- Measure of association (r).

- Measure of difference (d).

### (Partial) Percent Variance Explained

These measures represent the ratio of Signal^2 / (Signal^2 + Noise^2),
with the “noise” having all other “signals” partial-ed out (be they of
other fixed or random effects). The most popular of these indices is
\eta^2_p (Eta; which is equivalent to R^2).

The conversion of the F- or t-statistic is based on Friedman (1982).

Let’s look at an example:

``` r

library(afex)

data(md_12.1)

aov_fit <- aov_car(rt ~ angle * noise + Error(id / (angle * noise)),
  data = md_12.1,
  anova_table = list(correction = "none", es = "pes")
)
aov_fit
```

    > Anova Table (Type 3 tests)
    > 
    > Response: rt
    >        Effect    df     MSE         F  pes p.value
    > 1       angle 2, 18 3560.00 40.72 *** .819   <.001
    > 2       noise  1, 9 8460.00 33.77 *** .790   <.001
    > 3 angle:noise 2, 18 1160.00 45.31 *** .834   <.001
    > ---
    > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

Let’s compare the \eta^2_p (the `pes` column) obtained here with ones
recovered from
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md):

``` r

library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)

F_to_eta2(
  f = c(40.72, 33.77, 45.31),
  df = c(2, 1, 2),
  df_error = c(18, 9, 18)
)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.82         | [0.66, 1.00]
    > 0.79         | [0.49, 1.00]
    > 0.83         | [0.69, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

**They are identical!**[^2] (except for the fact that
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
also provides confidence intervals[^3] :)

In this case we were able to easily obtain the effect size (thanks to
`afex`!), but in other cases it might not be as easy, and using
estimates based on test statistic offers a good approximation.

For example:

#### In Simple Effect and Contrast Analysis

``` r

library(emmeans)
```

    > Welcome to emmeans.
    > Caution: You lose important information if you filter this package's results.
    > See '? untidy'

``` r

joint_tests(aov_fit, by = "noise")
```

    > noise = absent:
    >  model term df1 df2 F.ratio p.value
    >  angle        2   9   8.000  0.0096
    > 
    > noise = present:
    >  model term df1 df2 F.ratio p.value
    >  angle        2   9  51.000  <.0001

``` r

F_to_eta2(
  f = c(8, 51),
  df = 2,
  df_error = 9
)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.64         | [0.18, 1.00]
    > 0.92         | [0.78, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

We can also use
[`t_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
for contrast analysis:

``` r

pairs(emmeans(aov_fit, ~angle))
```

    >  contrast estimate   SE df t.ratio p.value
    >  X0 - X4      -108 17.4  9  -6.200  0.0004
    >  X0 - X8      -168 20.6  9  -8.200  0.0001
    >  X4 - X8       -60 18.4  9  -3.300  0.0244
    > 
    > Results are averaged over the levels of: noise 
    > P value adjustment: tukey method for comparing a family of 3 estimates

``` r

t_to_eta2(
  t = c(-6.2, -8.2, -3.2),
  df_error = 9
)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.81         | [0.54, 1.00]
    > 0.88         | [0.70, 1.00]
    > 0.53         | [0.11, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

#### In Linear Mixed Models

``` r

library(lmerTest)

fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova(fit_lmm)
```

    > Type III Analysis of Variance Table with Satterthwaite's method
    >      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)    
    > Days  30031   30031     1    17    45.9 3.3e-06 ***
    > ---
    > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r

F_to_eta2(45.9, 1, 17)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.73         | [0.51, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

We can also use
[`t_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
for the slope of `Days` (which in this case gives the same result).

``` r

parameters::model_parameters(fit_lmm, effects = "fixed", ci_method = "satterthwaite")
```

    > # Fixed Effects
    > 
    > Parameter   | Coefficient |   SE |           95% CI | t(17.00) |      p
    > -----------------------------------------------------------------------
    > (Intercept) |      251.41 | 6.82 | [237.01, 265.80] |    36.84 | < .001
    > Days        |       10.47 | 1.55 | [  7.21,  13.73] |     6.77 | < .001

    > 
    > Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    >   using a Wald t-distribution with Satterthwaite approximation.

``` r

t_to_eta2(6.77, df_error = 17)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.73         | [0.51, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

#### Bias-Corrected Indices

Alongside \eta^2_p there are also the less biased \omega_p^2 (Omega) and
\epsilon^2_p (Epsilon; sometimes called \text{Adj. }\eta^2_p, which is
equivalent to R^2\_{adj}; Albers and Lakens (2018), Mordkoff (2019)).

``` r

F_to_eta2(45.9, 1, 17)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.73         | [0.51, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

F_to_epsilon2(45.9, 1, 17)
```

    > ε² (partial) |       95% CI
    > ---------------------------
    > 0.71         | [0.48, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

F_to_omega2(45.9, 1, 17)
```

    > ω² (partial) |       95% CI
    > ---------------------------
    > 0.70         | [0.47, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

### Measure of Association

Similar to \eta^2_p, r is a signal to noise ratio, and is in fact equal
to \sqrt{\eta^2_p} (so it’s really a *partial* r). It is often used
instead of \eta^2_p when discussing the *strength* of association (but I
suspect people use it instead of \eta^2_p because it gives a bigger
number, which looks better).

#### For Slopes

``` r

parameters::model_parameters(fit_lmm, effects = "fixed", ci_method = "satterthwaite")
```

    > # Fixed Effects
    > 
    > Parameter   | Coefficient |   SE |           95% CI | t(17.00) |      p
    > -----------------------------------------------------------------------
    > (Intercept) |      251.41 | 6.82 | [237.01, 265.80] |    36.84 | < .001
    > Days        |       10.47 | 1.55 | [  7.21,  13.73] |     6.77 | < .001

    > 
    > Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    >   using a Wald t-distribution with Satterthwaite approximation.

``` r

t_to_r(6.77, df_error = 17)
```

    > r    |       95% CI
    > -------------------
    > 0.85 | [0.67, 0.92]

In a fixed-effect linear model, this returns the **partial**
correlation. Compare:

``` r

fit_lm <- lm(rating ~ complaints + critical, data = attitude)

parameters::model_parameters(fit_lm)
```

    > Parameter   | Coefficient |    SE |         95% CI | t(27) |      p
    > -------------------------------------------------------------------
    > (Intercept) |       14.25 | 11.17 | [-8.67, 37.18] |  1.28 | 0.213 
    > complaints  |        0.75 |  0.10 | [ 0.55,  0.96] |  7.46 | < .001
    > critical    |    1.91e-03 |  0.14 | [-0.28,  0.28] |  0.01 | 0.989

    > 
    > Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    >   using a Wald t-distribution approximation.

``` r

t_to_r(
  t = c(7.46, 0.01),
  df_error = 27
)
```

    > r        |        95% CI
    > ------------------------
    > 0.82     | [ 0.67, 0.89]
    > 1.92e-03 | [-0.35, 0.35]

to:

``` r

correlation::correlation(attitude,
  select = "rating",
  select2 = c("complaints", "critical"),
  partial = TRUE
)
```

    > # Correlation Matrix (pearson-method)
    > 
    > Parameter1 | Parameter2 |        r |        95% CI | t(28) |         p
    > ----------------------------------------------------------------------
    > rating     | complaints |     0.82 | [ 0.65, 0.91] |  7.60 | < .001***
    > rating     |   critical | 2.70e-03 | [-0.36, 0.36] |  0.01 | 0.989    
    > 
    > p-value adjustment method: Holm (1979)
    > Observations: 30

#### In Contrast Analysis

This measure is also sometimes used in contrast analysis, where it is
called the point bi-serial correlation - r\_{pb} (Cohen et al. 1965;
Rosnow et al. 2000):

``` r

pairs(emmeans(aov_fit, ~angle))
```

    >  contrast estimate   SE df t.ratio p.value
    >  X0 - X4      -108 17.4  9  -6.200  0.0004
    >  X0 - X8      -168 20.6  9  -8.200  0.0001
    >  X4 - X8       -60 18.4  9  -3.300  0.0244
    > 
    > Results are averaged over the levels of: noise 
    > P value adjustment: tukey method for comparing a family of 3 estimates

``` r

t_to_r(
  t = c(-6.2, -8.2, -3.2),
  df_error = 9
)
```

    > r     |         95% CI
    > ----------------------
    > -0.90 | [-0.95, -0.67]
    > -0.94 | [-0.97, -0.80]
    > -0.73 | [-0.88, -0.23]

### Measures of Difference

These indices represent Signal/Noise with the “signal” representing the
difference between two means. This is akin to Cohen’s d, and is a close
approximation when comparing two groups of equal size (Wolf 1986; Rosnow
et al. 2000).

These can be useful in contrast analyses.

#### Between-Subject Contrasts

``` r

m <- lm(breaks ~ tension, data = warpbreaks)

em_tension <- emmeans(m, ~tension)
pairs(em_tension)
```

    >  contrast estimate SE df t.ratio p.value
    >  L - M        10.0  4 51   2.500  0.0400
    >  L - H        14.7  4 51   3.700  <.0001
    >  M - H         4.7  4 51   1.200  0.4600
    > 
    > P value adjustment: tukey method for comparing a family of 3 estimates

``` r

t_to_d(
  t = c(2.53, 3.72, 1.20),
  df_error = 51
)
```

    > d    |        95% CI
    > --------------------
    > 0.71 | [ 0.14, 1.27]
    > 1.04 | [ 0.45, 1.62]
    > 0.34 | [-0.22, 0.89]

However, these are merely approximations of a *true* Cohen’s *d*. It is
advised to directly estimate Cohen’s *d*, whenever possible. For
example, here with
[`emmeans::eff_size()`](https://rvlenth.github.io/emmeans/reference/eff_size.html):

``` r

eff_size(em_tension, sigma = sigma(m), edf = df.residual(m))
```

    >  contrast effect.size   SE df lower.CL upper.CL
    >  L - M           0.84 0.34 51     0.15     1.53
    >  L - H           1.24 0.36 51     0.53     1.95
    >  M - H           0.40 0.34 51    -0.28     1.07
    > 
    > sigma used for effect sizes: 11.88 
    > Confidence level used: 0.95

#### Within-Subject Contrasts

``` r

pairs(emmeans(aov_fit, ~angle))
```

    >  contrast estimate   SE df t.ratio p.value
    >  X0 - X4      -108 17.4  9  -6.200  0.0004
    >  X0 - X8      -168 20.6  9  -8.200  0.0001
    >  X4 - X8       -60 18.4  9  -3.300  0.0244
    > 
    > Results are averaged over the levels of: noise 
    > P value adjustment: tukey method for comparing a family of 3 estimates

``` r

t_to_d(
  t = c(-6.2, -8.2, -3.3),
  df_error = 9,
  paired = TRUE
)
```

    > d     |         95% CI
    > ----------------------
    > -2.07 | [-3.19, -0.91]
    > -2.73 | [-4.12, -1.32]
    > -1.10 | [-1.90, -0.26]

(Note set `paired = TRUE` to not over estimate the size of the effect;
Rosenthal (1991); Rosnow et al. (2000))

## References

Albers, Casper, and Daniël Lakens. 2018. “When Power Analyses Based on
Pilot Data Are Biased: Inaccurate Effect Size Estimators and Follow-up
Bias.” *Journal of Experimental Social Psychology* 74: 187–95.

Cohen, Jacob et al. 1965. “Some Statistical Issues in Psychological
Research.” *Handbook of Clinical Psychology*, 95–121.

Friedman, Herbert. 1982. “Simplified Determinations of Statistical
Power, Magnitude of Effect and Research Sample Sizes.” *Educational and
Psychological Measurement* 42 (2): 521–26.

Mordkoff, J Toby. 2019. “A Simple Method for Removing Bias from a
Popular Measure of Standardized Effect Size: Adjusted Partial Eta
Squared.” *Advances in Methods and Practices in Psychological Science* 2
(3): 228–32.

Rosenthal, Robert. 1991. “Meta-Analytic Procedures for Social Sciences.”
*Newbury Park, CA: Sage* 10: 9781412984997.

Rosnow, Ralph L, Robert Rosenthal, and Donald B Rubin. 2000. “Contrasts
and Correlations in Effect-Size Estimation.” *Psychological Science* 11
(6): 446–53.

Wolf, Fredric M. 1986. *Meta-Analysis: Quantitative Methods for Research
Synthesis*. Vol. 59. Sage.

[^1]: Note that for generalized linear models (Poisson, Logistic…),
    where the outcome is never on an arbitrary scale, estimates
    themselves **are** indices of effect size! Thus this vignette is
    relevant only to general linear models.

[^2]: Note that these are *partial* percent variance explained, and so
    their sum can be larger than 1.

[^3]: Confidence intervals for all indices are estimated using the
    non-centrality parameter method; These methods search for a the best
    non-central parameter of the non-central F/t distribution for the
    desired tail-probabilities, and then convert these ncps to the
    corresponding effect sizes.
