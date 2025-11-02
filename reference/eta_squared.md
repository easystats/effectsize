# \\\eta^2\\ and Other Effect Size for ANOVA

Functions to compute effect size measures for ANOVAs, such as Eta-
(\\\eta\\), Omega- (\\\omega\\) and Epsilon- (\\\epsilon\\) squared, and
Cohen's f (or their partialled versions) for ANOVA tables. These indices
represent an estimate of how much variance in the response variables is
accounted for by the explanatory variable(s).  
  
When passing models, effect sizes are computed using the sums of squares
obtained from `anova(model)` which might not always be appropriate. See
details.

## Usage

``` r
eta_squared(
  model,
  partial = TRUE,
  generalized = FALSE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)

omega_squared(
  model,
  partial = TRUE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)

epsilon_squared(
  model,
  partial = TRUE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)

cohens_f(
  model,
  partial = TRUE,
  generalized = FALSE,
  squared = FALSE,
  method = c("eta", "omega", "epsilon"),
  model2 = NULL,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)

cohens_f_squared(
  model,
  partial = TRUE,
  generalized = FALSE,
  squared = TRUE,
  method = c("eta", "omega", "epsilon"),
  model2 = NULL,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE,
  ...
)

eta_squared_posterior(
  model,
  partial = TRUE,
  generalized = FALSE,
  ss_function = stats::anova,
  draws = 500,
  verbose = TRUE,
  ...
)
```

## Arguments

- model:

  An ANOVA table (or an ANOVA-like table, e.g., outputs from
  [`parameters::model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html)),
  or a statistical model for which such a table can be extracted. See
  details.

- partial:

  If `TRUE`, return partial indices.

- generalized:

  A character vector of observed (non-manipulated) variables to be used
  in the estimation of a generalized Eta Squared. Can also be `TRUE`, in
  which case generalized Eta Squared is estimated assuming *none* of the
  variables are observed (all are manipulated). (For `afex_aov` models,
  when `TRUE`, the observed variables are extracted automatically from
  the fitted model, if they were provided during fitting.

- ci:

  Confidence Interval (CI) level

- alternative:

  a character string specifying the alternative hypothesis; Controls the
  type of CI returned: `"greater"` (default) or `"less"` (one-sided CI),
  or `"two.sided"` (two-sided CI). Partial matching is allowed (e.g.,
  `"g"`, `"l"`, `"two"`...). See *One-Sided CIs* in
  [effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

- verbose:

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to or from other methods.

  - Can be `include_intercept = TRUE` to include the effect size for the
    intercept (when it is included in the ANOVA table).

  - For Bayesian models, arguments passed to `ss_function`.

- squared:

  Return Cohen's *f* or Cohen's *f*-squared?

- method:

  What effect size should be used as the basis for Cohen's *f*?

- model2:

  Optional second model for Cohen's f (/squared). If specified, returns
  the effect size for R-squared-change between the two models.

- ss_function:

  For Bayesian models, the function used to extract sum-of-squares. Uses
  [`anova()`](https://rdrr.io/r/stats/anova.html) by default, but can
  also be [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) for
  simple linear models.

- draws:

  For Bayesian models, an integer indicating the number of draws from
  the posterior predictive distribution to return. Larger numbers take
  longer to run, but provide estimates that are more stable.

## Value

A data frame with the effect size(s) between 0-1 (`Eta2`, `Epsilon2`,
`Omega2`, `Cohens_f` or `Cohens_f2`, possibly with the `partial` or
`generalized` suffix), and their CIs (`CI_low` and `CI_high`).  
  
For `eta_squared_posterior()`, a data frame containing the ppd of the
Eta squared for each fixed effect, which can then be passed to
[`bayestestR::describe_posterior()`](https://easystats.github.io/bayestestR/reference/describe_posterior.html)
for summary stats.

A data frame containing the effect size values and their confidence
intervals.

## Details

For `aov` (or `lm`), `aovlist` and `afex_aov` models, and for `anova`
objects that provide Sums-of-Squares, the effect sizes are computed
directly using Sums-of-Squares. (For `maov` (or `mlm`) models, effect
sizes are computed for each response separately.)  
  
For other ANOVA tables and models (converted to ANOVA-like tables via
[`anova()`](https://rdrr.io/r/stats/anova.html) methods), effect sizes
are approximated via test statistic conversion of the omnibus *F*
statistic provided by the (see
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
for more details.)

### Type of Sums of Squares

When `model` is a statistical model, the sums of squares (or *F*
statistics) used for the computation of the effect sizes are based on
those returned by `anova(model)`. Different models have different
default output type. For example, for `aov` and `aovlist` these are
*type-1* sums of squares, but for `lmerMod` (and `lmerModLmerTest`)
these are *type-3* sums of squares. Make sure these are the sums of
squares you are interested in. You might want to convert your model to
an ANOVA(-like) table yourself and then pass the result to
`eta_squared()`. See examples below for use of
[`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html) and the `afex`
package.  
  
For type 3 sum of squares, it is generally recommended to fit models
with *orthogonal factor weights* (e.g., `contr.sum`) and *centered
covariates*, for sensible results. See examples and the `afex` package.

### Un-Biased Estimate of Eta

Both ***Omega*** and ***Epsilon*** are unbiased estimators of the
population's ***Eta***, which is especially important is small samples.
But which to choose?  
  
Though Omega is the more popular choice (Albers and Lakens, 2018),
Epsilon is analogous to adjusted R2 (Allen, 2017, p. 382), and has been
found to be less biased (Carroll & Nordholm, 1975).

### Cohen's f

Cohen's f can take on values between zero, when the population means are
all equal, and an indefinitely large number as standard deviation of
means increases relative to the average standard deviation within each
group.  
  
When comparing two models in a sequential regression analysis, Cohen's f
for R-square change is the ratio between the increase in R-square and
the percent of unexplained variance.  
  
Cohen has suggested that the values of 0.10, 0.25, and 0.40 represent
small, medium, and large effect sizes, respectively.

### Eta Squared from Posterior Predictive Distribution

For Bayesian models (fit with `brms` or `rstanarm`),
`eta_squared_posterior()` simulates data from the posterior predictive
distribution (ppd) and for each simulation the Eta Squared is computed
for the model's fixed effects. This means that the returned values are
the population level effect size as implied by the posterior model (and
not the effect size in the sample data). See
[`rstantools::posterior_predict()`](https://mc-stan.org/rstantools/reference/posterior_predict.html)
for more info.

## Confidence (Compatibility) Intervals (CIs)

Unless stated otherwise, confidence (compatibility) intervals (CIs) are
estimated using the noncentrality parameter method (also called the
"pivot method"). This method finds the noncentrality parameter ("*ncp*")
of a noncentral *t*, *F*, or \\\chi^2\\ distribution that places the
observed *t*, *F*, or \\\chi^2\\ test statistic at the desired
probability point of the distribution. For example, if the observed *t*
statistic is 2.0, with 50 degrees of freedom, for which cumulative
noncentral *t* distribution is *t* = 2.0 the .025 quantile (answer: the
noncentral *t* distribution with *ncp* = .04)? After estimating these
confidence bounds on the *ncp*, they are converted into the effect size
metric to obtain a confidence interval for the effect size (Steiger,
2004).  
  
For additional details on estimation and troubleshooting, see
[effectsize_CIs](https://easystats.github.io/effectsize/reference/effectsize_CIs.md).

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

- Albers, C., and Lakens, D. (2018). When power analyses based on pilot
  data are biased: Inaccurate effect size estimators and follow-up bias.
  Journal of experimental social psychology, 74, 187-195.

- Allen, R. (2017). Statistics and Experimental Design for
  Psychologists: A Model Comparison Approach. World Scientific
  Publishing Company.

- Carroll, R. M., & Nordholm, L. A. (1975). Sampling Characteristics of
  Kelley's epsilon and Hays' omega. Educational and Psychological
  Measurement, 35(3), 541-554.

- Kelley, T. (1935) An unbiased correlation ratio measure. Proceedings
  of the National Academy of Sciences. 21(9). 554-559.

- Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
  statistics: measures of effect size for some common research designs.
  Psychological methods, 8(4), 434.

- Steiger, J. H. (2004). Beyond the F test: Effect size confidence
  intervals and tests of close fit in the analysis of variance and
  contrast analysis. Psychological Methods, 9, 164-182.

## See also

[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)

Other effect sizes for ANOVAs:
[`rank_epsilon_squared()`](https://easystats.github.io/effectsize/reference/rank_epsilon_squared.md)

## Examples

``` r
data(mtcars)
mtcars$am_f <- factor(mtcars$am)
mtcars$cyl_f <- factor(mtcars$cyl)

model <- aov(mpg ~ am_f * cyl_f, data = mtcars)

(eta2 <- eta_squared(model))
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Eta2 (partial) |       95% CI
#> ------------------------------------------
#> am_f       |           0.63 | [0.42, 1.00]
#> cyl_f      |           0.66 | [0.45, 1.00]
#> am_f:cyl_f |           0.10 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

# More types:
eta_squared(model, partial = FALSE)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Eta2 |       95% CI
#> --------------------------------
#> am_f       | 0.36 | [0.13, 1.00]
#> cyl_f      | 0.41 | [0.14, 1.00]
#> am_f:cyl_f | 0.02 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
eta_squared(model, generalized = "cyl_f")
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Eta2 (generalized) |       95% CI
#> ----------------------------------------------
#> am_f       |               0.36 | [0.13, 1.00]
#> cyl_f      |               0.63 | [0.42, 1.00]
#> am_f:cyl_f |               0.04 | [0.00, 1.00]
#> 
#> - Observed variables: cyl_f
#> - One-sided CIs: upper bound fixed at [1.00].
omega_squared(model)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Omega2 (partial) |       95% CI
#> --------------------------------------------
#> am_f       |             0.57 | [0.35, 1.00]
#> cyl_f      |             0.60 | [0.37, 1.00]
#> am_f:cyl_f |             0.02 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
epsilon_squared(model)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Epsilon2 (partial) |       95% CI
#> ----------------------------------------------
#> am_f       |               0.61 | [0.40, 1.00]
#> cyl_f      |               0.63 | [0.41, 1.00]
#> am_f:cyl_f |               0.03 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
cohens_f(model)
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Cohen's f (partial) |      95% CI
#> ----------------------------------------------
#> am_f       |                1.30 | [0.86, Inf]
#> cyl_f      |                1.38 | [0.90, Inf]
#> am_f:cyl_f |                0.33 | [0.00, Inf]
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

model0 <- aov(mpg ~ am_f + cyl_f, data = mtcars) # no interaction
cohens_f_squared(model0, model2 = model)
#> Cohen's f2 (partial) |      95% CI | R2_delta
#> ---------------------------------------------
#> 0.11                 | [0.00, Inf] |     0.02
#> 
#> - One-sided CIs: upper bound fixed at [Inf].

## Interpretation of effect sizes
## ------------------------------

interpret_omega_squared(0.10, rules = "field2013")
#> [1] "medium"
#> (Rules: field2013)
#> 
interpret_eta_squared(0.10, rules = "cohen1992")
#> [1] "small"
#> (Rules: cohen1992)
#> 
interpret_epsilon_squared(0.10, rules = "cohen1992")
#> [1] "small"
#> (Rules: cohen1992)
#> 

interpret(eta2, rules = "cohen1992")
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter  | Eta2 (partial) |       95% CI | Interpretation
#> -----------------------------------------------------------
#> am_f       |           0.63 | [0.42, 1.00] |          large
#> cyl_f      |           0.66 | [0.45, 1.00] |          large
#> am_f:cyl_f |           0.10 | [0.00, 1.00] |          small
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
#> - Interpretation rule: cohen1992

if (FALSE) { # require("see") && interactive()
plot(eta2) # Requires the {see} package
}
# Recommended: Type-2 or -3 effect sizes + effects coding
# -------------------------------------------------------
contrasts(mtcars$am_f) <- contr.sum
contrasts(mtcars$cyl_f) <- contr.sum

model <- aov(mpg ~ am_f * cyl_f, data = mtcars)
model_anova <- car::Anova(model, type = 3)

epsilon_squared(model_anova)
#> Type 3 ANOVAs only give sensible and informative results when covariates
#>   are mean-centered and factors are coded with orthogonal contrasts (such
#>   as those produced by `contr.sum`, `contr.poly`, or `contr.helmert`, but
#>   *not* by the default `contr.treatment`).
#> # Effect Size for ANOVA (Type III)
#> 
#> Parameter  | Epsilon2 (partial) |       95% CI
#> ----------------------------------------------
#> am_f       |               0.08 | [0.00, 1.00]
#> cyl_f      |               0.60 | [0.38, 1.00]
#> am_f:cyl_f |               0.03 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
# afex takes care of both type-3 effects and effects coding:
data(obk.long, package = "afex")
model <- afex::aov_car(value ~ gender + Error(id / (phase * hour)),
  data = obk.long, observed = "gender"
)
#> Contrasts set to contr.sum for the following variables: gender

omega_squared(model)
#> # Effect Size for ANOVA (Type III)
#> 
#> Parameter         | Omega2 (partial) |       95% CI
#> ---------------------------------------------------
#> gender            |             0.00 | [0.00, 1.00]
#> phase             |             0.16 | [0.00, 1.00]
#> gender:phase      |             0.00 | [0.00, 1.00]
#> hour              |             0.13 | [0.00, 1.00]
#> gender:hour       |             0.00 | [0.00, 1.00]
#> phase:hour        |         3.73e-03 | [0.00, 1.00]
#> gender:phase:hour |             0.00 | [0.00, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
eta_squared(model, generalized = TRUE) # observed vars are pulled from the afex model.
#> # Effect Size for ANOVA (Type III)
#> 
#> Parameter         | Eta2 (generalized) |       95% CI
#> -----------------------------------------------------
#> gender            |               0.03 | [0.00, 1.00]
#> phase             |               0.15 | [0.00, 1.00]
#> gender:phase      |           3.53e-34 | [0.00, 1.00]
#> hour              |               0.10 | [0.00, 1.00]
#> gender:hour       |           2.36e-03 | [0.00, 1.00]
#> phase:hour        |               0.01 | [0.00, 1.00]
#> gender:phase:hour |           6.54e-03 | [0.00, 1.00]
#> 
#> - Observed variables: gender
#> - One-sided CIs: upper bound fixed at [1.00].
if (FALSE) { # require("lmerTest") && require("lme4") && FALSE
## Approx. effect sizes for mixed models
## -------------------------------------
model <- lme4::lmer(mpg ~ am_f * cyl_f + (1 | vs), data = mtcars)
omega_squared(model)
}
if (FALSE) { # require(rstanarm) && require(bayestestR) && require(car) && interactive()
## Bayesian Models (PPD)
## ---------------------
fit_bayes <- rstanarm::stan_glm(
  mpg ~ factor(cyl) * wt + qsec,
  data = mtcars, family = gaussian(),
  refresh = 0
)

es <- eta_squared_posterior(fit_bayes,
  verbose = FALSE,
  ss_function = car::Anova, type = 3
)
bayestestR::describe_posterior(es, test = NULL)


# compare to:
fit_freq <- lm(mpg ~ factor(cyl) * wt + qsec,
  data = mtcars
)
aov_table <- car::Anova(fit_freq, type = 3)
eta_squared(aov_table)
}
```
