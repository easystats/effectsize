# Effect Sizes for ANOVAs

### Eta²

In the context of ANOVA-like tests, it is common to report ANOVA-like
effect sizes. These effect sizes represent the amount of variance
explained by each of the model’s terms, where each term can be
represented by 1 *or more* parameters.

For example, in the following case, the parameters for the `treatment`
term represent specific contrasts between the factor’s levels (treatment
groups) - the difference between each level and the reference level
(`obk.long == 'control'`).

``` r

data(obk.long, package = "afex")
# modify the data slightly for the demonstration:
obk.long <- obk.long[1:240 %% 3 == 0, ]
obk.long$id <- seq_len(nrow(obk.long))

m <- lm(value ~ treatment, data = obk.long)

parameters::model_parameters(m)
```

    > Parameter     | Coefficient |   SE |       95% CI | t(77) |      p
    > ------------------------------------------------------------------
    > (Intercept)   |        4.28 | 0.36 | [3.56, 5.00] | 11.85 | < .001
    > treatment [A] |        1.97 | 0.54 | [0.89, 3.05] |  3.64 | < .001
    > treatment [B] |        2.09 | 0.47 | [1.15, 3.03] |  4.42 | < .001

    > 
    > Uncertainty intervals (equal-tailed) and p-values (two-tailed) computed
    >   using a Wald t-distribution approximation.

But we can also ask about the overall effect of `treatment` - how much
of the variation in our dependent variable `value` can be predicted by
(or explained by) the variation between the `treatment` groups. Such a
question can be answered with an ANOVA test:

``` r

parameters::model_parameters(anova(m))
```

    > Parameter | Sum_Squares | df | Mean_Square |     F |      p
    > -----------------------------------------------------------
    > treatment |       72.23 |  2 |       36.11 | 11.08 | < .001
    > Residuals |      250.96 | 77 |        3.26 |       |       
    > 
    > Anova Table (Type 1 tests)

As we can see, the variance in `value` (the *sums-of-squares*, or *SS*)
has been split into pieces:

- The part associated with `treatment`.
- The unexplained part (The Residual-*SS*).

We can now ask what is the percent of the total variance in `value` that
is associated with `treatment`. This measure is called Eta-squared
(written as \eta^2):

\eta^2 = \frac{SS\_{effect}}{SS\_{total}} = \frac{72.23}{72.23 + 250.96}
= 0.22

and can be accessed via the
[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
function:

``` r

library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)


eta_squared(m, partial = FALSE)
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter |   η² |       95% CI
    > -------------------------------
    > treatment | 0.22 | [0.09, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

#### Adding More Terms

When we add more terms to our model, we can ask two different questions
about the percent of variance explained by a predictor - how much
variance is accounted by the predictor in *total*, and how much is
accounted when *controlling* for any other predictors. The latter
questions is answered by the *partial*-Eta squared (\eta^2_p), which is
the percent of the **partial** variance (after accounting for other
predictors in the model) associated with a term:

\eta^2_p = \frac{SS\_{effect}}{SS\_{effect} + SS\_{error}} which can
also be accessed via the
[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
function:

``` r

m <- lm(value ~ gender + phase + treatment, data = obk.long)

eta_squared(m, partial = FALSE)
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter |       η² |       95% CI
    > -----------------------------------
    > gender    |     0.03 | [0.00, 1.00]
    > phase     | 9.48e-03 | [0.00, 1.00]
    > treatment |     0.25 | [0.11, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

eta_squared(m) # partial = TRUE by default
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter | η² (partial) |       95% CI
    > ---------------------------------------
    > gender    |         0.04 | [0.00, 1.00]
    > phase     |         0.01 | [0.00, 1.00]
    > treatment |         0.26 | [0.12, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

*(`phase` is a repeated-measures variable, but for simplicity it is not
modeled as such.)*

In the calculation above, the *SS*s were computed sequentially - that is
the *SS* for `phase` is computed after controlling for `gender`, and the
*SS* for `treatment` is computed after controlling for both `gender` and
`phase`. This method of sequential *SS* is called also *type-I* test. If
this is what you want, that’s great - however in many fields (and other
statistical programs) it is common to use “simultaneous” sums of squares
(*type-II* or *type-III* tests), where each *SS* is computed controlling
for all other predictors, regardless of order. This can be done with
`car::Anova(type = ...)`:

``` r

eta_squared(car::Anova(m, type = 2), partial = FALSE)
```

    > # Effect Size for ANOVA (Type II)
    > 
    > Parameter |       η² |       95% CI
    > -----------------------------------
    > gender    |     0.05 | [0.00, 1.00]
    > phase     | 9.22e-03 | [0.00, 1.00]
    > treatment |     0.24 | [0.11, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

eta_squared(car::Anova(m, type = 3)) # partial = TRUE by default
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter | η² (partial) |       95% CI
    > ---------------------------------------
    > gender    |         0.07 | [0.01, 1.00]
    > phase     |         0.01 | [0.00, 1.00]
    > treatment |         0.26 | [0.12, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

\eta^2_p will always be larger than \eta^2. The idea is to simulate the
effect size in a design where only the term of interest was manipulated.
This terminology assumes some causal relationship between the predictor
and the outcome, which reflects the experimental world from which these
analyses and measures hail; However, \eta^2_p can also simply be seen as
a **signal-to-noise- ratio**, as it only uses the term’s *SS* and the
error-term’s *SS*.\[^in repeated-measure designs the term-specific
residual-*SS* is used for the computation of the effect size\].

(Note that in a one-way fixed-effect designs \eta^2 = \eta^2_p.)

#### Adding Interactions

Type II and type III treat interaction differently. Without going into
the weeds here, keep in mind that **when using type III SS, it is
important to center all of the predictors**; for numeric variables this
can be done by mean-centering the predictors; for factors this can be
done by using orthogonal coding (such as `contr.sum` for
*effects-coding*) for the dummy variables (and *NOT* treatment coding,
which is the default in R). This unfortunately makes parameter
interpretation harder, but *only* when this is does do the *SS*s
associated with each lower-order term (or lower-order interaction)
represent the ***SS*** of the **main effect** (with treatment coding
they represent the *SS* of the simple effects).

``` r

# compare
m_interaction1 <- lm(value ~ treatment * gender, data = obk.long)

# to:
m_interaction2 <- lm(
  value ~ treatment * gender,
  data = obk.long,
  contrasts = list(
    treatment = "contr.sum",
    gender = "contr.sum"
  )
)

eta_squared(car::Anova(m_interaction1, type = 3))
```

    > Type 3 ANOVAs only give sensible and informative results when covariates
    >   are mean-centered and factors are coded with orthogonal contrasts (such
    >   as those produced by `contr.sum`, `contr.poly`, or `contr.helmert`, but
    >   *not* by the default `contr.treatment`).

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | η² (partial) |       95% CI
    > ----------------------------------------------
    > treatment        |         0.12 | [0.02, 1.00]
    > gender           |     9.11e-03 | [0.00, 1.00]
    > treatment:gender |         0.20 | [0.07, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

eta_squared(car::Anova(m_interaction2, type = 3))
```

    > Type 3 ANOVAs only give sensible and informative results when covariates
    >   are mean-centered and factors are coded with orthogonal contrasts (such
    >   as those produced by `contr.sum`, `contr.poly`, or `contr.helmert`, but
    >   *not* by the default `contr.treatment`).

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | η² (partial) |       95% CI
    > ----------------------------------------------
    > treatment        |         0.27 | [0.13, 1.00]
    > gender           |         0.12 | [0.03, 1.00]
    > treatment:gender |         0.20 | [0.07, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

If all of this type-III-effects-coding seems like a hassle, you can use
the `afex` package, which takes care of all of this behind the scenes:

``` r

library(afex)
```

    > Loading required package: lme4

    > Loading required package: Matrix

    > ************
    > Welcome to afex. For support visit: http://afex.singmann.science/

    > - Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()
    > - Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'
    > - 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests
    > - Get and set global package options with: afex_options()
    > - Set sum-to-zero contrasts globally: set_sum_contrasts()
    > - For example analyses see: browseVignettes("afex")
    > ************

    > 
    > Attaching package: 'afex'

    > The following object is masked _by_ '.GlobalEnv':
    > 
    >     obk.long

    > The following object is masked from 'package:lme4':
    > 
    >     lmer

``` r

m_afex <- aov_car(value ~ treatment * gender + Error(id), data = obk.long)
```

    > Contrasts set to contr.sum for the following variables: treatment, gender

``` r

eta_squared(m_afex)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | η² (partial) |       95% CI
    > ----------------------------------------------
    > treatment        |         0.27 | [0.13, 1.00]
    > gender           |         0.12 | [0.03, 1.00]
    > treatment:gender |         0.20 | [0.07, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

### Other Measures of Effect Size

#### Unbiased Effect Sizes

These effect sizes are unbiased estimators of the population’s \eta^2:

- **Omega Squared** (\omega^2)
- **Epsilon Squared** (\epsilon^2), also referred to as *Adjusted Eta
  Squared*.

``` r

omega_squared(m_afex)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | ω² (partial) |       95% CI
    > ----------------------------------------------
    > treatment        |         0.24 | [0.10, 1.00]
    > gender           |         0.10 | [0.02, 1.00]
    > treatment:gender |         0.17 | [0.05, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

epsilon_squared(m_afex)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | ε² (partial) |       95% CI
    > ----------------------------------------------
    > treatment        |         0.25 | [0.11, 1.00]
    > gender           |         0.11 | [0.02, 1.00]
    > treatment:gender |         0.18 | [0.06, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

Both \omega^2 and \epsilon^2 (and their partial counterparts, \omega^2_p
& \epsilon^2_p) are unbiased estimators of the population’s \eta^2 (or
\eta^2_p, respectively), which is especially important is small samples.
Though \omega^2 is the more popular choice (Albers and Lakens 2018),
\epsilon^2 is analogous to adjusted-R^2 (Allen 2017, 382), and has been
found to be less biased (Carroll and Nordholm 1975).

#### Generalized Eta²

*Partial* Eta squared aims at estimating the effect size in a design
where only the term of interest was manipulated, assuming all other
terms are have also manipulated. However, not all predictors are always
manipulated - some can only be observed. For such cases, we can use
*generalized* Eta squared (\eta^2_G), which like \eta^2_p estimating the
effect size in a design where only the term of interest was manipulated,
accounting for the fact that some terms cannot be manipulated (and so
their variance would be present in such a design).

``` r

eta_squared(m_afex, generalized = "gender")
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | η² (generalized) |       95% CI
    > --------------------------------------------------
    > treatment        |             0.21 | [0.08, 1.00]
    > gender           |             0.10 | [0.02, 1.00]
    > treatment:gender |             0.18 | [0.06, 1.00]
    > 
    > - Observed variables: gender
    > - One-sided CIs: upper bound fixed at [1.00].

\eta^2_G is useful in repeated-measures designs, as it can estimate what
a *within-subject* effect size would have been had that predictor been
manipulated *between-subjects* (Olejnik and Algina 2003).

#### Cohen’s *f*

Finally, we have the forgotten child - Cohen’s f. Cohen’s f is a
transformation of \eta^2_p, and is the ratio between the term-*SS* and
the error-*SS*.

\text{Cohen's} f_p = \sqrt{\frac{\eta^2_p}{1-\eta^2_p}} =
\sqrt{\frac{SS\_{effect}}{SS\_{error}}}

It can take on values between zero, when the population means are all
equal, and an indefinitely large number as the means are further and
further apart. It is analogous to Cohen’s d when there are only two
groups.

``` r

cohens_f(m_afex)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter        | Cohen's f (partial) |      95% CI
    > ----------------------------------------------------
    > treatment        |                0.61 | [0.38, Inf]
    > gender           |                0.37 | [0.17, Inf]
    > treatment:gender |                0.50 | [0.28, Inf]
    > 
    > - One-sided CIs: upper bound fixed at [Inf].

### When Sum-of-Squares are Hard to Come By

Until now we’ve discusses effect sizes in fixed-effect linear model and
repeated-measures ANOVA’s - cases where the *SS*s are readily available,
and so the various effect sized presented can easily be estimated. How
ever this is not always the case.

For example, in linear mixed models (LMM/HLM/MLM), the estimation of all
required *SS*s is not straightforward. However, we can still
*approximate* these effect sizes (only their partial versions) based on
the **test-statistic approximation method** (learn more in the [*Effect
Size from Test Statistics*
vignette](https://easystats.github.io/effectsize/articles/from_test_statistics.html)).

``` r

library(lmerTest)
```

    > 
    > Attaching package: 'lmerTest'

    > The following object is masked from 'package:lme4':
    > 
    >     lmer

    > The following object is masked from 'package:stats':
    > 
    >     step

``` r

fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova(fit_lmm) # note the type-3 errors
```

    > Type III Analysis of Variance Table with Satterthwaite's method
    >      Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)    
    > Days  30031   30031     1    17    45.9 3.3e-06 ***
    > ---
    > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r

F_to_eta2(45.8, df = 1, df_error = 17)
```

    > η² (partial) |       95% CI
    > ---------------------------
    > 0.73         | [0.51, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

Or directly with \`eta_squared() and co.:

``` r

eta_squared(fit_lmm)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter | η² (partial) |       95% CI
    > ---------------------------------------
    > Days      |         0.73 | [0.51, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

epsilon_squared(fit_lmm)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter | ε² (partial) |       95% CI
    > ---------------------------------------
    > Days      |         0.71 | [0.48, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

omega_squared(fit_lmm)
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter | ω² (partial) |       95% CI
    > ---------------------------------------
    > Days      |         0.70 | [0.47, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

Another case where *SS*s are not available is when using Bayesian
models…

### For Bayesian Models

An alternative route to obtaining effect sizes of explained variance, is
via the use of the ***posterior predictive distribution*** (*PPD*). The
PPD is the Bayesian expected distribution of possible unobserved values.
Thus, after observing some data, we can estimate not just the expected
mean values (the conditional marginal means), but also the full
*distribution* of data around these values (Gelman et al. 2014, chap.
7).

By sampling from the PPD, we can decompose the sample to the various
*SS*s needed for the computation of explained variance measures. By
repeatedly sampling from the PPD, we can generate a posterior
distribution of explained variance estimates. But note that **these
estimates are conditioned not only on the location-parameters of the
model, but also on the scale-parameters of the model!** So it is vital
to [validate the
PPD](https://mc-stan.org/docs/2_23/stan-users-guide/meta-models-part.html#meta-models.part/)
before using it to estimate explained variance measures.

Let’s fit our model:

``` r

library(rstanarm)
```

    > Loading required package: Rcpp

    > This is rstanarm version 2.32.2

    > - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    > - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    > - For execution on a local, multicore CPU with excess RAM we recommend calling

    >   options(mc.cores = parallel::detectCores())

``` r

m_bayes <- stan_glm(value ~ gender + phase + treatment,
  data = obk.long, family = gaussian(),
  refresh = 0
)
```

We can use
[`eta_squared_posterior()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
to get the posterior distribution of eta^2 or eta^2_p for each effect.
Like an ANOVA table, we must make sure to use the right effects-coding
and *SS*-type:

``` r

pes_posterior <- eta_squared_posterior(m_bayes,
  draws = 500, # how many samples from the PPD?
  partial = TRUE, # partial eta squared
  # type 3 SS
  ss_function = car::Anova, type = 3,
  verbose = FALSE
)

head(pes_posterior)
```

    >   gender  phase treatment
    > 1  0.194 0.1694     0.065
    > 2  0.260 0.0268     0.248
    > 3  0.004 0.0161     0.186
    > 4  0.149 0.1023     0.392
    > 5  0.014 0.1156     0.406
    > 6  0.025 0.0097     0.364

``` r

bayestestR::describe_posterior(pes_posterior,
  rope_range = c(0, 0.1), test = "rope"
)
```

    > Summary of Posterior Distribution
    > 
    > Parameter | Median |       95% CI |         ROPE | % in ROPE
    > ------------------------------------------------------------
    > gender    |   0.07 | [0.00, 0.27] | [0.00, 0.10] |    64.56%
    > phase     |   0.05 | [0.00, 0.20] | [0.00, 0.10] |    83.54%
    > treatment |   0.26 | [0.04, 0.47] | [0.00, 0.10] |     7.17%

Compare to:

``` r

m_ML <- lm(value ~ gender + phase + treatment, data = obk.long)

eta_squared(car::Anova(m_ML, type = 3))
```

    > # Effect Size for ANOVA (Type III)
    > 
    > Parameter | η² (partial) |       95% CI
    > ---------------------------------------
    > gender    |         0.07 | [0.01, 1.00]
    > phase     |         0.01 | [0.00, 1.00]
    > treatment |         0.26 | [0.12, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

## For Ordinal Outcomes

When our outcome is not a numeric variable, the effect sizes described
above cannot be used - measured based on sum-of-squares are ill suited
for such outcomes. Instead, we must use effect sizes for *ordinal*
ANOVAs.

In `R`, there are two functions for running *ordinal* one way ANOVAs:
[`kruskal.test()`](https://rdrr.io/r/stats/kruskal.test.html) for
differences between independent groups, and
[`friedman.test()`](https://rdrr.io/r/stats/friedman.test.html) for
differences between dependent groups.

For the one-way ordinal ANOVA, the Rank-Epsilon-Squared (E^2_R) and
Rank-Eta-Squared (\eta^2_H) are measures of association similar to their
non-rank counterparts: values range between 0 (no relative superiority
between any of the groups) to 1 (complete separation - with no overlap
in ranks between the groups).

``` r

group_data <- list(
  g1 = c(2.9, 3.0, 2.5, 2.6, 3.2), # normal subjects
  g2 = c(3.8, 2.7, 4.0, 2.4), # with obstructive airway disease
  g3 = c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
)

kruskal.test(group_data)
```

    > 
    >   Kruskal-Wallis rank sum test
    > 
    > data:  group_data
    > Kruskal-Wallis chi-squared = 0.8, df = 2, p-value = 0.7

``` r

rank_epsilon_squared(group_data)
```

    > ε²(R) |       95% CI
    > --------------------
    > 0.06  | [0.02, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

rank_eta_squared(group_data)
```

    > η²(H) |       95% CI
    > --------------------
    > 0.13  | [0.08, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

For an ordinal repeated measures one-way ANOVA, Kendall’s *W* is a
measure of agreement on the effect of condition between various “blocks”
(the subjects), or more often conceptualized as a measure of reliability
of the rating / scores of observations (or “groups”) between “raters”
(“blocks”).

``` r

# Subjects are COLUMNS
(ReactionTimes <- matrix(
  c(
    398, 338, 520,
    325, 388, 555,
    393, 363, 561,
    367, 433, 470,
    286, 492, 536,
    362, 475, 496,
    253, 334, 610
  ),
  nrow = 7, byrow = TRUE,
  dimnames = list(
    paste0("Subject", 1:7),
    c("Congruent", "Neutral", "Incongruent")
  )
))
```

    >          Congruent Neutral Incongruent
    > Subject1       398     338         520
    > Subject2       325     388         555
    > Subject3       393     363         561
    > Subject4       367     433         470
    > Subject5       286     492         536
    > Subject6       362     475         496
    > Subject7       253     334         610

``` r

friedman.test(ReactionTimes)
```

    > 
    >   Friedman rank sum test
    > 
    > data:  ReactionTimes
    > Friedman chi-squared = 11, df = 2, p-value = 0.004

``` r

kendalls_w(ReactionTimes)
```

    > Kendall's W |       95% CI
    > --------------------------
    > 0.80        | [0.76, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

## References

Albers, Casper, and Daniël Lakens. 2018. “When Power Analyses Based on
Pilot Data Are Biased: Inaccurate Effect Size Estimators and Follow-up
Bias.” *Journal of Experimental Social Psychology* 74: 187–95.

Allen, Rory. 2017. *Statistics and Experimental Design for
Psychologists: A Model Comparison Approach*. World Scientific Publishing
Company.

Carroll, Robert M, and Lena A Nordholm. 1975. “Sampling Characteristics
of Kelley’s Epsilon and Hays’ Omega.” *Educational and Psychological
Measurement* 35 (3): 541–54.

Gelman, Andrew, John B Carlin, Hal S Stern, and Donald B Rubin. 2014.
“Bayesian Data Analysis (Vol. 2).” *Boca Raton, FL: Chapman*.

Olejnik, Stephen, and James Algina. 2003. “Generalized Eta and Omega
Squared Statistics: Measures of Effect Size for Some Common Research
Designs.” *Psychological Methods* 8 (4): 434.
