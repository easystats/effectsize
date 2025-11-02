# Effect Sizes: Getting Started

## Aims of the Package

In both theoretical and applied research, it is often of interest to
assess the strength of an observed association. This is typically done
to allow the judgment of the magnitude of an effect \[especially when
units of measurement are not meaningful, e.g., in the use of estimated
latent variables; Bollen (1989)\], to facilitate comparing between
predictors’ importance within a given model, or both. Though some
indices of effect size, such as the correlation coefficient (itself a
standardized covariance coefficient) are readily available, other
measures are often harder to obtain. **effectsize** is an R package (R
Core Team 2020) that fills this important gap, providing utilities for
easily estimating a wide variety of standardized effect sizes (i.e.,
effect sizes that are not tied to the units of measurement of the
variables of interest) and their confidence intervals (CIs), from a
variety of statistical models. **effectsize** provides easy-to-use
functions, with full documentation and explanation of the various effect
sizes offered, and is also used by developers of other R packages as the
back-end for effect size computation, such as **parameters** (Lüdecke et
al. 2020), **ggstatsplot** (Patil 2018), **gtsummary** (Sjoberg et al.
2020) and more.

## Comparison to Other Packages

**effectsize**’s functionality is in part comparable to packages like
**lm.beta** (Behrendt 2014), **MOTE** (Buchanan et al. 2019), and
**MBESS** (Kelley 2020). Yet, there are some notable differences, e.g.:

- Both **MOTE** and **MBESS** provide functions for computing effect
  sizes such as Cohen’s *d* and effect sizes for ANOVAs (Cohen 1988),
  and their confidence intervals. However, both require manual input of
  *F*- or *t*-statistics, *degrees of freedom*, and *sums of squares*
  for the computation the effect sizes, whereas **effectsize** can
  automatically extract this information from the provided models, thus
  allowing for better ease-of-use as well as reducing any potential for
  error.  

## Examples of Features

**effectsize** provides various functions for extracting and estimating
effect sizes and their confidence intervals \[estimated using the
noncentrality parameter method; Steiger (2004)\]. In this article, we
provide basic usage examples for estimating some of the most common
effect size. A comprehensive overview, including in-depth examples and
[a full list of features and
functions](https://easystats.github.io/effectsize/reference/index.html),
are accessible via a dedicated website
(<https://easystats.github.io/effectsize/>).

### Indices of Effect Size

#### Standardized Differences

**effectsize** provides functions for estimating the common indices of
standardized differences such as Cohen’s *d*
([`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)),
Hedges’ *g*
([`hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.md))
for both paired and independent samples (Cohen 1988; Hedges and Olkin
1985), and Glass’ \Delta
([`glass_delta()`](https://easystats.github.io/effectsize/reference/cohens_d.md))
for independent samples with different variances (Hedges and Olkin
1985).

``` r

library(effectsize)
options(es.use_symbols = TRUE) # get nice symbols when printing! (On Windows, requires R >= 4.2.0)

cohens_d(mpg ~ am, data = mtcars)
#> Cohen's d |         95% CI
#> --------------------------
#> -1.48     | [-2.27, -0.67]
#> 
#> - Estimated using pooled SD.
```

#### Contingency Tables

Pearson’s \phi
([`phi()`](https://easystats.github.io/effectsize/reference/phi.md)) and
Cramér’s *V*
([`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md))
can be used to estimate the strength of association between two
categorical variables (Cramér 1946), while Cohen’s *g*
([`cohens_g()`](https://easystats.github.io/effectsize/reference/cohens_g.md))
estimates the deviance between paired categorical variables (Cohen
1988).

``` r

M <- rbind(
  c(150, 130, 35, 55),
  c(100, 50, 10, 40),
  c(165, 65, 2, 25)
)

cramers_v(M)
#> Cramer's V (adj.) |       95% CI
#> --------------------------------
#> 0.17              | [0.11, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
```

### Parameter and Model Standardization

> Note: this functionality has been moved to the `parameters` and
> `datawizard` packages.

Standardizing parameters (i.e., coefficients) can allow for their
comparison within and between models, variables and studies. To this
end, two functions are available:
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.html),
which returns an updated model, re-fit with standardized data, and
[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html),
which returns a table of standardized coefficients from a provided model
\[for a list of supported models, see the *insight* package; Lüdecke et
al. (2019)\].

``` r

model <- lm(mpg ~ cyl * am,
  data = mtcars
)

datawizard::standardize(model)
#> 
#> Call:
#> lm(formula = mpg ~ cyl * am, data = data_std)
#> 
#> Coefficients:
#> (Intercept)          cyl           am       cyl:am  
#>     -0.0977      -0.7426       0.1739      -0.1930

parameters::standardize_parameters(model)
#> # Standardization method: refit
#> 
#> Parameter   | Std. Coef. |         95% CI
#> -----------------------------------------
#> (Intercept) |      -0.10 | [-0.30,  0.11]
#> cyl         |      -0.74 | [-0.95, -0.53]
#> am          |       0.17 | [-0.04,  0.39]
#> cyl × am    |      -0.19 | [-0.41,  0.02]
```

Standardized parameters can also be produced for generalized linear
models (GLMs; where only the predictors are standardized):

``` r

model <- glm(am ~ cyl + hp,
  family = "binomial",
  data = mtcars
)

parameters::standardize_parameters(model, exponentiate = TRUE)
#> # Standardization method: refit
#> 
#> Parameter   | Std_Odds_Ratio |        95% CI
#> --------------------------------------------
#> (Intercept) |           0.53 | [0.18,  1.32]
#> cyl         |           0.05 | [0.00,  0.29]
#> hp          |           6.70 | [1.32, 61.54]
#> 
#> - Response is unstandardized.
```

[`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
provides several standardization methods, such as robust
standardization, or *pseudo*-standardized coefficients for (generalized)
linear mixed models (Hoffman 2015). A full review of these methods can
be found in the [*Parameter and Model Standardization*
vignette](https://easystats.github.io/parameters/articles/standardize_parameters_effsize.html).

### Effect Sizes for ANOVAs

Unlike standardized parameters, the effect sizes reported in the context
of ANOVAs (analysis of variance) or ANOVA-like tables represent the
amount of variance explained by each of the model’s terms, where each
term can be represented by one or more parameters.
[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
can produce such popular effect sizes as Eta-squared (\eta^2), its
partial version (\eta^2_p), as well as the generalized \eta^2_G (Cohen
1988; Olejnik and Algina 2003):

``` r

options(contrasts = c("contr.sum", "contr.poly"))

data("ChickWeight")
# keep only complete cases and convert `Time` to a factor
ChickWeight <- subset(ChickWeight, ave(weight, Chick, FUN = length) == 12)
ChickWeight$Time <- factor(ChickWeight$Time)

model <- aov(weight ~ Diet * Time + Error(Chick / Time),
  data = ChickWeight
)

eta_squared(model, partial = TRUE)
#> # Effect Size for ANOVA (Type I)
#> 
#> Group      | Parameter | η² (partial) |       95% CI
#> ----------------------------------------------------
#> Chick      |      Diet |         0.27 | [0.06, 1.00]
#> Chick:Time |      Time |         0.87 | [0.85, 1.00]
#> Chick:Time | Diet:Time |         0.22 | [0.11, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

eta_squared(model, generalized = "Time")
#> # Effect Size for ANOVA (Type I)
#> 
#> Group      | Parameter | η² (generalized) |       95% CI
#> --------------------------------------------------------
#> Chick      |      Diet |             0.04 | [0.00, 1.00]
#> Chick:Time |      Time |             0.74 | [0.71, 1.00]
#> Chick:Time | Diet:Time |             0.03 | [0.00, 1.00]
#> 
#> - Observed variables: Time
#> - One-sided CIs: upper bound fixed at [1.00].
```

**effectsize** also offers \epsilon^2_p
([`epsilon_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md))
and \omega^2_p
([`omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)),
which are less biased estimates of the variance explained in the
population (Kelley 1935; Olejnik and Algina 2003). For more details
about the various effect size measures and their applications, see the
[*Effect sizes for ANOVAs*
vignette](https://easystats.github.io/effectsize/articles/anovaES.html).

### Effect Size Conversion

#### From Test Statistics

In many real world applications there are no straightforward ways of
obtaining standardized effect sizes. However, it is possible to get
approximations of most of the effect size indices (*d*, *r*, \eta^2_p…)
with the use of test statistics (Friedman 1982). These conversions are
based on the idea that test statistics are a function of effect size and
sample size (or more often of degrees of freedom). Thus it is possible
to reverse-engineer indices of effect size from test statistics (*F*,
*t*, \chi^2, and *z*).

``` r

F_to_eta2(
  f = c(40.72, 33.77),
  df = c(2, 1), df_error = c(18, 9)
)
#> η² (partial) |       95% CI
#> ---------------------------
#> 0.82         | [0.66, 1.00]
#> 0.79         | [0.49, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

t_to_d(t = -5.14, df_error = 22)
#> d     |         95% CI
#> ----------------------
#> -2.19 | [-3.23, -1.12]

t_to_r(t = -5.14, df_error = 22)
#> r     |         95% CI
#> ----------------------
#> -0.74 | [-0.85, -0.49]
```

These functions also power the
[`effectsize()`](https://easystats.github.io/effectsize/reference/effectsize.md)
convenience function for estimating effect sizes from R’s `htest`-type
objects. For example:

``` r

data(hardlyworking, package = "effectsize")

aov1 <- oneway.test(salary ~ n_comps,
  data = hardlyworking, var.equal = TRUE
)
effectsize(aov1)
#> η²   |       95% CI
#> -------------------
#> 0.20 | [0.14, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

xtab <- rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477))
Xsq <- chisq.test(xtab)
effectsize(Xsq)
#> Cramer's V (adj.) |       95% CI
#> --------------------------------
#> 0.07              | [0.05, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
```

These functions also power our *Effect Sizes From Test Statistics* shiny
app (<https://easystats4u.shinyapps.io/statistic2effectsize/>).

#### Between Effect Sizes

For comparisons between different types of designs and analyses, it is
useful to be able to convert between different types of effect sizes
\[*d*, *r*, Odds ratios and Risk ratios; Borenstein et al. (2009); Grant
(2014)\].

``` r

r_to_d(0.7)
#> [1] 1.960392

d_to_oddsratio(1.96)
#> [1] 34.98946

oddsratio_to_riskratio(34.99, p0 = 0.4)
#> [1] 2.397232

oddsratio_to_r(34.99)
#> [1] 0.6999301
```

### Effect Size Interpretation

Finally, **effectsize** provides convenience functions to apply existing
or custom interpretation rules of thumb, such as for instance Cohen’s
(1988). Although we strongly advocate for the cautious and parsimonious
use of such judgment-replacing tools, we provide these functions to
allow users and developers to explore and hopefully gain a deeper
understanding of the relationship between data values and their
interpretation. More information is available in the [*Automated
Interpretation of Indices of Effect Size*
vignette](https://easystats.github.io/effectsize/articles/interpret.html).

``` r

interpret_cohens_d(c(0.02, 0.52, 0.86), rules = "cohen1988")
#> [1] "very small" "medium"     "large"     
#> (Rules: cohen1988)
```

## Licensing and Availability

**effectsize** is licensed under the GNU General Public License (v3.0),
with all source code stored at GitHub
(<https://github.com/easystats/effectsize>), and with a corresponding
issue tracker for bug reporting and feature enhancements. In the spirit
of honest and open science, we encourage requests/tips for fixes,
feature updates, as well as general questions and concerns via direct
interaction with contributors and developers, by [filing an
issue](https://github.com/easystats/effectsize/issues/). See the
package’s [*Contribution
Guidelines*](https://github.com/easystats/effectsize/blob/main/.github/CONTRIBUTING.md/).

## Acknowledgments

**effectsize** is part of the
[*easystats*](https://github.com/easystats/easystats/) ecosystem, a
collaborative project created to facilitate the usage of R for
statistical analyses. Thus, we would like to thank the [members of
easystats](https://github.com/orgs/easystats/people/) as well as the
users.

## References

Behrendt, Stefan. 2014. *lm.beta: Add Standardized Regression
Coefficients to Lm-Objects*.
<https://CRAN.R-project.org/package=lm.beta/>.

Bollen, Kenneth A. 1989. *Structural Equations with Latent Variables*.
John Wiley & Sons. <https://doi.org/10.1002/9781118619179>.

Borenstein, Michael, Larry V Hedges, JPT Higgins, and Hannah R
Rothstein. 2009. “Converting Among Effect Sizes.” *Introduction to
Meta-Analysis*, 45–49.

Buchanan, Erin M., Amber Gillenwaters, John E. Scofield, and K. D.
Valentine. 2019. *MOTE: Measure of the Effect: Package to Assist in
Effect Size Calculations and Their Confidence Intervals*.
<https://github.com/doomlab/MOTE/>.

Cohen, J. 1988. *Statistical Power Analysis for the Behavioral Sciences,
2nd Ed.* New York: Routledge.

Cramér, Harald. 1946. *Mathematical Methods of Statistics*. Princeton
University Press.

Friedman, Herbert. 1982. “Simplified Determinations of Statistical
Power, Magnitude of Effect and Research Sample Sizes.” *Educational and
Psychological Measurement* 42 (2): 521–26.

Grant, Robert L. 2014. “Converting an Odds Ratio to a Range of Plausible
Relative Risks for Better Communication of Research Findings.” *Bmj*
348: f7450.

Hedges, L, and I Olkin. 1985. *Statistical Methods for Meta-Analysis*.
Academic Press.

Hoffman, Lesa. 2015. *Longitudinal Analysis: Modeling Within-Person
Fluctuation and Change*. Routledge.

Kelley, Ken. 2020. *MBESS: The MBESS R Package*.
<https://CRAN.R-project.org/package=MBESS/>.

Kelley, Truman L. 1935. “An Unbiased Correlation Ratio Measure.”
*Proceedings of the National Academy of Sciences of the United States of
America* 21 (9): 554–59. <https://doi.org/10.1073/pnas.21.9.554>.

Lüdecke, Daniel, Mattan S Ben-Shachar, Indrajeet Patil, and Dominique
Makowski. 2020. “Extracting, Computing and Exploring the Parameters of
Statistical Models Using R.” *Journal of Open Source Software* 5 (53):
2445. <https://doi.org/10.21105/joss.02445>.

Lüdecke, Daniel, Philip Waggoner, and Dominique Makowski. 2019.
“insight: A Unified Interface to Access Information from Model Objects
in R.” *Journal of Open Source Software* 4 (38): 1412.
<https://doi.org/10.21105/joss.01412>.

Olejnik, Stephen, and James Algina. 2003. “Generalized Eta and Omega
Squared Statistics: Measures of Effect Size for Some Common Research
Designs.” *Psychological Methods* 8 (4): 434.

Patil, Indrajeet. 2018. “ggstatsplot: ’Ggplot2’ Based Plots with
Statistical Details.” *CRAN*, ahead of print.
<https://doi.org/10.5281/zenodo.2074621>.

R Core Team. 2020. *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Sjoberg, Daniel D., Michael Curry, Margie Hannum, Karissa Whiting, and
Emily C. Zabor. 2020. *gtsummary: Presentation-Ready Data Summary and
Analytic Result Tables*.
<https://CRAN.R-project.org/package=gtsummary/>.

Steiger, James H. 2004. “Beyond the F Test: Effect Size Confidence
Intervals and Tests of Close Fit in the Analysis of Variance and
Contrast Analysis.” *Psychological Methods* 9 (2): 164–82.
<https://doi.org/10.1037/1082-989X.9.2.164>.
