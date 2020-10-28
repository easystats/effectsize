---
title: "Size does matter: Estimation and Interpretation of Effect Size Indices and Standardized Parameters"
authors:
- affiliation: 1
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 2
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
- affiliation: 3
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967

date: "28 October, 2020"
output: 
  pdf_document:
    latex_engine: xelatex
bibliography: paper.bib
csl: apa.csl
tags:
- R
- easystats
- effect size
- regression
- linear models
- standardized coefficients
affiliations:
- index: 1
  name: Ben-Gurion University of the Negev, Israel
- index: 2
  name:  University Medical Center Hamburg-Eppendorf, Germany
- index: 3
  name: Nanyang Technological University, Singapore
---

# Aims of the Package

<!-- not sure how much of this is needed. Edit (add, remove) as you see fit... -->

It is often of interest to to asses the size of an observed association. This is typically done to allow the judgment of the magnitude of an effect (especially when units of measurement are not meaningful), to facilitate comparing predictors' importance within a given model, or both. Though some indecies of effect size, such a the correlation coefficient (a standardized covariance coefficient) are readily available, other measures are often harder to obtain. 

**effectsize** is an R-package [@rcore] that fills this important gap. Its primary goal is to provide utilities for estimating a wide variety of standardized effect sizes (i.e., effect sizes that are not tied to the units of measurement of the variables of interest, i.e., they are scale-invariant) and their confidence intervals (CI), from a variety of statistical models. **effectsize** provided easy to use functions, with full documentation and explanation of the various effect sizes offered, and is also used by developers of other R packages as the back-end for effect size computation, such as *parameters* [@ludecke2020extracting] and *ggstatsplot* [@patil2020ggstatsplot], *gtsummary* [@sjoberg2020gtsummary] and more.

# Examples of Features

**effectsize** provides various functions for extracting and estimating effect sizes and their confidence intervals [estimated using the noncentrality parameter method; @steiger2004beyond]. In the following, we show some brief examples. However, a comprehensive overview including in-depth examples are accessible via the dedicated website (https://easystats.github.io/effectsize/).

## Indices of Effect Size

### Standardized Differences

**effectsize** provides functions for estimating the common indices of standardized differences such as Cohen's *d* (`cohens_d()`), Hedge's *g* (`hedges_g()`) for both paired and independent samples, and Glass' $\Delta$ (`glass_delta()`).

``` r
library(effectsize)

cohens_d(mpg ~ am, data = mtcars)
#> Cohen's d |         95% CI
#> --------------------------
#>     -1.48 | [-2.27, -0.67]
```

### Contingency Tables

Person's $\phi$ (`phi()`) and Cramér's *V* (`cramers_v()`) can be used to estimate the strength of association between two categorical variables, while Cohen's *g* (`cohens_g()`) estimates the deviance between paired categorical variables.

``` r
M <- rbind(c(150, 130, 35, 55),
           c(100, 50,  10, 40),
           c(165, 65,  2,  25))

cramers_v(M)
#> Cramer's V |       95% CI
#> -------------------------
#>       0.18 | [0.12, 0.22]
```

## Parameter and Model Standardization

Standardizing parameters (i.e., coefficients) can allow for their comparison within and between models, variables and studies. To this end, two functions are available: `standardize()` which returns an updated model, re-fits with standardized data, and `standardize_parameters()` which returns a table of standardized coefficients from a provided model.

``` r
model <- lm(mpg ~ cyl * am, 
            data = mtcars)

standardize_parameters(model)
#> Parameter   | Coefficient (std.) |         95% CI
#> -------------------------------------------------
#> (Intercept) |              -0.10 | [-0.30,  0.11]
#> cyl         |              -0.74 | [-0.95, -0.53]
#> am          |               0.17 | [-0.04,  0.39]
#> cyl:am      |              -0.19 | [-0.41,  0.02]
#> 
#> # Standardization method: Refit
```

Standardize parameters can also be produced for generalized linear models (GLMs; where only the predictors are standardized):

<!-- Here I used `exponentiate`, but should maybe not? -->

``` r
model <- glm(am ~ cyl + hp,
             family = "binomial",
             data = mtcars, )

standardize_parameters(model, exponentiate = TRUE)
#> Parameter   | Odds Ratio (std.) |        95% CI
#> -----------------------------------------------
#> (Intercept) |              0.53 | [0.18,  1.32]
#> cyl         |              0.05 | [0.00,  0.29]
#> hp          |              6.70 | [1.32, 61.54]
#> 
#> # Standardization method: Refit
```

`standardize_parameters()` provides several standardization methods, such as robust standardization, or *pseudo*-standardized coefficients for (generalized) linear mixed models [@hoffman2015longitudinal]. A full review of the methods can be found in the [*Parameter and Model Standardization* vignette](https://easystats.github.io/effectsize/articles/standardize_parameters.html).

## ANOVA Table Effect Sizes

In the context of ANOVA-like tests, or ANOVA tables, it is common to report ANOVA-like effect sizes. Unlike standardized parameters, these effect sizes represent the amount of variance explained by each of the model's terms, where each term can be represented by 1 or more parameters. `eta_squared()` can produce such popular effect sizes as Eta-squared ($\eta^2$), its partial version ($\eta^2_p$), as well as the generalized $\eta^2_G$ [@olejnik2003generalized]:


``` r
data("ChickWeight")

options(contrasts = c('contr.sum', 'contr.poly'))

ChickWeight$Time <- factor(ChickWeight$Time)

model <- aov(weight ~ Diet * Time + Error(Chick / Time),
             data = ChickWeight)

eta_squared(model, partial = TRUE)
#> Group      | Parameter | Eta2 (partial) |       90% CI
#> ------------------------------------------------------
#> Chick      |      Diet |           0.27 | [0.06, 0.42]
#> Chick:Time |      Time |           0.87 | [0.85, 0.88]
#> Chick:Time | Diet:Time |           0.22 | [0.11, 0.23]

eta_squared(model, generalized = "Time")
#> Group      | Parameter | Eta2 (generalized) |       90% CI
#> ----------------------------------------------------------
#> Chick      |      Diet |               0.04 | [0.00, 0.09]
#> Chick:Time |      Time |               0.74 | [0.71, 0.77]
#> Chick:Time | Diet:Time |               0.03 | [0.00, 0.00]
```

**effectsize** also offers the unbiased estimates of $\epsilon^2_p$ (`epsilon_squared()`) and $\omega^2_p$ (`omega_squared()`). For more detail about the various effect size measures and their applications, see the  [*Effect sizes for ANOVAs* vignette](https://easystats.github.io/effectsize/articles/anovaES.html).

## Effect Size Conversion

### From Test Statistics

In many real world applications there are no straightforward ways of obtaining standardized effect sizes. However, it is possible to get approximations of most of the effect size indices (*d*, *r*, $\eta^2_p$...) with the use of test statistics. These conversions are based on the idea that test statistics are a function of effect size and sample size (or more often of degrees of freedom). Thus it is possible to reverse-engineer indices of effect size from test statistics (*F*, *t*, $\chi^2$, and *z*). 

``` r
F_to_eta2(f = c(40.72, 33.77),
          df = c(2, 1), df_error = c(18, 9))
#> Eta2 (partial) |       90% CI
#> -----------------------------
#>           0.82 | [0.66, 0.89]
#>           0.79 | [0.49, 0.89]

t_to_d(t = -5.14, df_error = 22)
#>     d |         95% CI
#> ----------------------
#> -2.19 | [-3.23, -1.12]

t_to_r(t = -5.14, df_error = 22)
#>     r |         95% CI
#> ----------------------
#> -0.74 | [-0.85, -0.49]
```

These functions also power the `effectsize()` convenience function for estimating effect sizes from R's `htest`-type objects. For example:

``` r
ts <- t.test(x = 1:10, y = 7:20)
effectsize(ts)
#>     d |         95% CI
#> ----------------------
#> -2.32 | [-3.38, -1.22]

xtab <- rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477))
Xsq <- chisq.test(xtab)
effectsize(Xsq)
#> Cramer's V |       95% CI
#> -------------------------
#>       0.07 | [0.05, 0.09]
```

These functions also power our *Effect Sizes From Test Statistics* shiny app (https://easystats4u.shinyapps.io/statistic2effectsize/).

### Between Effect Sizes

For comparisons between different types of designs and analyses, it is useful to be able to convert between different types of effect sizes [*d*, *r*, Odds ratios and Risk ratios; @borenstein2009converting; grant2014converting].

``` r
r_to_d(0.7)
#> [1] 1.960392

d_to_oddsratio(1.96)
#> [1] 34.98946

oddsratio_to_r(34.99)
#> [1] 0.6999301

oddsratio_to_riskratio(1.96, p0 = 0.15)
#> [1] 1.713287
```

## Effect Size Interpretation

Finally, **effectsize** provides convenience functions to apply existing or custom interpretation rules of thumb, such as for instance Cohen's (1988). Although we strongly advocate for the cautious and parsimonious use of such judgment-replacing tools, we provide these functions to allow users and developers to explore and hopefully gain a deeper understanding of the relationship between data values and their interpretation. More information is available in the package's [**documentation**](https://easystats.github.io/effectsize/articles/interpret.html).

``` r
interpret_d(c(0.02, 0.52, 0.86), rules = "cohen1988")
#> [1] "very small" "medium"     "large"     
#> (Rules: cohen1988)
```


# Licensing and Availability

**effectsize** is licensed under the GNU General Public License (v3.0), with all source code stored at GitHub (https://github.com/easystats/effectsize), and with a corresponding issue tracker for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests/tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

**effectsize** is part of the [*easystats*](https://github.com/easystats/easystats) ecosystem, a collaborative project created to facilitate the usage of R for statistical analyses. Thus, we would like to thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

# References
