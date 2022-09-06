
# effectsize: Indices of Effect Size and Standardized Parameters <img src="man/figures/logo.png" align="right" width="120" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.02815/status.svg/)](https://doi.org/10.21105/joss.02815)
[![downloads](https://cranlogs.r-pkg.org/badges/effectsize)](https://cran.r-project.org/package=effectsize/)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/effectsize)](https://cran.r-project.org/package=effectsize/)
[![status](https://tinyverse.netlify.com/badge/effectsize/)](https://CRAN.R-project.org/package=effectsize/)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

***Significant is just not enough!***

The goal of this package is to provide utilities to work with indices of
effect size and standardized parameters, allowing computation and
conversion of indices such as Cohen’s *d*, *r*, odds-ratios, etc.

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/effectsize)](https://cran.r-project.org/package=effectsize/)
[![effectsize status
badge](https://easystats.r-universe.dev/badges/effectsize/)](https://easystats.r-universe.dev/)
[![R-CMD-check](https://github.com/easystats/effectsize/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/easystats/effectsize/actions)
[![pkgdown](https://github.com/easystats/effectsize/workflows/pkgdown/badge.svg/)](https://github.com/easystats/effectsize/actions/)
[![Codecov test
coverage](https://codecov.io/gh/easystats/effectsize/branch/main/graph/badge.svg/)](https://app.codecov.io/gh/easystats/effectsize?branch=main/)

Run the following to install the stable release of **effectsize** from
CRAN:

``` r
install.packages("effectsize")
```

Or you can install the latest development version `0.7.0.9999` from
[*R-universe*](https://easystats.r-universe.dev):

``` r
install.packages("effectsize", repos = "https://easystats.r-universe.dev/")
```

<!-- Or from *GitHub*: -->
<!-- ```{r, warning=FALSE, message=FALSE, eval=FALSE} -->
<!-- if (!require("remotes")) install.packages("remotes") -->
<!-- remotes::install_github("easystats/effectsize") -->
<!-- ``` -->

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-effectsize-orange.svg?colorB=E91E63/)](https://easystats.github.io/effectsize/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800/)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-effectsize-orange.svg?colorB=2196F3/)](https://easystats.github.io/effectsize/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/effectsize/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

- **Effect Sizes**
  - [**For Simple Hypothesis
    Tests**](https://easystats.github.io/effectsize/articles/simple_htests.html)  
  - [**ANOVA Effect
    Sizes**](https://easystats.github.io/effectsize/articles/anovaES.html)
- **Effect Sizes Conversion**
  - [**Between Effect
    Sizes**](https://easystats.github.io/effectsize/articles/convert.html)
  - [**Effect Size from Test
    Statistics**](https://easystats.github.io/effectsize/articles/from_test_statistics.html)
- [**Automated Interpretation of Indices of Effect
  Size**](https://easystats.github.io/effectsize/articles/interpret.html)

# Features

This package is focused on indices of effect size. Check out the package
website for [**a full list of features and functions** provided by
`effectsize`](https://easystats.github.io/effectsize/reference/index.html).

``` r
library(effectsize)
```

## Effect Size Computation

### Standardized Differences (Cohen’s *d*, Hedges’ *g*, Glass’ *delta*)

The package provides functions to compute indices of effect size.

``` r
cohens_d(mpg ~ am, data = mtcars)
## Cohen's d |         95% CI
## --------------------------
## -1.48     | [-2.27, -0.67]
## 
## - Estimated using pooled SD.

hedges_g(mpg ~ am, data = mtcars)
## Hedges' g |         95% CI
## --------------------------
## -1.44     | [-2.21, -0.65]
## 
## - Estimated using pooled SD.

glass_delta(mpg ~ am, data = mtcars)
## Glass' delta |         95% CI
## -----------------------------
## -1.17        | [-1.93, -0.39]
```

`effectsize` also provides effect sizes for *contingency tables*, *rank
tests*, and more…

### ANOVAs (Eta<sup>2</sup>, Omega<sup>2</sup>, …)

``` r
model <- aov(mpg ~ factor(gear), data = mtcars)

eta_squared(model)
## # Effect Size for ANOVA
## 
## Parameter    | Eta2 |       95% CI
## ----------------------------------
## factor(gear) | 0.43 | [0.18, 1.00]
## 
## - One-sided CIs: upper bound fixed at [1.00].

omega_squared(model)
## # Effect Size for ANOVA
## 
## Parameter    | Omega2 |       95% CI
## ------------------------------------
## factor(gear) |   0.38 | [0.14, 1.00]
## 
## - One-sided CIs: upper bound fixed at [1.00].

epsilon_squared(model)
## # Effect Size for ANOVA
## 
## Parameter    | Epsilon2 |       95% CI
## --------------------------------------
## factor(gear) |     0.39 | [0.14, 1.00]
## 
## - One-sided CIs: upper bound fixed at [1.00].
```

And more…

<!-- ### Regression Models (Standardized Parameters) -->
<!-- Importantly, `effectsize` also provides [advanced methods](https://easystats.github.io/effectsize/articles/standardize_parameters.html) to compute standardized parameters for regression models. -->
<!-- ```{r beta, warning=FALSE, message=FALSE} -->
<!-- m <- lm(rating ~ complaints + privileges + advance, data = attitude) -->
<!-- standardize_parameters(m) -->
<!-- ``` -->
<!-- Also, models can be re-fit with standardized data: -->
<!-- ```{r std-model, warning=FALSE, message=FALSE} -->
<!-- standardize(m) -->
<!-- ``` -->

## Effect Size Conversion

The package also provides ways of converting between different effect
sizes.

``` r
d_to_r(d = 0.2)
## [1] 0.0995

oddsratio_to_riskratio(2.6, p0 = 0.4)
## [1] 1.59
```

And for recovering effect sizes from test statistics.

``` r
F_to_d(15, df = 1, df_error = 60)
## d    |       95% CI
## -------------------
## 1.00 | [0.46, 1.53]

F_to_r(15, df = 1, df_error = 60)
## r    |       95% CI
## -------------------
## 0.45 | [0.22, 0.61]

F_to_eta2(15, df = 1, df_error = 60)
## Eta2 (partial) |       95% CI
## -----------------------------
## 0.20           | [0.07, 1.00]
## 
## - One-sided CIs: upper bound fixed at [1.00].
```

## Effect Size Interpretation

The package allows for an automated interpretation of different indices.

``` r
interpret_r(r = 0.3)
## [1] "large"
## (Rules: funder2019)
```

Different sets of “rules of thumb” are implemented ([**guidelines are
detailed
here**](https://easystats.github.io/effectsize/articles/interpret.html))
and can be easily changed.

``` r
interpret_cohens_d(d = 0.45, rules = "cohen1988")
## [1] "small"
## (Rules: cohen1988)

interpret_cohens_d(d = 0.45, rules = "gignac2016")
## [1] "moderate"
## (Rules: gignac2016)
```

### Citation

In order to cite this package, please use the following citation:

- Ben-Shachar M, Lüdecke D, Makowski D (2020). effectsize: Estimation of
  Effect Size Indices and Standardized Parameters. *Journal of Open
  Source Software*, *5*(56), 2815. doi: 10.21105/joss.02815

Corresponding BibTeX entry:

    @Article{,
      title = {{e}ffectsize: Estimation of Effect Size Indices and Standardized Parameters},
      author = {Mattan S. Ben-Shachar and Daniel Lüdecke and Dominique Makowski},
      year = {2020},
      journal = {Journal of Open Source Software},
      volume = {5},
      number = {56},
      pages = {2815},
      publisher = {The Open Journal},
      doi = {10.21105/joss.02815},
      url = {https://doi.org/10.21105/joss.02815}
    }

# Contributing and Support

If you have any questions regarding the the functionality of the
package, you may either contact us via email or also [file an
issue](https://github.com/easystats/effectsize/issues/). Anyone wishing
to contribute to the package by adding functions, features, or in
another way, please follow [this
guide](https://github.com/easystats/effectsize/blob/main/.github/CONTRIBUTING.md/)
and our [code of
conduct](https://github.com/easystats/effectsize/blob/main/.github/CODE_OF_CONDUCT.md/).
