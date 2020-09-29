
# effectsize <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/effectsize)](https://cran.r-project.org/package=effectsize)
[![downloads](http://cranlogs.r-pkg.org/badges/effectsize)](https://cran.r-project.org/package=effectsize)
[![R-check](https://github.com/easystats/effectsize/workflows/R-check/badge.svg)](https://github.com/easystats/effectsize/actions)
[![pkgdown](https://github.com/easystats/effectsize/workflows/pkgdown/badge.svg)](https://github.com/easystats/effectsize/actions)
<!-- [![Build Status](https://travis-ci.org/easystats/effectsize.svg?branch=master)](https://travis-ci.org/easystats/effectsize) -->
<!-- [![codecov](https://codecov.io/gh/easystats/effectsize/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/effectsize) -->

***Size does matter***

The goal of this package is to provide utilities to work with indices of
effect size and standardized parameters, allowing computation and
conversion of indices such as Cohen’s *d*, *r*, odds-ratios, etc.

## Installation

Run the following to install the latest GitHub-version of `effectsize`:

``` r
install.packages("devtools")
devtools::install_github("easystats/effectsize")
```

Or install the latest stable release from CRAN:

``` r
install.packages("effectsize")
```

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-effectsize-orange.svg?colorB=E91E63)](https://easystats.github.io/effectsize/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-effectsize-orange.svg?colorB=2196F3)](https://easystats.github.io/effectsize/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/effectsize/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - [**Data
    Standardization**](https://easystats.github.io/effectsize/articles/standardize_data.html)
  - [**Parameter and Model
    Standardization**](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
  - [**ANOVA Effect
    Sizes**](https://easystats.github.io/effectsize/articles/anovaES.html)
  - [**Effect Sizes in Bayesian
    Models**](https://easystats.github.io/effectsize/articles/bayesian_models.html)
  - [**Effect Size
    Conversion**](https://easystats.github.io/effectsize/articles/convert.html)
  - [**Effect Size from Test
    Statistics**](https://easystats.github.io/effectsize/articles/from_test_statistics.html)
  - [**Automated Interpretation of Indices of Effect
    Size**](https://easystats.github.io/effectsize/articles/interpret.html)

# Features

This package is focused on indices of effect size. But **there are
hundreds of them\! Thus, *everybody* is welcome to contribute** by
adding support for the interpretation of new indices. If you’re not sure
how to code it’s okay, just open an issue to discuss it and we’ll help
:)

``` r
library(effectsize)
```

## Effect Size Computation

### Basic Indices (Cohen’s *d*, Hedges’ *g*, Glass’ *delta*)

The package provides functions to compute indices of effect size.

``` r
cohens_d(iris$Sepal.Length, iris$Sepal.Width)
## Cohen's d |       95% CI
## ------------------------
##      4.21 | [3.80, 4.61]
hedges_g(iris$Sepal.Length, iris$Sepal.Width)
## Hedge's g |       95% CI
## ------------------------
##      4.20 | [3.79, 4.60]
glass_delta(iris$Sepal.Length, iris$Sepal.Width)
## Glass' delta |       95% CI
## ---------------------------
##         6.39 | [5.83, 6.95]
```

### ANOVAs (Eta<sup>2</sup>, Omega<sup>2</sup>, …)

``` r
model <- aov(Sepal.Length ~ Species, data = iris)

omega_squared(model)
## Parameter | Omega2 (partial) |       90% CI
## -------------------------------------------
## Species   |             0.61 | [0.53, 0.67]
eta_squared(model)
## Parameter | Eta2 (partial) |       90% CI
## -----------------------------------------
## Species   |           0.62 | [0.54, 0.68]
epsilon_squared(model)
## Parameter | Epsilon2 (partial) |       90% CI
## ---------------------------------------------
## Species   |               0.61 | [0.54, 0.67]
cohens_f(model)
## Parameter | Cohen's f (partial) |       90% CI
## ----------------------------------------------
## Species   |                1.27 | [1.09, 1.45]
```

### Regression Models

Importantly, `effectsize` also provides [advanced
methods](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
to compute standardized parameters for regression models.

``` r
lm(Sepal.Length ~ Species + Sepal.Length, data = iris) %>% 
  standardize_parameters()
## Parameter         | Coefficient (std.) |         95% CI
## -------------------------------------------------------
## (Intercept)       |              -1.01 | [-1.18, -0.84]
## Speciesversicolor |               1.12 | [ 0.88,  1.37]
## Speciesvirginica  |               1.91 | [ 1.66,  2.16]
## 
## # Standardization method: Refit
```

## Effect Size Interpretation

The package allows for an automated interpretation of different indices.

``` r
interpret_r(r = 0.3)
## [1] "large"
```

Different sets of “rules of thumb” are implemented ([**guidelines are
detailed
here**](https://easystats.github.io/effectsize/articles/interpret.html))
and can be easily changed.

``` r
interpret_d(d = 0.45, rules = "cohen1988")
## [1] "small"
interpret_d(d = 0.45, rules = "gignac2016")
## [1] "moderate"
```

## Effect Size Conversion

The package also provides ways of converting between different effect
sizes.

``` r
convert_d_to_r(d = 1)
## [1] 0.447
```

## Standardization

Many indices of effect size stem out, or are related, to
[*standardization*](https://easystats.github.io/effectsize/articles/standardize_parameters.html).
Thus, it is expected that `effectsize` provides functions to standardize
data and models.

### Data standardization, normalization and rank-transformation

A standardization sets the mean and SD to 0 and 1:

``` r
library(parameters)

df <- standardize(iris)
describe_distribution(df$Sepal.Length)
##      Mean | SD |  IQR |         Range | Skewness | Kurtosis |   n | n_Missing
## -----------------------------------------------------------------------------
## -4.48e-16 |  1 | 1.57 | [-1.86, 2.48] |     0.31 |    -0.55 | 150 |         0
```

This can be also applied to statistical models:

``` r
std_model <- standardize(lm(Sepal.Length ~ Species, data = iris))
coef(std_model)
##       (Intercept) Speciesversicolor  Speciesvirginica 
##             -1.01              1.12              1.91
```

Alternatively, normalization is similar to standardization in that it is
a linear translation of the parameter space (i.e., it does not change
the shape of the data distribution). However, it puts the values within
a 0 - 1 range, which can be useful in cases where you want to compare or
visualise data on the same scale.

``` r
df <- normalize(iris)
describe_distribution(df$Sepal.Length)
## Mean |   SD |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
## -------------------------------------------------------------------------
## 0.43 | 0.23 | 0.36 | [0.00, 1.00] |     0.31 |    -0.55 | 150 |         0
```

This is a special case of a rescaling function, which can be used to
rescale the data to an arbitrary new scale. Let’s change all numeric
variables to “percentages”:

``` r
df <- change_scale(iris, to = c(0, 100)) 
describe_distribution(df$Sepal.Length)
##  Mean |    SD |   IQR |          Range | Skewness | Kurtosis |   n | n_Missing
## ------------------------------------------------------------------------------
## 42.87 | 23.00 | 36.11 | [0.00, 100.00] |     0.31 |    -0.55 | 150 |         0
```

For some robust statistics, one might also want to transfom the numeric
values into *ranks* (or signed-ranks), which can be performed using the
`ranktransform()` function.

``` r
ranktransform(c(1, 3, -2, 6, 6, 0))
## [1] 3.0 4.0 1.0 5.5 5.5 2.0
```
