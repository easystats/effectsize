
# effectsize <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/effectsize)](https://cran.r-project.org/package=effectsize)
[![downloads](http://cranlogs.r-pkg.org/badges/effectsize)](https://cran.r-project.org/package=effectsize)
[![Build
Status](https://travis-ci.org/easystats/effectsize.svg?branch=master)](https://travis-ci.org/easystats/effectsize)
[![codecov](https://codecov.io/gh/easystats/effectsize/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/effectsize)

***Size does matter***

The goal of this package is to provide utilities to work with indices of
effect size and standardized parameters, allowing computation and
conversion of indices such as Cohen’s *d*, *r*, odds-ratios, etc.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/effectsize")
```

``` r
library("effectsize")
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
  - [**Parameters
    Standardization**](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
  - [**Automated Interpretation of Indices of Effect
    Size**](https://easystats.github.io/effectsize/articles/interpret.html)
  - [**Effect size
    conversion**](https://easystats.github.io/effectsize/articles/convert.html)

# Features

This package is focused on indices of effect size. But **there are
hundreds of them\! Thus, *everybody* is welcome to contribute** by
adding support for the interpretation of new indices. If you’re not sure
how to code it it’s okay, just open an issue to discuss it and we’ll
help :)

## Effect Size Computation

### Basic Indices (Cohen’s *d*, Hedges’ *g*, Glass’ *delta*)

The package provides functions to compute indices of effect size.

``` r
cohens_d(iris$Sepal.Length, iris$Sepal.Width)
## [1] -4.21
hedges_g(iris$Sepal.Length, iris$Sepal.Width)
## [1] -4.2
glass_delta(iris$Sepal.Length, iris$Sepal.Width)
## [1] -3.36
```

### ANOVAs (Eta<sup>2</sup>, Omega<sup>2</sup>, …)

``` r
model <- aov(Sepal.Length ~ Species, data = iris)

omega_squared(model, partial = TRUE)
##   Parameter Omega_Sq_partial
## 1   Species            0.612
## 2 Residuals               NA
eta_squared(model, partial = TRUE)
##   Parameter Eta_Sq_partial
## 1   Species          0.619
## 2 Residuals             NA
epsilon_squared(model)
##   Parameter Epsilon_sq
## 1   Species      0.614
## 2 Residuals         NA
cohens_f(model)
##   Parameter Cohens_f
## 1   Species     1.27
## 2 Residuals       NA
```

### Regression Models

Importantly, `effectsize` also provides [advanced
methods](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
to compute standardized parameters for regression models.

``` r
lm(Sepal.Length ~ Species + Sepal.Length, data = iris) %>% 
  standardize_parameters()
```

| Parameter         | Std\_Coefficient |
| :---------------- | ---------------: |
| (Intercept)       |           \-1.01 |
| Speciesversicolor |             1.12 |
| Speciesvirginica  |             1.91 |

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
interpret_d(d = 0.45, rules = "funder2019")
## [1] "medium"
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
```

| Mean | SD |   Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | -: | ----: | --: | -------: | -------: | --: | ---------: |
|    0 |  1 | \-1.9 | 2.5 |      0.3 |    \-0.6 | 150 |          0 |

Alternatively, normalization is similar to standardization in that it is
a linear translation of the parameter space (i.e., it does not change
the shape of the data distribution). However, it puts the values within
a 0 - 1 range, which can be useful in cases where you want to compare or
visualise data on the same scale.

``` r
df <- normalize(iris)
describe_distribution(df$Sepal.Length)
```

| Mean |  SD | Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | --: | --: | --: | -------: | -------: | --: | ---------: |
|  0.4 | 0.2 |   0 |   1 |      0.3 |    \-0.6 | 150 |          0 |

For some robust statistics, one might also want to transfom the numeric
values into *ranks* (or signed-ranks), which can be performed using the
`ranktransform()` function.

``` r
ranktransform(c(1, 3, -2, 6, 6, 0))
```

### Model Standardization

``` r
std_model <- standardize(lm(Sepal.Length ~ Species, data = iris))
coef(std_model)
##       (Intercept) Speciesversicolor  Speciesvirginica 
##             -1.01              1.12              1.91
```
