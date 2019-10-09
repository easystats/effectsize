
<!-- # bayestestR <img src='man/figures/logo.png' align="right" height="139" /> -->

# effectsize

[![CRAN](http://www.r-pkg.org/badges/version/effectsize)](https://cran.r-project.org/package=effectsize)
[![downloads](http://cranlogs.r-pkg.org/badges/effectsize)](https://cran.r-project.org/package=effectsize)
[![Build
Status](https://travis-ci.org/easystats/effectsize.svg?branch=master)](https://travis-ci.org/easystats/effectsize)
[![codecov](https://codecov.io/gh/easystats/effectsize/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/effectsize)

***Size does matter***

The goal of this package is to provide utilities to work with indices of
effect size and standardized parameters, allowing computation and
conversion of indices such as Cohen’s *d*, *r*, odds, etc.

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

## Standardization

### Data Standardization and Normalization

``` r
library(parameters)

df <- standardize(iris)
describe_distribution(df$Sepal.Length)
```

| Mean | SD |   Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | -: | ----: | --: | -------: | -------: | --: | ---------: |
|    0 |  1 | \-1.9 | 2.5 |      0.3 |    \-0.6 | 150 |          0 |

``` r
df <- normalize(iris)
describe_distribution(df$Sepal.Length)
```

| Mean |  SD | Min | Max | Skewness | Kurtosis |   n | n\_Missing |
| ---: | --: | --: | --: | -------: | -------: | --: | ---------: |
|  0.4 | 0.2 |   0 |   1 |      0.3 |    \-0.6 | 150 |          0 |

### Model Standardization

``` r
std_model <- standardize(lm(Sepal.Length ~ Species, data = iris))
coef(std_model)
```

## Computation

``` r
standardize_parameters(lm(Sepal.Length ~ Species, data = iris))
##           Parameter Std_Coefficient
## 1       (Intercept)            -1.0
## 2 Speciesversicolor             1.1
## 3  Speciesvirginica             1.9
```

## Interpretation

``` r
interpret_d(d = 0.5)
## [1] "medium"
```

## Conversion

``` r
convert_d_to_r(d = 1)
## [1] 0.45
```
