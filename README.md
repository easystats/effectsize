
# effectsize <img src='man/figures/logo.png' align="right" height="139" />

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

This package is focused on indices of effect size. But **there are
hundreds of them\! Thus, *everybody* is welcome to contribute** by
adding support for the interpretation of new indices. If you’re not sure
how to code it it’s okay, just open an issue to discuss it and we’ll
help :)

## Effect Size Computation

### Measure of association (correlation *r*)

``` r
library(dplyr)

lm(Sepal.Length ~ Petal.Length, data = iris) %>% 
  standardize_parameters()
```

| Parameter    | Std\_Coefficient |
| :----------- | ---------------: |
| (Intercept)  |             0.00 |
| Petal.Length |             0.87 |

Standardizing the coefficient of this simple linear regression gives a
value of `0.87`, but did you know that this is actually the **same as a
correlation**? Thus, you can eventually apply some (*in*)famous
interpretation guidelines (e.g., Cohen’s rules of thumb).

``` r
library(parameters)

cor.test(iris$Sepal.Length, iris$Petal.Length) %>% 
  model_parameters()
## Parameter1        |        Parameter2 |    r |     t |  df |      p |       95% CI |  Method
## --------------------------------------------------------------------------------------------
## iris$Sepal.Length | iris$Petal.Length | 0.87 | 21.65 | 148 | < .001 | [0.83, 0.91] | Pearson
```

### Standardized differences

How does it work in the case of differences, when **factors** are
entered and differences between a given level and a reference level (the
intercept)? You might have heard that it is similar to a **Cohen’s
*d***. Well, let’s see.

``` r
lm(Sepal.Length ~ Species, data = iris) %>% 
  standardize_parameters()
```

| Parameter         | Std\_Coefficient |
| :---------------- | ---------------: |
| (Intercept)       |           \-1.01 |
| Speciesversicolor |             1.12 |
| Speciesvirginica  |             1.91 |

This linear model suggests that the *standardized* difference between
the *versicolor* level of Species and the *setosa* level (the reference
level - the intercept) is of 1.12 standard deviation of `Sepal.Length`
(because the response variable was standardized, right?). Let’s compute
the **Cohen’s *d*** between these two levels:

``` r
# Select portion of data containing the two levels of interest
data <- iris[iris$Species %in% c("setosa", "versicolor"), ]

cohens_d(Sepal.Length ~ Species, data = data) 
## [1] 2.1
```

***It is very different\!*** Why? How? Both differences should be
expressed in terms of SD of the response variable. *And there’s the
trick*. First of all, in the linear model above, the SD by which the
difference is scaled is the one of the whole response, which include
**all the three levels**, whereas below, we filtered the data to only
include the levels of interest. If we recompute the model on this
filtered data, it should be better:

``` r
lm(Sepal.Length ~ Species, data = data) %>% 
  standardize_parameters()
```

| Parameter         | Std\_Coefficient |
| :---------------- | ---------------: |
| (Intercept)       |           \-0.72 |
| Speciesversicolor |             1.45 |

Not really. Why? Because the actual formula to compute a **Cohen’s *d***
doesn’t use the simple SD to scale the effect (as it is done when
standardizing the parameters), but computes something called the
[**pooled
SD**](https://easystats.github.io/effectsize/reference/pooled_sd.html).
However, this can be turned off by setting `correct = "raw"`.

``` r
cohens_d(Sepal.Length ~ Species, data = data, correct = "raw") 
## [1] 1.45
```

***And here we are :)***

### What about standardized interaction effects?

Well, this one’s a mess (at least for me). Help is required to make
sense out of it. Otherwise, *NEXT*.

## Effect Size Interpretation

The [**guidelines are detailed
here**](https://easystats.github.io/effectsize/articles/interpret.html).

``` r
interpret_d(d = 0.45, rules = "cohen1988")
## [1] "small"
interpret_d(d = 0.45, rules = "funder2019")
## [1] "medium"
```

## Effect Size Conversion

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
