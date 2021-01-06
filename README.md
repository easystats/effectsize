
# effectsize <img src='man/figures/logo.png' align="right" height="139" />

[![DOI](https://joss.theoj.org/papers/10.21105/joss.02815/status.svg)](https://doi.org/10.21105/joss.02815)
[![CRAN](http://www.r-pkg.org/badges/version/effectsize)](https://cran.r-project.org/package=effectsize)
[![downloads](http://cranlogs.r-pkg.org/badges/effectsize)](https://cran.r-project.org/package=effectsize)
[![R-check](https://github.com/easystats/effectsize/workflows/R-check/badge.svg)](https://github.com/easystats/effectsize/actions)
[![pkgdown](https://github.com/easystats/effectsize/workflows/pkgdown/badge.svg)](https://github.com/easystats/effectsize/actions)

***Size does matter***

The goal of this package is to provide utilities to work with indices of
effect size and standardized parameters, allowing computation and
conversion of indices such as Cohen’s *d*, *r*, odds-ratios, etc.

## Installation

Run the following to install the latest GitHub-version of `effectsize`:

``` r
install.packages("remotes")
remotes::install_github("easystats/effectsize")
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

-   [**Data
    Standardization**](https://easystats.github.io/effectsize/articles/standardize_data.html)
-   [**Parameter and Model
    Standardization**](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
-   [**ANOVA Effect
    Sizes**](https://easystats.github.io/effectsize/articles/anovaES.html)
-   [**Effect Sizes in Bayesian
    Models**](https://easystats.github.io/effectsize/articles/bayesian_models.html)
-   [**Effect Size
    Conversion**](https://easystats.github.io/effectsize/articles/convert.html)
-   [**Effect Size from Test
    Statistics**](https://easystats.github.io/effectsize/articles/from_test_statistics.html)
-   [**Automated Interpretation of Indices of Effect
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
## - Bias corrected using Hedges and Olkin's method.

glass_delta(mpg ~ am, data = mtcars)
## Glass' delta |         95% CI
## -----------------------------
## -1.17        | [-2.06, -0.58]
```

### ANOVAs (Eta<sup>2</sup>, Omega<sup>2</sup>, …)

``` r
model <- aov(mpg ~ factor(gear), data = mtcars)

eta_squared(model)
## Parameter    | Eta2 |       90% CI
## ----------------------------------
## factor(gear) | 0.43 | [0.18, 0.59]

omega_squared(model)
## Parameter    | Omega2 |       90% CI
## ------------------------------------
## factor(gear) |   0.38 | [0.14, 0.55]

epsilon_squared(model)
## Parameter    | Epsilon2 |       90% CI
## --------------------------------------
## factor(gear) |     0.39 | [0.14, 0.56]
```

And more…

### Regression Models (Standardized Parameters)

Importantly, `effectsize` also provides [advanced
methods](https://easystats.github.io/effectsize/articles/standardize_parameters.html)
to compute standardized parameters for regression models.

``` r
m <- lm(rating ~ complaints + privileges + advance, data = attitude)

standardize_parameters(m)
## # Standardization method: refit
## 
## Parameter   | Coefficient (std.) |        95% CI
## ------------------------------------------------
## (Intercept) |          -9.57e-16 | [-0.22, 0.22]
## complaints  |               0.85 | [ 0.58, 1.13]
## privileges  |              -0.04 | [-0.33, 0.24]
## advance     |              -0.02 | [-0.26, 0.22]
```

Also, models can be re-fit with standardized data:

``` r
standardize(m)
## 
## Call:
## lm(formula = rating ~ complaints + privileges + advance, data = data_std)
## 
## Coefficients:
## (Intercept)   complaints   privileges      advance  
##   -9.57e-16     8.55e-01    -4.35e-02    -2.19e-02
```

<!-- add cohens_f2? -->

## Effect Size Conversion

The package also provides ways of converting between different effect
sizes.

``` r
convert_d_to_r(d = 1)
## [1] 0.447
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
## Eta2 (partial) |       90% CI
## -----------------------------
## 0.20           | [0.07, 0.34]
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
interpret_d(d = 0.45, rules = "cohen1988")
## [1] "small"
## (Rules: cohen1988)

interpret_d(d = 0.45, rules = "gignac2016")
## [1] "moderate"
## (Rules: gignac2016)
```

## Utilities

*Data Standardization, Normalization, Scaling, and Rank-Transforming*

Many indices of effect size stem out, or are related, to
[*standardization*](https://easystats.github.io/effectsize/articles/standardize_parameters.html).
Thus, it is expected that `effectsize` provides functions to standardize
data.

A standardization sets the mean and SD to 0 and 1:

``` r
library(parameters) # for describe_distribution

df <- standardize(attitude)
describe_distribution(df$rating)
##      Mean | SD |  IQR |         Range | Skewness | Kurtosis |  n | n_Missing
## ----------------------------------------------------------------------------
## -5.46e-16 |  1 | 1.29 | [-2.02, 1.67] |    -0.40 |    -0.49 | 30 |         0
```

Alternatively, normalization is similar to standardization in that it is
a linear translation of the parameter space (i.e., it does not change
the shape of the data distribution). However, it puts the values within
a 0 - 1 range, which can be useful in cases where you want to compare or
visualise data on the same scale.

``` r
df <- normalize(attitude)
describe_distribution(df$rating)
## Mean |   SD |  IQR |        Range | Skewness | Kurtosis |  n | n_Missing
## ------------------------------------------------------------------------
## 0.55 | 0.27 | 0.35 | [0.00, 1.00] |    -0.40 |    -0.49 | 30 |         0
```

This is a special case of a rescaling function, which can be used to
rescale the data to an arbitrary new scale. Let’s change all numeric
variables to “percentages”:

``` r
df <- change_scale(attitude, to = c(0, 100)) 
describe_distribution(df$rating)
##  Mean |    SD |   IQR |          Range | Skewness | Kurtosis |  n | n_Missing
## -----------------------------------------------------------------------------
## 54.74 | 27.05 | 35.00 | [0.00, 100.00] |    -0.40 |    -0.49 | 30 |         0
```

For some robust statistics, one might also want to transfom the numeric
values into *ranks*, which can be performed using the `ranktransform()`
function.

``` r
ranktransform(c(1, 3, -2, 6, 6, 0.5))
## [1] 3.0 4.0 1.0 5.5 5.5 2.0
```

or signed-ranks:

``` r
ranktransform(c(1, 3, -2, 6, 6, 0.5), sign = TRUE)
## [1]  2.0  4.0 -3.0  5.5  5.5  1.0
```

### Citation

In order to cite this package, please use the following citation:

-   Ben-Shachar M, Lüdecke D, Makowski D (2020). effectsize: Estimation
    of Effect Size Indices and Standardized Parameters. *Journal of Open
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
issue](https://github.com/easystats/effectsize/issues). Anyone wishing
to contribute to the package by adding functions, features, or in
another way, please follow [this
guide](https://github.com/easystats/effectsize/blob/main/.github/CONTRIBUTING.md)
and our [code of
conduct](https://github.com/easystats/effectsize/blob/main/.github/CODE_OF_CONDUCT.md).
