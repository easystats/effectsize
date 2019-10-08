
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

# Features

## Standardization

``` r
iris %>% 
  standardize(robust = TRUE) %>% 
  summary()
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
##  Min.   :-1.45   Min.   :-2.25   Min.   :-1.81   Min.   :-1.16  
##  1st Qu.:-0.67   1st Qu.:-0.45   1st Qu.:-1.48   1st Qu.:-0.96  
##  Median : 0.00   Median : 0.00   Median : 0.00   Median : 0.00  
##  Mean   : 0.04   Mean   : 0.13   Mean   :-0.32   Mean   :-0.10  
##  3rd Qu.: 0.58   3rd Qu.: 0.67   3rd Qu.: 0.40   3rd Qu.: 0.48  
##  Max.   : 2.02   Max.   : 3.15   Max.   : 1.38   Max.   : 1.16  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
```

``` r
iris %>% 
  normalize() %>% 
  summary()
##   Sepal.Length   Sepal.Width    Petal.Length   Petal.Width  
##  Min.   :0.00   Min.   :0.00   Min.   :0.00   Min.   :0.00  
##  1st Qu.:0.22   1st Qu.:0.33   1st Qu.:0.10   1st Qu.:0.08  
##  Median :0.42   Median :0.42   Median :0.57   Median :0.50  
##  Mean   :0.43   Mean   :0.44   Mean   :0.47   Mean   :0.46  
##  3rd Qu.:0.58   3rd Qu.:0.54   3rd Qu.:0.69   3rd Qu.:0.71  
##  Max.   :1.00   Max.   :1.00   Max.   :1.00   Max.   :1.00  
##        Species  
##  setosa    :50  
##  versicolor:50  
##  virginica :50  
##                 
##                 
## 
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
interpret_d(0.5)
## [1] "medium"
```

## Conversion

``` r
convert_d_to_r(1)
## [1] 0.45
```

# References
