# Pooled Indices of (Co)Deviation

The Pooled Standard Deviation is a weighted average of standard
deviations for two or more groups, *assumed to have equal variance*. It
represents the common deviation among the groups, around each of their
respective means.

## Usage

``` r
sd_pooled(x, y = NULL, data = NULL, verbose = TRUE, ...)

mad_pooled(x, y = NULL, data = NULL, constant = 1.4826, verbose = TRUE, ...)

cov_pooled(x, y = NULL, data = NULL, verbose = TRUE, ...)
```

## Arguments

- x, y:

  A numeric vector, or a character name of one in `data`. Any missing
  values (`NA`s) are dropped from the resulting vector. `x` can also be
  a formula (see
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html)), in which
  case `y` is ignored.

- data:

  An optional data frame containing the variables.

- verbose:

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to or from other methods. When `x` is a formula,
  these can be `subset` and `na.action`.

- constant:

  scale factor.

## Value

Numeric, the pooled standard deviation. For `cov_pooled()` a matrix.

## Details

The standard version is calculated as: \$\$\sqrt{\frac{\sum (x_i -
\bar{x})^2}{n_1 + n_2 - 2}}\$\$ The robust version is calculated as:
\$\$1.4826 \times Median(\|\left\\x - Median_x,\\y -
Median_y\right\\\|)\$\$

## See also

[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`mahalanobis_d()`](https://easystats.github.io/effectsize/reference/mahalanobis_d.md)

## Examples

``` r
sd_pooled(mpg ~ am, data = mtcars)
#> [1] 4.902029
mad_pooled(mtcars$mpg, factor(mtcars$am))
#> [1] 4.52193

cov_pooled(mpg + hp + cyl ~ am, data = mtcars)
#>             mpg         hp       cyl
#> mpg   24.029887 -269.13174 -5.991498
#> hp  -269.131741 4570.24588 89.247233
#> cyl   -5.991498   89.24723  2.395682
```
