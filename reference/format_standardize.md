# Format a Standardized Vector

Transform a standardized vector into character, e.g.,
`c("-1 SD", "Mean", "+1 SD")`.

## Usage

``` r
format_standardize(
  x,
  reference = x,
  robust = FALSE,
  digits = 1,
  protect_integers = TRUE,
  ...
)
```

## Arguments

- x:

  A standardized numeric vector.

- reference:

  The reference vector from which to compute the mean and SD.

- robust:

  Logical, if `TRUE`, centering is done by subtracting the median from
  the variables and dividing it by the median absolute deviation (MAD).
  If `FALSE`, variables are standardized by subtracting the mean and
  dividing it by the standard deviation (SD).

- digits:

  Number of digits for rounding or significant figures. May also be
  `"signif"` to return significant figures or `"scientific"` to return
  scientific notation. Control the number of digits by adding the value
  as suffix, e.g. `digits = "scientific4"` to have scientific notation
  with 4 decimal places, or `digits = "signif5"` for 5 significant
  figures (see also [`signif()`](https://rdrr.io/r/base/Round.html)).

- protect_integers:

  Should integers be kept as integers (i.e., without decimals)?

- ...:

  Other arguments to pass to
  [`insight::format_value()`](https://easystats.github.io/insight/reference/format_value.html)
  such as `digits`, etc.

## Examples

``` r
format_standardize(c(-1, 0, 1))
#> [1] -1.0 SD Mean    +1.0 SD
#> Levels: +1.0 SD Mean -1.0 SD
format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000))
#> [1] -1.0 SD Mean    +1.0 SD +2.0 SD
#> Levels: +2.0 SD +1.0 SD Mean -1.0 SD
format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE)
#> [1] -1.0 MAD Median   +1.0 MAD +2.0 MAD
#> Levels: +2.0 MAD +1.0 MAD Median -1.0 MAD

format_standardize(standardize(mtcars$wt), digits = 1)
#>  [1] -0.6 SD -0.3 SD -0.9 SD Mean    +0.2 SD +0.2 SD +0.4 SD Mean    -0.1 SD
#> [10] +0.2 SD +0.2 SD +0.9 SD +0.5 SD +0.6 SD +2.1 SD +2.3 SD +2.2 SD -1.0 SD
#> [19] -1.6 SD -1.4 SD -0.8 SD +0.3 SD +0.2 SD +0.6 SD +0.6 SD -1.3 SD -1.1 SD
#> [28] -1.7 SD Mean    -0.5 SD +0.4 SD -0.4 SD
#> 23 Levels: +2.3 SD +2.2 SD +2.1 SD +0.9 SD +0.6 SD +0.5 SD +0.4 SD ... -1.7 SD
format_standardize(standardize(mtcars$wt, robust = TRUE), digits = 1)
#>  [1] -0.9 MAD -0.6 MAD -1.3 MAD -0.1 MAD +0.1 MAD +0.2 MAD +0.3 MAD -0.2 MAD
#>  [9] -0.2 MAD +0.1 MAD +0.1 MAD +1.0 MAD +0.5 MAD +0.6 MAD +2.5 MAD +2.7 MAD
#> [17] +2.6 MAD -1.5 MAD -2.2 MAD -1.9 MAD -1.1 MAD +0.3 MAD +0.1 MAD +0.7 MAD
#> [25] +0.7 MAD -1.8 MAD -1.5 MAD -2.4 MAD -0.2 MAD -0.7 MAD +0.3 MAD -0.7 MAD
#> 22 Levels: +2.7 MAD +2.6 MAD +2.5 MAD +1.0 MAD +0.7 MAD +0.6 MAD ... -2.4 MAD
```
