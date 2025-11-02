# Interpret Intraclass Correlation Coefficient (ICC)

The value of an ICC lies between 0 to 1, with 0 indicating no
reliability among raters and 1 indicating perfect reliability.

## Usage

``` r
interpret_icc(icc, rules = "koo2016", ...)
```

## Arguments

- icc:

  Value or vector of Intraclass Correlation Coefficient (ICC) values.

- rules:

  Can be `"koo2016"` (default) or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not used for now.

## Rules

- Koo (2016) (`"koo2016"`; default)

  - **ICC \< 0.50** - Poor reliability

  - **0.5 \<= ICC \< 0.75** - Moderate reliability

  - **0.75 \<= ICC \< 0.9** - Good reliability

  - \*\*ICC \>= 0.9 \*\* - Excellent reliability

## References

- Koo, T. K., and Li, M. Y. (2016). A guideline of selecting and
  reporting intraclass correlation coefficients for reliability
  research. Journal of chiropractic medicine, 15(2), 155-163.

## Examples

``` r
interpret_icc(0.6)
#> [1] "moderate"
#> (Rules: koo2016)
#> 
interpret_icc(c(0.4, 0.8))
#> [1] "poor" "good"
#> (Rules: koo2016)
#> 
```
