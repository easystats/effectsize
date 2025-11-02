# Interpret ANOVA Effect Sizes

Interpret ANOVA Effect Sizes

## Usage

``` r
interpret_omega_squared(es, rules = "field2013", ...)

interpret_eta_squared(es, rules = "field2013", ...)

interpret_epsilon_squared(es, rules = "field2013", ...)

interpret_r2_semipartial(es, rules = "field2013", ...)
```

## Arguments

- es:

  Value or vector of (partial) eta / omega / epsilon squared or
  semipartial r squared values.

- rules:

  Can be `"field2013"` (default), `"cohen1992"` or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not used for now.

## Rules

- Field (2013) (`"field2013"`; default)

  - **ES \< 0.01** - Very small

  - **0.01 \<= ES \< 0.06** - Small

  - **0.06 \<= ES \< 0.14** - Medium

  - \*\*ES \>= 0.14 \*\* - Large

- Cohen (1992) (`"cohen1992"`) applicable to one-way anova, or to
  *partial* eta / omega / epsilon squared in multi-way anova.

  - **ES \< 0.02** - Very small

  - **0.02 \<= ES \< 0.13** - Small

  - **0.13 \<= ES \< 0.26** - Medium

  - **ES \>= 0.26** - Large

## References

- Field, A (2013) Discovering statistics using IBM SPSS Statistics.
  Fourth Edition. Sage:London.

- Cohen, J. (1992). A power primer. Psychological bulletin, 112(1), 155.

## See also

https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize/

## Examples

``` r
interpret_eta_squared(.02)
#> [1] "small"
#> (Rules: field2013)
#> 
interpret_eta_squared(c(.5, .02), rules = "cohen1992")
#> [1] "large" "small"
#> (Rules: cohen1992)
#> 
```
