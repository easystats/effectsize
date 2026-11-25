# Interpret ANOVA Effect Sizes

Interpret ANOVA Effect Sizes

## Usage

``` r
interpret_omega_squared(es, rules = "field2013", ...)

interpret_eta_squared(es, rules = "field2013", ...)

interpret_epsilon_squared(es, rules = "field2013", ...)

interpret_r2_semipartial(es, rules = "field2013", ...)

interpret_cohens_f2(es, rules = "field2013", ...)

interpret_cohens_f(es, rules = "field2013", ...)
```

## Arguments

- es:

  Value or vector of (partial) eta / omega / epsilon squared or
  semipartial r squared values, or Cohen's f or f squared.

- rules:

  Can be `"field2013"` (default for eta squared), `"cohen1992"` (default
  for f squared) or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not used for now.

## Rules

- Field (2013) (`"field2013"`; default)

  - **ES \< 0.01** - Very small

  - **0.01 \<= ES \< 0.06** - Small

  - **0.06 \<= ES \< 0.14** - Medium

  - \*\*ES \>= 0.14 \*\* - Large

- Cohen (1992) table 1 row 8 (`"cohen1992r8"` or `"cohen1992"`; multiple
  and multiple partial correlation)

  - **ES \< 0.02** - Very small

  - **0.02 \<= ES \< 0.13** - Small

  - **0.13 \<= ES \< 0.26** - Medium

  - **ES \>= 0.26** - Large

- Cohen (1992) row 7 (`"cohen1992r7"`; one-way ANOVA)

  - **ES \< 0.01** - Very small

  - **0.01 \<= ES \< 0.06** - Small

  - **0.06 \<= ES \< 0.14** - Medium

  - **ES \>= 0.14** - Large

The same rules can be applied to Cohen's \\(f^2\\) (and \\(f\\)) by
transforming those values:

    # Field (2013)
    eta2_to_f2(c(small = 0.01, medium = 0.06, large = 0.14))
    #>      small     medium      large
    #> 0.01010101 0.06382979 0.16279070

Cohen's rules are derived by applying the reverse transformation on
Cohen's cut-offs for \\(f^2\\) (row 8) and \\(f\\) (row 7):

    # Row 8
    f2_to_eta2(c(small = 0.02, medium = 0.15, large = 0.35))
    #>      small     medium      large
    #> 0.01960784 0.13043478 0.25925926

    # Row 7
    f_to_eta2(c(small = 0.1, medium = 0.25, large = 0.4))
    #>      small     medium      large
    #> 0.00990099 0.05882353 0.13793103

## References

- Field, A (2013) Discovering statistics using IBM SPSS Statistics.
  Fourth Edition. Sage:London.

- Cohen, J. (1992). [A power
  primer](https://doi.org/10.1037/0033-2909.112.1.155). Psychological
  Bulletin, 112(1), 155-159.

## See also

https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize/

## Examples

``` r
interpret_eta_squared(.02)
#> [1] "small"
#> (Rules: field2013)
#> 
interpret_eta_squared(c(.5, .02), rules = "cohen1992r8")
#> [1] "large" "small"
#> (Rules: cohen1992)
#> 
```
