# Interpret Kendall's Coefficient of Concordance *W*

Interpret Kendall's Coefficient of Concordance *W*

## Usage

``` r
interpret_kendalls_w(w, rules = "landis1977")
```

## Arguments

- w:

  Value or vector of Kendall's coefficient of concordance.

- rules:

  Can be `"landis1977"` (default) or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

## Rules

- Landis & Koch (1977) (`"landis1977"`; default)

  - **0.00 \<= w \< 0.20** - Slight agreement

  - **0.20 \<= w \< 0.40** - Fair agreement

  - **0.40 \<= w \< 0.60** - Moderate agreement

  - **0.60 \<= w \< 0.80** - Substantial agreement

  - **w \>= 0.80** - Almost perfect agreement

## References

- Landis, J. R., & Koch G. G. (1977). The measurement of observer
  agreement for categorical data. Biometrics, 33:159-74.
