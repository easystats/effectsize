# Interpret Bayesian Posterior Percentage in ROPE.

Interpretation of

## Usage

``` r
interpret_rope(rope, rules = "default", ci = 0.9)
```

## Arguments

- rope:

  Value or vector of percentages in ROPE.

- rules:

  A character string (see details) or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ci:

  The Credible Interval (CI) probability, corresponding to the
  proportion of HDI, that was used. Can be `1` in the case of "full
  ROPE".

## Rules

- Default

  - For CI \< 1

    - **Rope = 0** - Significant

    - **0 \< Rope \< 1** - Undecided

    - **Rope = 1** - Negligible

  - For CI = 1

    - **Rope \< 0.01** - Significant

    - **0.01 \< Rope \< 0.025** - Probably significant

    - **0.025 \< Rope \< 0.975** - Undecided

    - **0.975 \< Rope \< 0.99** - Probably negligible

    - **Rope \> 0.99** - Negligible

## References

[BayestestR's reporting
guidelines](https://easystats.github.io/bayestestR/articles/guidelines.html)

## Examples

``` r
interpret_rope(0, ci = 0.9)
#> [1] "significant"
#> (Rules: default)
#> 
interpret_rope(c(0.005, 0.99), ci = 1)
#> [1] "significant"         "probably negligible"
#> (Rules: default)
#> 
```
