# Interpret Standardized Differences

Interpretation of standardized differences using different sets of rules
of thumb.

## Usage

``` r
interpret_cohens_d(d, rules = "cohen1988", ...)

interpret_hedges_g(g, rules = "cohen1988")

interpret_glass_delta(delta, rules = "cohen1988")
```

## Arguments

- d, g, delta:

  Value or vector of effect size values.

- rules:

  Can be `"cohen1988"` (default), `"gignac2016"`, `"sawilowsky2009"`,
  `"lovakov2021"` or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not directly used.

## Rules

Rules apply to equally to positive and negative *d* (i.e., they are
given as absolute values).

- Cohen (1988) (`"cohen1988"`; default)

  - **d \< 0.2** - Very small

  - **0.2 \<= d \< 0.5** - Small

  - **0.5 \<= d \< 0.8** - Medium

  - **d \>= 0.8** - Large

- Sawilowsky (2009) (`"sawilowsky2009"`)

  - **d \< 0.1** - Tiny

  - **0.1 \<= d \< 0.2** - Very small

  - **0.2 \<= d \< 0.5** - Small

  - **0.5 \<= d \< 0.8** - Medium

  - **0.8 \<= d \< 1.2** - Large

  - **1.2 \<= d \< 2** - Very large

  - **d \>= 2** - Huge

- Lovakov & Agadullina (2021) (`"lovakov2021"`)

  - **d \< 0.15** - Very small

  - **0.15 \<= d \< 0.36** - Small

  - **0.36 \<= d \< 0.65** - Medium

  - **d \>= 0.65** - Large

- Gignac & Szodorai (2016) (`"gignac2016"`, based on the
  [`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  conversion, see
  [`interpret_r()`](https://easystats.github.io/effectsize/reference/interpret_r.md))

  - **d \< 0.2** - Very small

  - **0.2 \<= d \< 0.41** - Small

  - **0.41 \<= d \< 0.63** - Moderate

  - **d \>= 0.63** - Large

## References

- Lovakov, A., & Agadullina, E. R. (2021). Empirically Derived
  Guidelines for Effect Size Interpretation in Social Psychology.
  European Journal of Social Psychology.

- Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for
  individual differences researchers. Personality and individual
  differences, 102, 74-78.

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Sawilowsky, S. S. (2009). New effect size rules of thumb.

## Examples

``` r
interpret_cohens_d(.02)
#> [1] "very small"
#> (Rules: cohen1988)
#> 
interpret_cohens_d(c(.5, .02))
#> [1] "medium"     "very small"
#> (Rules: cohen1988)
#> 
interpret_cohens_d(.3, rules = "lovakov2021")
#> [1] "small"
#> (Rules: lovakov2021)
#> 
```
