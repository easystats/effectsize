# Interpret Cohen's *g*

Interpret Cohen's *g*

## Usage

``` r
interpret_cohens_g(g, rules = "cohen1988", ...)
```

## Arguments

- g:

  Value or vector of effect size values.

- rules:

  Can be `"cohen1988"` (default) or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not directly used.

## Note

"*Since **g** is so transparently clear a unit, it is expected that
workers in any given substantive area of the behavioral sciences will
very frequently be able to set relevant \[effect size\] values without
the proposed conventions, or set up conventions of their own which are
suited to their area of inquiry.*" - Cohen, 1988, page 147.

## Rules

Rules apply to equally to positive and negative *g* (i.e., they are
given as absolute values).

- Cohen (1988) (`"cohen1988"`; default)

  - **d \< 0.05** - Very small

  - **0.05 \<= d \< 0.15** - Small

  - **0.15 \<= d \< 0.25** - Medium

  - **d \>= 0.25** - Large

## References

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

## Examples

``` r
interpret_cohens_g(.02)
#> [1] "very small"
#> (Rules: cohen1988)
#> 
interpret_cohens_g(c(.3, .15))
#> [1] "large"  "medium"
#> (Rules: cohen1988)
#> 
```
