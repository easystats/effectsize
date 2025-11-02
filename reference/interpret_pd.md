# Interpret Probability of Direction (pd)

Interpret Probability of Direction (pd)

## Usage

``` r
interpret_pd(pd, rules = "default", ...)
```

## Arguments

- pd:

  Value or vector of probabilities of direction.

- rules:

  Can be `"default"`, `"makowski2019"` or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not directly used.

## Rules

- Default (i.e., equivalent to p-values)

  - **pd \<= 0.975** - not significant

  - **pd \> 0.975** - significant

- Makowski et al. (2019) (`"makowski2019"`)

  - **pd \<= 0.95** - uncertain

  - **pd \> 0.95** - possibly existing

  - **pd \> 0.97** - likely existing

  - **pd \> 0.99** - probably existing

  - **pd \> 0.999** - certainly existing

## References

- Makowski, D., Ben-Shachar, M. S., Chen, S. H., and Lüdecke, D. (2019).
  Indices of effect existence and significance in the Bayesian
  framework. Frontiers in psychology, 10, 2767.

## Examples

``` r
interpret_pd(.98)
#> [1] "significant"
#> (Rules: default)
#> 
interpret_pd(c(.96, .99), rules = "makowski2019")
#> [1] "possibly existing" "likely existing"  
#> (Rules: makowski2019)
#> 
```
