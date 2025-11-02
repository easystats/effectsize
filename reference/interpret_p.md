# Interpret *p*-Values

Interpret *p*-Values

## Usage

``` r
interpret_p(p, rules = "default")
```

## Arguments

- p:

  Value or vector of p-values.

- rules:

  Can be `"default"`, `"rss"` (for *Redefine statistical significance*
  rules) or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

## Rules

- Default

  - **p \>= 0.05** - Not significant

  - **p \< 0.05** - Significant

- Benjamin et al. (2018) (`"rss"`)

  - **p \>= 0.05** - Not significant

  - **0.005 \<= p \< 0.05** - Suggestive

  - **p \< 0.005** - Significant

## References

- Benjamin, D. J., Berger, J. O., Johannesson, M., Nosek, B. A.,
  Wagenmakers, E. J., Berk, R., ... & Cesarini, D. (2018). Redefine
  statistical significance. Nature Human Behaviour, 2(1), 6-10.

## Examples

``` r
interpret_p(c(.5, .02, 0.001))
#> [1] "not significant" "significant"     "significant"    
#> (Rules: default)
#> 
interpret_p(c(.5, .02, 0.001), rules = "rss")
#> [1] "not significant" "suggestive"      "significant"    
#> (Rules: rss)
#> 

stars <- rules(c(0.001, 0.01, 0.05, 0.1), c("***", "**", "*", "+", ""),
  right = FALSE, name = "stars"
)
interpret_p(c(.5, .02, 0.001), rules = stars)
#> [1] ""   "*"  "**"
#> (Rules: stars)
#> 
```
