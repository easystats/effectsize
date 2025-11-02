# Generic Function for Interpretation

Interpret a value based on a set of rules. See
[`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

## Usage

``` r
interpret(x, ...)

# S3 method for class 'numeric'
interpret(x, rules, name = attr(rules, "rule_name"), transform = NULL, ...)

# S3 method for class 'effectsize_table'
interpret(x, rules, transform = NULL, ...)
```

## Arguments

- x:

  Vector of value break points (edges defining categories), or a data
  frame of class `effectsize_table`.

- ...:

  Currently not used.

- rules:

  Set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).
  When `x` is a data frame, can be a name of an established set of
  rules.

- name:

  Name of the set of rules (will be printed).

- transform:

  a function (or name of a function) to apply to `x` before
  interpreting. See examples.

## Value

- For numeric input: A character vector of interpretations.

- For data frames: the `x` input with an additional `Interpretation`
  column.

## See also

[`rules()`](https://easystats.github.io/effectsize/reference/rules.md)

## Examples

``` r
rules_grid <- rules(c(0.01, 0.05), c("very significant", "significant", "not significant"))
interpret(0.001, rules_grid)
#> [1] "very significant"
#> (Rules: Custom rules)
#> 
interpret(0.021, rules_grid)
#> [1] "significant"
#> (Rules: Custom rules)
#> 
interpret(0.08, rules_grid)
#> [1] "not significant"
#> (Rules: Custom rules)
#> 
interpret(c(0.01, 0.005, 0.08), rules_grid)
#> [1] "very significant" "very significant" "not significant" 
#> (Rules: Custom rules)
#> 

interpret(c(0.35, 0.15), c("small" = 0.2, "large" = 0.4), name = "Cohen's Rules")
#> [1] "large" "small"
#> (Rules: Cohen's Rules)
#> 
interpret(c(0.35, 0.15), rules(c(0.2, 0.4), c("small", "medium", "large")))
#> [1] "medium" "small" 
#> (Rules: Custom rules)
#> 

bigness <- rules(c(1, 10), c("small", "medium", "big"))
interpret(abs(-5), bigness)
#> [1] "medium"
#> (Rules: Custom rules)
#> 
interpret(-5, bigness, transform = abs)
#> [1] "medium"
#> (Rules: Custom rules)
#> 

# ----------
d <- cohens_d(mpg ~ am, data = mtcars)
interpret(d, rules = "cohen1988")
#> Cohen's d |         95% CI | Interpretation
#> -------------------------------------------
#> -1.48     | [-2.27, -0.67] |          large
#> 
#> - Estimated using pooled SD.
#> - Interpretation rule: cohen1988

d <- glass_delta(mpg ~ am, data = mtcars)
interpret(d, rules = "gignac2016")
#> Glass' delta (adj.) |         95% CI
#> ------------------------------------
#> -1.10               | [-1.80, -0.37]

interpret(d, rules = rules(1, c("tiny", "yeah okay")))
#> Glass' delta (adj.) |         95% CI
#> ------------------------------------
#> -1.10               | [-1.80, -0.37]

m <- lm(formula = wt ~ am * cyl, data = mtcars)
eta2 <- eta_squared(m)
interpret(eta2, rules = "field2013")
#> # Effect Size for ANOVA (Type I)
#> 
#> Parameter | Eta2 (partial) |       95% CI | Interpretation
#> ----------------------------------------------------------
#> am        |           0.63 | [0.44, 1.00] |          large
#> cyl       |           0.47 | [0.24, 1.00] |          large
#> am:cyl    |       2.26e-03 | [0.00, 1.00] |     very small
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
#> - Interpretation rule: field2013

X <- chisq.test(mtcars$am, mtcars$cyl == 8)
interpret(oddsratio(X), rules = "cohen1988")
#> Odds ratio |       95% CI | Interpretation
#> ------------------------------------------
#> 0.11       | [0.02, 0.62] |          large
#> 
#> - Interpretation rule: cohen1988
interpret(cramers_v(X), rules = "lovakov2021")
#> Cramer's V (adj.) |       95% CI | Interpretation
#> -------------------------------------------------
#> 0.44              | [0.03, 1.00] |          large
#> 
#> - One-sided CIs: upper bound fixed at [1.00].
#> - Interpretation rule: lovakov2021
```
