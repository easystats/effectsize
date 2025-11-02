# Interpret the Variance Inflation Factor (VIF)

Interpret VIF index of multicollinearity.

## Usage

``` r
interpret_vif(vif, rules = "default")
```

## Arguments

- vif:

  Value or vector of VIFs.

- rules:

  Can be `"default"` or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

## Rules

- Default

  - **VIF \< 5** - Low

  - **5 \<= VIF \< 10** - Moderate

  - **VIF \>= 10** - High

## Examples

``` r

interpret_vif(c(1.4, 30.4))
#> [1] "low"  "high"
#> (Rules: default)
#> 
```
