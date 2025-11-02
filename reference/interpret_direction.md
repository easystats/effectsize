# Interpret Direction

Interpret Direction

## Usage

``` r
interpret_direction(x)
```

## Arguments

- x:

  Numeric value.

## Examples

``` r
interpret_direction(.02)
#> [1] "positive"
#> (Rules: math)
#> 
interpret_direction(c(.5, -.02))
#> [1] "positive" "negative"
#> (Rules: math)
#> 
interpret_direction(0)
#> [1] NA
#> (Rules: math)
#> 
```
