# Create an Interpretation Grid

Create a container for interpretation rules of thumb. Usually used in
conjunction with
[interpret](https://easystats.github.io/effectsize/reference/interpret.md).

## Usage

``` r
rules(values, labels = NULL, name = NULL, right = TRUE)

is.rules(x)
```

## Arguments

- values:

  Vector of reference values (edges defining categories or critical
  values).

- labels:

  Labels associated with each category. If `NULL`, will try to infer it
  from `values` (if it is a named vector or a list), otherwise, will
  return the breakpoints.

- name:

  Name of the set of rules (will be printed).

- right:

  logical, for threshold-type rules, indicating if the thresholds
  themselves should be included in the interval to the right (lower
  values) or in the interval to the left (higher values).

- x:

  An arbitrary R object.

## See also

[`interpret()`](https://easystats.github.io/effectsize/reference/interpret.md)

## Examples

``` r
rules(c(0.05), c("significant", "not significant"), right = FALSE)
#> # Reference Thresholds (Custom rules)
#> 
#>              Label            
#> ------------------------------
#>           significant   < 0.05
#> 0.05 <= not significant       
rules(c(0.2, 0.5, 0.8), c("small", "medium", "large"))
#> # Reference Values (Custom rules)
#> 
#> Labels ~ Values
#> ---------------
#>  small ~ 0.2   
#> medium ~ 0.5   
#>  large ~ 0.8   
rules(c("small" = 0.2, "medium" = 0.5), name = "Cohen's Rules")
#> # Reference Values (Cohen's Rules)
#> 
#> Labels ~ Values
#> ---------------
#>  small ~ 0.2   
#> medium ~ 0.5   
```
