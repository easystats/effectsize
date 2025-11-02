# Interpret Coefficient of Determination (\\R^2\\)

Interpret Coefficient of Determination (\\R^2\\)

## Usage

``` r
interpret_r2(r2, rules = "cohen1988")
```

## Arguments

- r2:

  Value or vector of \\R^2\\ values.

- rules:

  Can be `"cohen1988"` (default), `"falk1992"`, `"chin1998"`,
  `"hair2011"`, or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md)\].

## Rules

### For Linear Regression

- Cohen (1988) (`"cohen1988"`; default)

  - **R2 \< 0.02** - Very weak

  - **0.02 \<= R2 \< 0.13** - Weak

  - **0.13 \<= R2 \< 0.26** - Moderate

  - **R2 \>= 0.26** - Substantial

- Falk & Miller (1992) (`"falk1992"`)

  - **R2 \< 0.1** - Negligible

  - **R2 \>= 0.1** - Adequate

### For PLS / SEM R-Squared of *latent* variables

- Chin, W. W. (1998) (`"chin1998"`)

  - **R2 \< 0.19** - Very weak

  - **0.19 \<= R2 \< 0.33** - Weak

  - **0.33 \<= R2 \< 0.67** - Moderate

  - **R2 \>= 0.67** - Substantial

- Hair et al. (2011) (`"hair2011"`)

  - **R2 \< 0.25** - Very weak

  - **0.25 \<= R2 \< 0.50** - Weak

  - **0.50 \<= R2 \< 0.75** - Moderate

  - **R2 \>= 0.75** - Substantial

## References

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Falk, R. F., & Miller, N. B. (1992). A primer for soft modeling.
  University of Akron Press.

- Chin, W. W. (1998). The partial least squares approach to structural
  equation modeling. Modern methods for business research, 295(2),
  295-336.

- Hair, J. F., Ringle, C. M., & Sarstedt, M. (2011). PLS-SEM: Indeed a
  silver bullet. Journal of Marketing theory and Practice, 19(2),
  139-152.

## Examples

``` r
interpret_r2(.02)
#> [1] "weak"
#> (Rules: cohen1988)
#> 
interpret_r2(c(.5, .02))
#> [1] "substantial" "weak"       
#> (Rules: cohen1988)
#> 
```
