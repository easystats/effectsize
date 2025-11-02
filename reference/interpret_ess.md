# Interpret Bayesian Diagnostic Indices

Interpretation of Bayesian diagnostic indices, such as Effective Sample
Size (ESS) and Rhat.

## Usage

``` r
interpret_ess(ess, rules = "burkner2017")

interpret_rhat(rhat, rules = "vehtari2019")
```

## Arguments

- ess:

  Value or vector of Effective Sample Size (ESS) values.

- rules:

  A character string (see *Rules*) or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- rhat:

  Value or vector of Rhat values.

## Rules

### ESS

- Bürkner, P. C. (2017) (`"burkner2017"`; default)

  - **ESS \< 1000** - Insufficient

  - **ESS \>= 1000** - Sufficient

### Rhat

- Vehtari et al. (2019) (`"vehtari2019"`; default)

  - **Rhat \< 1.01** - Converged

  - **Rhat \>= 1.01** - Failed

- Gelman & Rubin (1992) (`"gelman1992"`)

  - **Rhat \< 1.1** - Converged

  - **Rhat \>= 1.1** - Failed

## References

- Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel
  models using Stan. Journal of Statistical Software, 80(1), 1-28.

- Gelman, A., & Rubin, D. B. (1992). Inference from iterative simulation
  using multiple sequences. Statistical science, 7(4), 457-472.

- Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P. C.
  (2019). Rank-normalization, folding, and localization: An improved
  Rhat for assessing convergence of MCMC. arXiv preprint
  arXiv:1903.08008.

## Examples

``` r
interpret_ess(1001)
#> [1] "sufficient"
#> (Rules: burkner2017)
#> 
interpret_ess(c(852, 1200))
#> [1] "insufficient" "sufficient"  
#> (Rules: burkner2017)
#> 

interpret_rhat(1.00)
#> [1] "converged"
#> (Rules: vehtari2019)
#> 
interpret_rhat(c(1.5, 0.9))
#> [1] "failed"    "converged"
#> (Rules: vehtari2019)
#> 
```
