# Interpret Odds Ratio

Interpret Odds Ratio

## Usage

``` r
interpret_oddsratio(OR, rules = "cohen1988", p0 = NULL, log = FALSE, ...)
```

## Arguments

- OR:

  Value or vector of (log) odds ratio values.

- rules:

  If `"cohen1988"` (default), `OR` is transformed to a standardized
  difference (via
  [`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md))
  and interpreted according to Cohen's rules (see
  [`interpret_cohens_d()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md);
  see Chen et al., 2010). If a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md)
  is used, OR is interpreted as is.

- p0:

  Baseline risk. If not specified, the *d* to *OR* conversion uses am
  approximation (see details).

- log:

  Are the provided values log odds ratio.

- ...:

  Currently not used.

## Rules

Rules apply to OR as ratios, so OR of 10 is as extreme as a OR of 0.1
(1/10).

- Cohen (1988) (`"cohen1988"`, based on the
  [`oddsratio_to_d()`](https://easystats.github.io/effectsize/reference/d_to_r.md)
  conversion, see
  [`interpret_cohens_d()`](https://easystats.github.io/effectsize/reference/interpret_cohens_d.md))

  - **OR \< 1.44** - Very small

  - **1.44 \<= OR \< 2.48** - Small

  - **2.48 \<= OR \< 4.27** - Medium

  - **OR \>= 4.27** - Large

## References

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Chen, H., Cohen, P., & Chen, S. (2010). How big is a big odds ratio?
  Interpreting the magnitudes of odds ratios in epidemiological studies.
  Communications in Statistics-Simulation and Computation, 39(4),
  860-864.

- Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
  Effect-size indices for dichotomized outcomes in meta-analysis.
  Psychological methods, 8(4), 448.

## Examples

``` r
interpret_oddsratio(1)
#> [1] "very small"
#> (Rules: cohen1988)
#> 
interpret_oddsratio(c(5, 2))
#> [1] "large" "small"
#> (Rules: cohen1988)
#> 
```
