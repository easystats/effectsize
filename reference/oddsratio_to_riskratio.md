# Convert Between Odds Ratios, Risk Ratios and Other Metrics of Change in Probabilities

Convert Between Odds Ratios, Risk Ratios and Other Metrics of Change in
Probabilities

## Usage

``` r
oddsratio_to_riskratio(OR, p0, log = FALSE, verbose = TRUE, ...)

oddsratio_to_arr(OR, p0, log = FALSE, verbose = TRUE, ...)

oddsratio_to_nnt(OR, p0, log = FALSE, verbose = TRUE, ...)

logoddsratio_to_riskratio(logOR, p0, log = TRUE, verbose = TRUE, ...)

logoddsratio_to_arr(logOR, p0, log = TRUE, verbose = TRUE, ...)

logoddsratio_to_nnt(logOR, p0, log = TRUE, verbose = TRUE, ...)

riskratio_to_oddsratio(RR, p0, log = FALSE, verbose = TRUE, ...)

riskratio_to_arr(RR, p0, verbose = TRUE, ...)

riskratio_to_logoddsratio(RR, p0, log = TRUE, verbose = TRUE, ...)

riskratio_to_nnt(RR, p0, verbose = TRUE, ...)

arr_to_riskratio(ARR, p0, verbose = TRUE, ...)

arr_to_oddsratio(ARR, p0, log = FALSE, verbose = TRUE, ...)

arr_to_logoddsratio(ARR, p0, log = TRUE, verbose = TRUE, ...)

arr_to_nnt(ARR, ...)

nnt_to_oddsratio(NNT, p0, log = FALSE, verbose = TRUE, ...)

nnt_to_logoddsratio(NNT, p0, log = TRUE, verbose = TRUE, ...)

nnt_to_riskratio(NNT, p0, verbose = TRUE, ...)

nnt_to_arr(NNT, ...)
```

## Arguments

- OR, logOR, RR, ARR, NNT:

  Odds-ratio of `odds(p1)/odds(p0)`, log-Odds-ratio of
  `log(odds(p1)/odds(p0))`, Risk ratio of `p1/p0`, Absolute Risk
  Reduction of `p1 - p0`, or Number-needed-to-treat of `1/(p1 - p0)`.
  `OR` and `logOR` can also be a logistic regression model.

- p0:

  Baseline risk

- log:

  If:

  - `TRUE`:

    - In `oddsratio_to_*()`, `OR` input is treated as `log(OR)`.

    - In `*_to_oddsratio()`, returned value is `log(OR)`.

  - `FALSE`:

    - In `logoddsratio_to_*()`, `logOR` input is treated as `OR`.

    - In `*_to_logoddsratio()`, returned value is `OR`.

- verbose:

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to and from other methods.

## Value

Converted index, or if `OR`/`logOR` is a logistic regression model, a
parameter table with the converted indices.

## Details

If an impossible combination of risk ratio `RR` and baseline risk `p0`
is provided, such that `RR * p0 > 1`, then this conversion will produce
invalid results where the expected risk is larger than 1. In such cases
`riskratio_to_*()` functions will return `NA`.

## References

Grant, R. L. (2014). Converting an odds ratio to a range of plausible
relative risks for better communication of research findings. Bmj, 348,
f7450.

## See also

[`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`arr()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
and
[`nnt()`](https://easystats.github.io/effectsize/reference/oddsratio.md).

Other convert between effect sizes:
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
[`diff_to_cles`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
[`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
[`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)

## Examples

``` r
p0 <- 0.4
p1 <- 0.7

(OR <- probs_to_odds(p1) / probs_to_odds(p0))
#> [1] 3.5
(RR <- p1 / p0)
#> [1] 1.75
(ARR <- p1 - p0)
#> [1] 0.3
(NNT <- arr_to_nnt(ARR))
#> [1] 3.333333

riskratio_to_oddsratio(RR, p0 = p0)
#> [1] 3.5
oddsratio_to_riskratio(OR, p0 = p0)
#> [1] 1.75
riskratio_to_arr(RR, p0 = p0)
#> [1] 0.3
arr_to_oddsratio(nnt_to_arr(NNT), p0 = p0)
#> [1] 3.5

m <- glm(am ~ factor(cyl),
  data = mtcars,
  family = binomial()
)
oddsratio_to_riskratio(m, verbose = FALSE) # RR is relative to the intercept if p0 not provided
#> Parameter | Risk ratio |       95% CI
#> -------------------------------------
#> (p0)      |       0.73 |             
#> cyl [6]   |       0.59 | [0.11, 1.16]
#> cyl [8]   |       0.20 | [0.02, 0.70]
#> 
#> Uncertainty intervals (profile-likelihood) and p-values
#>   (two-tailed) computed using a Wald z-distribution approximation.
```
