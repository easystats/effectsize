# Convert Between Metrics of Change in Probabilities and Probabilities

Convert Between Metrics of Change in Probabilities and Probabilities

## Usage

``` r
oddsratio_to_probs(OR, p0, log = FALSE, odds = FALSE, ...)

logoddsratio_to_probs(logOR, p0, log = TRUE, odds = FALSE, ...)

riskratio_to_probs(RR, p0, odds = FALSE, ...)

arr_to_probs(ARR, p0, odds = FALSE, ...)

nnt_to_probs(NNT, p0, odds = FALSE, ...)
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

- odds:

  Should odds be returned instead of probabilities?

- ...:

  Arguments passed to and from other methods.

## Value

Probabilities (or probability odds).

## See also

[`oddsratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`arr()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
and
[`nnt()`](https://easystats.github.io/effectsize/reference/oddsratio.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
and
[`oddsratio_to_arr()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)
and others.

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

riskratio_to_probs(RR, p0 = p0)
#> [1] 0.7
oddsratio_to_probs(OR, p0 = p0)
#> [1] 0.7

all.equal(nnt_to_probs(NNT, p0 = p0, odds = TRUE),
          probs_to_odds(p1))
#> [1] TRUE

arr_to_probs(-ARR, p0 = p1)
#> [1] 0.4
nnt_to_probs(-NNT, p0 = p1)
#> [1] 0.4



# RR |>
#   riskratio_to_arr(p0) |>
#   arr_to_oddsratio(p0) |>
#   oddsratio_to_nnt(p0) |>
#   nnt_to_probs(p0)
```
