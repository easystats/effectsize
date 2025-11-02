# Convert Between Odds and Probabilities

Convert Between Odds and Probabilities

## Usage

``` r
odds_to_probs(odds, log = FALSE, ...)

probs_to_odds(probs, log = FALSE, ...)
```

## Arguments

- odds:

  The *Odds* (or `log(odds)` when `log = TRUE`) to convert.

- log:

  Take in or output log odds (such as in logistic models).

- ...:

  Arguments passed to or from other methods.

- probs:

  Probability values to convert.

## Value

Converted index.

## See also

[`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html)

Other convert between effect sizes:
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
[`diff_to_cles`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
[`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md),
[`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
[`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)

## Examples

``` r
odds_to_probs(3)
#> [1] 0.75
odds_to_probs(1.09, log = TRUE)
#> [1] 0.7483817

probs_to_odds(0.95)
#> [1] 19
probs_to_odds(0.95, log = TRUE)
#> [1] 2.944439
```
