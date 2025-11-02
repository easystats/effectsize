# Convert Between ANOVA Effect Sizes

Convert Between ANOVA Effect Sizes

## Usage

``` r
eta2_to_f2(es)

eta2_to_f(es)

f2_to_eta2(f2)

f_to_eta2(f)
```

## Arguments

- es:

  Any measure of variance explained such as Eta-, Epsilon-, Omega-, or
  R-Squared, partial or otherwise. See details.

- f, f2:

  Cohen's *f* or *f*-squared.

## Details

Any measure of variance explained can be converted to a corresponding
Cohen's *f* via:  
  
\$\$f^2 = \frac{\eta^2}{1 - \eta^2}\$\$  
  
\$\$\eta^2 = \frac{f^2}{1 + f^2}\$\$  
  
If a partial Eta-Squared is used, the resulting Cohen's *f* is a
partial-Cohen's *f*; If a less biased estimate of variance explained is
used (such as Epsilon- or Omega-Squared), the resulting Cohen's *f* is
likewise a less biased estimate of Cohen's *f*.

## References

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Steiger, J. H. (2004). Beyond the F test: Effect size confidence
  intervals and tests of close fit in the analysis of variance and
  contrast analysis. Psychological Methods, 9, 164-182.

## See also

[`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md)
for more details.

Other convert between effect sizes:
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
[`diff_to_cles`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
[`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
[`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
