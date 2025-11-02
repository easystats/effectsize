# Convert Standardized Differences to Common Language Effect Sizes

Convert Standardized Differences to Common Language Effect Sizes

## Usage

``` r
d_to_p_superiority(d)

rb_to_p_superiority(rb)

rb_to_vda(rb)

d_to_u2(d)

d_to_u1(d)

d_to_u3(d)

d_to_overlap(d)

rb_to_wmw_odds(rb)
```

## Arguments

- d, rb:

  A numeric vector of Cohen's d / rank-biserial correlation *or* the
  output from
  [`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)
  /
  [`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md).

## Value

A list of `Cohen's U3`, `Overlap`, `Pr(superiority)`, a numeric vector
of `Pr(superiority)`, or a data frame, depending on the input.

## Details

This function use the following formulae for Cohen's *d*:
\$\$Pr(superiority) = \Phi(d/\sqrt{2})\$\$  
\$\$\textrm{Cohen's } U_3 = \Phi(d)\$\$  
\$\$\textrm{Cohen's } U_2 = \Phi(\|d\|/2)\$\$  
\$\$\textrm{Cohen's } U_1 = (2\times U_2 - 1)/U_2\$\$  
\$\$Overlap = 2 \times \Phi(-\|d\|/2)\$\$  
And the following for the rank-biserial correlation: \$\$Pr(superiority)
= (r\_{rb} + 1)/2\$\$  
\\WMW\_{Odds} = Pr(superiority) / (1 - Pr(superiority))\\

## Note

For *d*, these calculations assume that the populations have equal
variance and are normally distributed.

Vargha and Delaney's *A* is an alias for the non-parametric *probability
of superiority*.

## References

- Cohen, J. (1977). Statistical power analysis for the behavioral
  sciences. New York: Routledge.

- Reiser, B., & Faraggi, D. (1999). Confidence intervals for the
  overlapping coefficient: the normal equal variance case. Journal of
  the Royal Statistical Society, 48(3), 413-418.

- Ruscio, J. (2008). A probability-based measure of effect size:
  robustness to base rates and other factors. Psychological methods,
  13(1), 19–30.

## See also

[`cohens_u3()`](https://easystats.github.io/effectsize/reference/p_superiority.md)
for descriptions of the effect sizes (also,
[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md),
[`rank_biserial()`](https://easystats.github.io/effectsize/reference/rank_biserial.md)).

Other convert between effect sizes:
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
[`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
[`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
[`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)
