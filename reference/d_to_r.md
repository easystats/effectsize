# Convert Between *d*, *r*, and Odds Ratio

Enables a conversion between different indices of effect size, such as
standardized difference (Cohen's d), (point-biserial) correlation r or
(log) odds ratios.

## Usage

``` r
d_to_r(d, n1, n2, ...)

r_to_d(r, n1, n2, ...)

oddsratio_to_d(OR, p0, log = FALSE, ...)

logoddsratio_to_d(logOR, p0, log = TRUE, ...)

d_to_oddsratio(d, log = FALSE, ...)

d_to_logoddsratio(d, log = TRUE, ...)

oddsratio_to_r(OR, p0, n1, n2, log = FALSE, ...)

logoddsratio_to_r(logOR, p0, n1, n2, log = TRUE, ...)

r_to_oddsratio(r, n1, n2, log = FALSE, ...)

r_to_logoddsratio(r, n1, n2, log = TRUE, ...)
```

## Arguments

- d, r, OR, logOR:

  Standardized difference value (Cohen's d), correlation coefficient
  (r), Odds ratio, or logged Odds ratio.

- n1, n2:

  Group sample sizes. If either is missing, groups are assumed to be of
  equal size.

- ...:

  Arguments passed to or from other methods.

- p0:

  Baseline risk. If not specified, the *d* to *OR* conversion uses am
  approximation (see details).

- log:

  Take in or output the log of the ratio (such as in logistic models),
  e.g. when the desired input or output are log odds ratios instead odds
  ratios.

## Value

Converted index.

## Details

Conversions between *d* and *OR* is done through these formulae:

- \\d = \frac{\log(OR)\times\sqrt{3}}{\pi}\\

- \\log(OR) = d \* \frac{\pi}{\sqrt(3)}\\

Converting between *d* and *r* is done through these formulae:

- \\d = \frac{\sqrt{h} \* r}{\sqrt{1 - r^2}}\\

- \\r = \frac{d}{\sqrt{d^2 + h}}\\

Where \\h = \frac{n_1 + n_2 - 2}{n_1} + \frac{n_1 + n_2 - 2}{n_2}\\.
When groups are of equal size, *h* reduces to approximately 4. The
resulting *r* is also called the binomial effect size display (BESD;
Rosenthal et al., 1982).

## References

- Borenstein, M., Hedges, L. V., Higgins, J. P. T., & Rothstein, H. R.
  (2009). Converting among effect sizes. Introduction to meta-analysis,
  45-49.

- Jacobs, P., & Viechtbauer, W. (2017). Estimation of the biserial
  correlation and its sampling variance for use in meta-analysis.
  Research synthesis methods, 8(2), 161-180.
  [doi:10.1002/jrsm.1218](https://doi.org/10.1002/jrsm.1218)

- Rosenthal, R., & Rubin, D. B. (1982). A simple, general purpose
  display of magnitude of experimental effect. Journal of educational
  psychology, 74(2), 166.

- Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
  Effect-size indices for dichotomized outcomes in meta-analysis.
  Psychological methods, 8(4), 448.

## See also

[`cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.md)

Other convert between effect sizes:
[`diff_to_cles`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
[`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
[`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md),
[`w_to_fei()`](https://easystats.github.io/effectsize/reference/w_to_fei.md)

## Examples

``` r
r_to_d(0.5)
#> [1] 1.154701
d_to_oddsratio(1.154701)
#> [1] 8.120534
oddsratio_to_r(8.120534)
#> [1] 0.5000001

d_to_r(1)
#> [1] 0.4472136
r_to_oddsratio(0.4472136, log = TRUE)
#> [1] 1.813799
oddsratio_to_d(1.813799, log = TRUE)
#> [1] 0.9999998
```
