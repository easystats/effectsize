# Convert Between Effect Sizes for Contingency Tables Correlations

Enables a conversion between different indices of effect size, such as
Cohen's *w* to פ (Fei), and Cramer's *V* to Tschuprow's *T*.

## Usage

``` r
w_to_fei(w, p)

w_to_v(w, nrow, ncol)

w_to_t(w, nrow, ncol)

w_to_c(w)

fei_to_w(fei, p)

v_to_w(v, nrow, ncol)

t_to_w(t, nrow, ncol)

c_to_w(c)

v_to_t(v, nrow, ncol)

t_to_v(t, nrow, ncol)
```

## Arguments

- w, c, v, t, fei:

  Effect size to be converted

- p:

  Vector of expected values. See
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

- nrow, ncol:

  The number of rows/columns in the contingency table.

## References

- Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M.,
  Lüdecke, D. (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical
  Data That Use the Chi‑Squared Statistic. Mathematics, 11, 1982.
  [doi:10.3390/math11091982](https://doi.org/10.3390/math11091982)

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

## See also

[`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
[`chisq_to_fei()`](https://easystats.github.io/effectsize/reference/chisq_to_phi.md)

Other convert between effect sizes:
[`d_to_r()`](https://easystats.github.io/effectsize/reference/d_to_r.md),
[`diff_to_cles`](https://easystats.github.io/effectsize/reference/diff_to_cles.md),
[`eta2_to_f2()`](https://easystats.github.io/effectsize/reference/eta2_to_f2.md),
[`odds_to_probs()`](https://easystats.github.io/effectsize/reference/odds_to_probs.md),
[`oddsratio_to_riskratio()`](https://easystats.github.io/effectsize/reference/oddsratio_to_riskratio.md)

## Examples

``` r
library(effectsize)

## 2D tables
## ---------
data("Music_preferences2")
Music_preferences2
#>       Pop Rock Jazz Classic
#> Psych 151  130   12       7
#> Econ   77    6  111       4
#> Law     0    4    2     165

cramers_v(Music_preferences2, adjust = FALSE)
#> Cramer's V |       95% CI
#> -------------------------
#> 0.80       | [0.75, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].

v_to_t(0.80, 3, 4)
#> [1] 0.7228816

tschuprows_t(Music_preferences2)
#> Tschuprow's T (adj.) |       95% CI
#> -----------------------------------
#> 0.72                 | [0.68, 1.00]
#> 
#> - One-sided CIs: upper bound fixed at [1.00].



## Goodness of fit
## ---------------
data("Smoking_FASD")
Smoking_FASD
#>  FAS PFAS   TD 
#>   17   11  640 

cohens_w(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#> Cohen's w |       95% CI
#> ------------------------
#> 0.11      | [0.03, 9.95]
#> 
#> - One-sided CIs: upper bound fixed at [9.95~].

w_to_fei(0.11, p = c(0.015, 0.010, 0.975))
#> [1] 0.01105542

fei(Smoking_FASD, p = c(0.015, 0.010, 0.975))
#> Fei  |       95% CI
#> -------------------
#> 0.01 | [0.00, 1.00]
#> 
#> - Adjusted for uniform expected probabilities.
#> - One-sided CIs: upper bound fixed at [1.00].

## Power analysis
## --------------
# See https://osf.io/cg64s/

p0 <- c(0.35, 0.65)
Fei <- 0.3

pwr::pwr.chisq.test(
  w = fei_to_w(Fei, p = p0),
  df = length(p0) - 1,
  sig.level = 0.01,
  power = 0.85
)
#> 
#>      Chi squared power calculation 
#> 
#>               w = 0.4088311
#>               N = 78.0676
#>              df = 1
#>       sig.level = 0.01
#>           power = 0.85
#> 
#> NOTE: N is the number of observations
#> 
```
