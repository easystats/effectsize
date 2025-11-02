# Interpret Correlation Coefficient

Interpret Correlation Coefficient

## Usage

``` r
interpret_r(r, rules = "funder2019", ...)

interpret_phi(r, rules = "funder2019", ...)

interpret_cramers_v(r, rules = "funder2019", ...)

interpret_rank_biserial(r, rules = "funder2019", ...)

interpret_fei(r, rules = "funder2019", ...)
```

## Arguments

- r:

  Value or vector of correlation coefficient.

- rules:

  Can be `"funder2019"` (default), `"gignac2016"`, `"cohen1988"`,
  `"evans1996"`, `"lovakov2021"` or a custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Not directly used.

## Details

Since Cohen's *w* does not have a fixed upper bound, for all by the most
simple of cases (2-by-2 or 1-by-2 tables), interpreting Cohen's *w* as a
correlation coefficient is inappropriate (Ben-Shachar, et al., 2024;
Cohen, 1988, p. 222). Please us
[`cramers_v()`](https://easystats.github.io/effectsize/reference/phi.md)
of the like instead.

## Note

As \\\phi\\ can be larger than 1 - it is recommended to compute and
interpret Cramer's *V* instead.

## Rules

Rules apply to positive and negative *r* alike.

- Funder & Ozer (2019) (`"funder2019"`; default)

  - **r \< 0.05** - Tiny

  - **0.05 \<= r \< 0.1** - Very small

  - **0.1 \<= r \< 0.2** - Small

  - **0.2 \<= r \< 0.3** - Medium

  - **0.3 \<= r \< 0.4** - Large

  - **r \>= 0.4** - Very large

- Gignac & Szodorai (2016) (`"gignac2016"`)

  - **r \< 0.1** - Very small

  - **0.1 \<= r \< 0.2** - Small

  - **0.2 \<= r \< 0.3** - Moderate

  - **r \>= 0.3** - Large

- Cohen (1988) (`"cohen1988"`)

  - **r \< 0.1** - Very small

  - **0.1 \<= r \< 0.3** - Small

  - **0.3 \<= r \< 0.5** - Moderate

  - **r \>= 0.5** - Large

- Lovakov & Agadullina (2021) (`"lovakov2021"`)

  - **r \< 0.12** - Very small

  - **0.12 \<= r \< 0.24** - Small

  - **0.24 \<= r \< 0.41** - Moderate

  - **r \>= 0.41** - Large

- Evans (1996) (`"evans1996"`)

  - **r \< 0.2** - Very weak

  - **0.2 \<= r \< 0.4** - Weak

  - **0.4 \<= r \< 0.6** - Moderate

  - **0.6 \<= r \< 0.8** - Strong

  - **r \>= 0.8** - Very strong

## References

- Lovakov, A., & Agadullina, E. R. (2021). Empirically Derived
  Guidelines for Effect Size Interpretation in Social Psychology.
  European Journal of Social Psychology.

- Funder, D. C., & Ozer, D. J. (2019). Evaluating effect size in
  psychological research: sense and nonsense. Advances in Methods and
  Practices in Psychological Science.

- Gignac, G. E., & Szodorai, E. T. (2016). Effect size guidelines for
  individual differences researchers. Personality and individual
  differences, 102, 74-78.

- Cohen, J. (1988). Statistical power analysis for the behavioral
  sciences (2nd Ed.). New York: Routledge.

- Evans, J. D. (1996). Straightforward statistics for the behavioral
  sciences. Thomson Brooks/Cole Publishing Co.

- Ben-Shachar, M.S., Patil, I., Thériault, R., Wiernik, B.M.,
  Lüdecke, D. (2023). Phi, Fei, Fo, Fum: Effect Sizes for Categorical
  Data That Use the Chi‑Squared Statistic. Mathematics, 11, 1982.
  [doi:10.3390/math11091982](https://doi.org/10.3390/math11091982)

## See also

Page 88 of APA's 6th Edition.

## Examples

``` r
interpret_r(.015)
#> [1] "tiny"
#> (Rules: funder2019)
#> 
interpret_r(c(.5, -.02))
#> [1] "very large" "tiny"      
#> (Rules: funder2019)
#> 
interpret_r(.3, rules = "lovakov2021")
#> [1] "medium"
#> (Rules: lovakov2021)
#> 
```
