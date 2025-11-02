# Interpret Bayes Factor (BF)

Interpret Bayes Factor (BF)

## Usage

``` r
interpret_bf(
  bf,
  rules = "jeffreys1961",
  log = FALSE,
  include_value = FALSE,
  protect_ratio = TRUE,
  exact = TRUE
)
```

## Arguments

- bf:

  Value or vector of Bayes factor (BF) values.

- rules:

  Can be `"jeffreys1961"` (default), `"raftery1995"` or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md)
  (for the *absolute magnitude* of evidence).

- log:

  Is the `bf` value `log(bf)`?

- include_value:

  Include the value in the output.

- protect_ratio:

  Should values smaller than 1 be represented as ratios?

- exact:

  Should very large or very small values be reported with a scientific
  format (e.g., 4.24e5), or as truncated values (as "\> 1000" and "\<
  1/1000").

## Details

Argument names can be partially matched.

## Rules

Rules apply to BF as ratios, so BF of 10 is as extreme as a BF of 0.1
(1/10).

- Jeffreys (1961) (`"jeffreys1961"`; default)

  - **BF = 1** - No evidence

  - **1 \< BF \<= 3** - Anecdotal

  - **3 \< BF \<= 10** - Moderate

  - **10 \< BF \<= 30** - Strong

  - **30 \< BF \<= 100** - Very strong

  - **BF \> 100** - Extreme.

- Raftery (1995) (`"raftery1995"`)

  - **BF = 1** - No evidence

  - **1 \< BF \<= 3** - Weak

  - **3 \< BF \<= 20** - Positive

  - **20 \< BF \<= 150** - Strong

  - **BF \> 150** - Very strong

## References

- Jeffreys, H. (1961), Theory of Probability, 3rd ed., Oxford University
  Press, Oxford.

- Raftery, A. E. (1995). Bayesian model selection in social research.
  Sociological methodology, 25, 111-164.

- Jarosz, A. F., & Wiley, J. (2014). What are the odds? A practical
  guide to computing and reporting Bayes factors. The Journal of Problem
  Solving, 7(1), 2.

## Examples

``` r
interpret_bf(1)
#> [1] "no evidence against or in favour of"
#> (Rules: jeffreys1961)
#> 
interpret_bf(c(5, 2, 0.01))
#> [1] "moderate evidence in favour of"  "anecdotal evidence in favour of"
#> [3] "very strong evidence against"   
#> (Rules: jeffreys1961)
#> 
```
