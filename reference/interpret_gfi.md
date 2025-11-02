# Interpret of CFA / SEM Indices of Goodness of Fit

Interpretation of indices of fit found in confirmatory analysis or
structural equation modelling, such as RMSEA, CFI, NFI, IFI, etc.

## Usage

``` r
interpret_gfi(x, rules = "byrne1994")

interpret_agfi(x, rules = "byrne1994")

interpret_nfi(x, rules = "byrne1994")

interpret_nnfi(x, rules = "byrne1994")

interpret_cfi(x, rules = "byrne1994")

interpret_rfi(x, rules = "default")

interpret_ifi(x, rules = "default")

interpret_pnfi(x, rules = "default")

interpret_rmsea(x, rules = "byrne1994")

interpret_srmr(x, rules = "byrne1994")

# S3 method for class 'lavaan'
interpret(x, ...)

# S3 method for class 'performance_lavaan'
interpret(x, ...)
```

## Arguments

- x:

  vector of values, or an object of class `lavaan`.

- rules:

  Can be the name of a set of rules (see below) or custom set of
  [`rules()`](https://easystats.github.io/effectsize/reference/rules.md).

- ...:

  Currently not used.

## Details

### Indices of fit

- **Chisq**: The model Chi-squared assesses overall fit and the
  discrepancy between the sample and fitted covariance matrices. Its
  p-value should be \> .05 (i.e., the hypothesis of a perfect fit cannot
  be rejected). However, it is quite sensitive to sample size.

- **GFI/AGFI**: The (Adjusted) Goodness of Fit is the proportion of
  variance accounted for by the estimated population covariance.
  Analogous to R2. The GFI and the AGFI should be \> .95 and \> .90,
  respectively (Byrne, 1994; `"byrne1994"`).

- **NFI/NNFI/TLI**: The (Non) Normed Fit Index. An NFI of 0.95,
  indicates the model of interest improves the fit by 95\\ NNFI (also
  called the Tucker Lewis index; TLI) is preferable for smaller samples.
  They should be \> .90 (Byrne, 1994; `"byrne1994"`) or \> .95
  (Schumacker & Lomax, 2004; `"schumacker2004"`).

- **CFI**: The Comparative Fit Index is a revised form of NFI. Not very
  sensitive to sample size (Fan, Thompson, & Wang, 1999). Compares the
  fit of a target model to the fit of an independent, or null, model. It
  should be \> .96 (Hu & Bentler, 1999; `"hu&bentler1999"`) or .90
  (Byrne, 1994; `"byrne1994"`).

- **RFI**: the Relative Fit Index, also known as RHO1, is not guaranteed
  to vary from 0 to 1. However, RFI close to 1 indicates a good fit.

- **IFI**: the Incremental Fit Index (IFI) adjusts the Normed Fit Index
  (NFI) for sample size and degrees of freedom (Bollen's, 1989). Over
  0.90 is a good fit, but the index can exceed 1.

- **PNFI**: the Parsimony-Adjusted Measures Index. There is no commonly
  agreed-upon cutoff value for an acceptable model for this index.
  Should be \> 0.50.

- **RMSEA**: The Root Mean Square Error of Approximation is a
  parsimony-adjusted index. Values closer to 0 represent a good fit. It
  should be \< .08 (Awang, 2012; `"awang2012"`) or \< .05 (Byrne, 1994;
  `"byrne1994"`). The p-value printed with it tests the hypothesis that
  RMSEA is less than or equal to .05 (a cutoff sometimes used for good
  fit), and thus should be not significant.

- **RMR/SRMR**: the (Standardized) Root Mean Square Residual represents
  the square-root of the difference between the residuals of the sample
  covariance matrix and the hypothesized model. As the RMR can be
  sometimes hard to interpret, better to use SRMR. Should be \< .08
  (Byrne, 1994; `"byrne1994"`).

See the documentation for
[`fitmeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html).

### What to report

For structural equation models (SEM), Kline (2015) suggests that at a
minimum the following indices should be reported: The model
**chi-square**, the **RMSEA**, the **CFI** and the **SRMR**.

## Note

When possible, it is recommended to report dynamic cutoffs of fit
indices. See https://dynamicfit.app/cfa/.

## References

- Awang, Z. (2012). A handbook on SEM. Structural equation modeling.

- Byrne, B. M. (1994). Structural equation modeling with EQS and
  EQS/Windows. Thousand Oaks, CA: Sage Publications.

- Fan, X., B. Thompson, and L. Wang (1999). Effects of sample size,
  estimation method, and model specification on structural equation
  modeling fit indexes. Structural Equation Modeling, 6, 56-83.

- Hu, L. T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
  covariance structure analysis: Conventional criteria versus new
  alternatives. Structural equation modeling: a multidisciplinary
  journal, 6(1), 1-55.

- Kline, R. B. (2015). Principles and practice of structural equation
  modeling. Guilford publications.

- Schumacker, R. E., and Lomax, R. G. (2004). A beginner's guide to
  structural equation modeling, Second edition. Mahwah, NJ: Lawrence
  Erlbaum Associates.

- Tucker, L. R., and Lewis, C. (1973). The reliability coefficient for
  maximum likelihood factor analysis. Psychometrika, 38, 1-10.

## Examples

``` r
interpret_gfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_agfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_nfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_nnfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_cfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_rmsea(c(.07, .04))
#> [1] "poor"         "satisfactory"
#> (Rules: byrne1994)
#> 
interpret_srmr(c(.5, .99))
#> [1] "poor" "poor"
#> (Rules: byrne1994)
#> 
interpret_rfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: default)
#> 
interpret_ifi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: default)
#> 
interpret_pnfi(c(.5, .99))
#> [1] "poor"         "satisfactory"
#> (Rules: default)
#> 

if (FALSE) { # require("lavaan") && interactive()
# Structural Equation Models (SEM)
structure <- " ind60 =~ x1 + x2 + x3
               dem60 =~ y1 + y2 + y3
               dem60 ~ ind60 "

model <- lavaan::sem(structure, data = lavaan::PoliticalDemocracy)

interpret(model)
}
```
