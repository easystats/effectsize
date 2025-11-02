# Support Functions for Model Extensions

``` r

library(effectsize)
```

### Supporting ANOVA Effect Sizes

To add support for you model, create a new `.anova_es()` method
function. This functions should generally do 3 things:

1.  Build a data frame with all the required information.
2.  Pass the data frame to one of the 3 functions.
3.  Set some attributes to the output.

#### Simple ANOVA tables

The input data frame must have these columns: - `Parameter` (char) - The
name of the parameter or, more often, the term. - `Sum_Squares` (num) -
The sum of squares. - `df` (num) - The degrees of freedom associated
with the `Sum_Squares`. - `Mean_Square_residuals` (num; *optional*) - if
*not* present, is calculated as `Sum_Squares / df`. (Any other column is
ignored.)

And exactly *1* row Where `Parameter` is `Residual`.

Optionally, one of the rows can have a `(Intercept)` value for
`Parameter`.

An example of a minimally valid data frame:

``` r

min_aov <- data.frame(
  Parameter = c("(Intercept)", "A", "B", "Residuals"),
  Sum_Squares = c(30, 40, 10, 100),
  df = c(1, 1, 2, 50)
)
```

Pass the data frame to
[`.es_aov_simple()`](https://easystats.github.io/effectsize/reference/effectsize_API.md):

``` r

.es_aov_simple(
  min_aov,
  type = "eta", partial = TRUE, generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95, alternative = "greater",
  verbose = TRUE
)
```

    >   Parameter Eta2_partial   CI CI_low CI_high
    > 1         A        0.286 0.95   0.12       1
    > 2         B        0.091 0.95   0.00       1

The output is a data frame with the columns: `Parameter`, the effect
size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`anova_type` (`NA` or `NULL`), `approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output.

#### ANOVA Tables with Multiple Error Strata

(e.g., `aovlist` models.)

The input data frame must have these columns:

- `Group` (char) - The strata
- `Parameter` (char)
- `Sum_Squares` (num)
- `df` (num)
- `Mean_Square_residuals` (num; *optional*)

And exactly *1* row ***per `Group`*** Where `Parameter` is `Residual`.

Optionally, one of the rows can have a `(Intercept)` value for
`Parameter`.

An example of a minimally valid data frame:

``` r

min_aovlist <- data.frame(
  Group = c("S", "S", "S:A", "S:A"),
  Parameter = c("(Intercept)", "Residuals", "A", "Residuals"),
  Sum_Squares = c(34, 21, 34, 400),
  df = c(1, 12, 4, 30)
)
```

Pass the data frame to
[`.es_aov_strata()`](https://easystats.github.io/effectsize/reference/effectsize_API.md),
along with a list of predictors (including the stratifying variables) to
the `DV_names` argument:

``` r

.es_aov_strata(
  min_aovlist,
  DV_names = c("S", "A"),
  type = "omega", partial = TRUE, generalized = FALSE,
  ci = 0.95, alternative = "greater",
  verbose = TRUE,
  include_intercept = TRUE
)
```

    >   Group   Parameter Omega2_partial   CI CI_low CI_high
    > 1     S (Intercept)           0.57 0.95   0.21       1
    > 2   S:A           A           0.00 0.95   0.00       1

The output is a data frame with the columns: `Group`, `Parameter`, the
effect size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output.

#### Approximate Effect sizes

When *sums of squares* cannot be extracted, we can still get
*approximate* effect sizes based on the
[`F_to_eta2()`](https://easystats.github.io/effectsize/reference/F_to_eta2.md)
family of functions.

The input data frame must have these columns:

- `Parameter` (char)
- `F` (num) - The *F* test statistic.
- `df` (num) - effect degrees of freedom.
- (Can also have a `t` col instead, in which case `df` is set to 1, and
  `F` is `t^2`).
- `df_error` (num) - error degrees of freedom.

Optionally, one of the rows can have `(Intercept)` as the `Parameter`.

An example of a minimally valid data frame:

``` r

min_anova <- data.frame(
  Parameter = c("(Intercept)", "A", "B"),
  F = c(4, 7, 0.7),
  df = c(1, 1, 2),
  df_error = 34
)
```

Pass the table to
[`.es_aov_table()`](https://easystats.github.io/effectsize/reference/effectsize_API.md):

``` r

.es_aov_table(
  min_anova,
  type = "eta", partial = TRUE, generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95, alternative = "greater",
  verbose = TRUE
)
```

    >   Parameter Eta2_partial   CI CI_low CI_high
    > 1         A         0.17 0.95  0.023       1
    > 2         B         0.04 0.95  0.000       1

The output is a data frame with the columns: `Parameter`, the effect
size, and (optionally) `CI` + `CI_low` + `CI_high`,

And with the following attributes: `generalized`, `ci`, `alternative`,
`approximate`.

You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and
return the output, and optionally the `approximate` attribute, and
return the output.

#### *Example*

Let’s fit a simple linear model and change its class:

``` r

mod <- lm(mpg ~ factor(cyl) + am, mtcars)

class(mod) <- "superMODEL"
```

We now need a new `.anova_es.superMODEL` function:

``` r

.anova_es.superMODEL <- function(model, ...) {
  # Get ANOVA table
  anov <- suppressWarnings(stats:::anova.lm(model))
  anov <- as.data.frame(anov)

  # Clean up
  anov[["Parameter"]] <- rownames(anov)
  colnames(anov)[2:1] <- c("Sum_Squares", "df")

  # Pass
  out <- .es_aov_simple(anov, ...)

  # Set attribute
  attr(out, "anova_type") <- 1

  out
}
```

And… that’s it! Our new `superMODEL` class of models is fully supported!

``` r

eta_squared(mod)
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Eta2 (partial) |       95% CI
    > -------------------------------------------
    > factor(cyl) |           0.76 | [0.61, 1.00]
    > am          |           0.12 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

eta_squared(mod, partial = FALSE)
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Eta2 |       95% CI
    > ---------------------------------
    > factor(cyl) | 0.73 | [0.57, 1.00]
    > am          | 0.03 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

omega_squared(mod)
```

    > # Effect Size for ANOVA (Type I)
    > 
    > Parameter   | Omega2 (partial) |       95% CI
    > ---------------------------------------------
    > factor(cyl) |             0.73 | [0.56, 1.00]
    > am          |             0.08 | [0.00, 1.00]
    > 
    > - One-sided CIs: upper bound fixed at [1.00].

``` r

# Etc...
```

## References
