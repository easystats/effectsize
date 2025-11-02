# `effectsize` API

Read the [*Support functions for model
extensions*](https://easystats.github.io/effectsize/articles/effectsize_API.html)
vignette.

## Usage

``` r
.es_aov_simple(
  aov_table,
  type = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE
)

.es_aov_strata(
  aov_table,
  DV_names,
  type = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE
)

.es_aov_table(
  aov_table,
  type = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95,
  alternative = "greater",
  verbose = TRUE
)
```

## Arguments

- aov_table:

  Input data frame

- type:

  Which effect size to compute?

- partial, generalized, ci, alternative, verbose:

  See
  [`eta_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.md).

- include_intercept:

  Should the intercept (`(Intercept)`) be included?

- DV_names:

  A character vector with the names of all the predictors, including the
  grouping variable (e.g., `"Subject"`).
