# AOV ----

#' Build an `.anova_es()` function that cleans up your model and makes a minimal
#' table with these columns:
#' - Parameter (char)
#' - Sum_Squares (num)
#' - df (num)
#' And *1* row Where `Parameter` is `Residual`
#' Optionally, a row where `Parameter` is `(Intercept)`
#'
#' eg:
min_aov <- data.frame(
  Parameter = c("(Intercept)","A", "B", "Residuals"),
  Sum_Squares = c(30, 40, 10, 100),
  df = c(1,1,2,50)
)
#' Any other column is ignored
#' A "Group" column is not allowed {FIX - ignore}
#'
#' Optional columns:
#' - "Response" - effect is estimated seperetly for each response (so the Residuals / must appear for each response) {FIX - do}
#' - "Mean_Square_residuals" - if *not* present, is calculated as Sum_Squares / df
#'
#' Pass the table to `.es_aov()`: {FIX - export this function}
effectsize:::.es_aov(
  params = min_aov,
  type = "eta",
  partial = FALSE,
  generalized = FALSE,
  include_intercept = FALSE,
  ci = 0.95, alternative = "greater",
  verbose = TRUE
)
#' The output is a data.frame with
#' - Parameter
#' - {effect size}
#' - Optional: CI + CI_low + CI_high
#'
#' And with the following attributes:
#' - partial
#' - generalized
#' - anova_type
#' - ci
#' - alternative
#' - approximate
#'
#'
#' You must then set the `anova_type` attribute to {1,2,3,NULL} return the
#' output.
#' BAM!


# AOVLIST ----


# ANOVA table w/o SS ----
