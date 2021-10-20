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
#' - "Response" - effect is estimated separately for each response (so the Residuals / must appear for each response) {FIX - do}
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
#' Build an `.anova_es()` function that cleans up your model and makes a minimal
#' table with these columns:
#' - Parameter (char)
#' - F (num)
#' - df (num)
#' (Can also have a "t" col instead.)
#' - df_error (num)
#' Optionally, a row where `Parameter` is `(Intercept)`
#'
#' eg:
min_anova <- data.frame(
  Parameter = c("(Intercept)","A", "B"),
  F = c(4, 7, 0.7),
  df = c(1,1,2),
  df_error = 34
)
#' Any other column is ignored
#'
#'
#' Pass the table to `.es_anova()`: {FIX - export this function}
effectsize:::.es_anova(
  params = min_anova,
  type = "eta",
  partial = TRUE,
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
#' - ci
#' - alternative
#' - approximate (NULL)
#' - anova_type (NULL)
#'
#'
#' You should then set the `anova_type` attribute to {1,2,3,NULL}, and
#' optionally the `approximate` attribute, and return the output.
#' BAM!
#'
#'

# parameters_model --------------------------------------------------------

