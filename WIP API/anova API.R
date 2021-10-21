


# AOVLIST ----


#' Build an `.anova_es()` function that cleans up your model and makes a minimal
#' table with these columns:
#' - Group (char)
#' - Parameter (char)
#' - Sum_Squares (num)
#' - df (num)
#' And *1* row ***per group*** Where `Parameter` is `Residual`.
#' Optionally, a row where `Parameter` is `(Intercept)`.
#'
#' eg:
min_aovlist <- data.frame(
  Group = c("A", "A", "B", "B"),
  Parameter = c("(Intercept)", "Residuals", "D", "Residuals"),
  Sum_Squares = c(34, 21, 34, 400),
  df = c(1, 12, 4, 30)
)
#' Any other column is ignored.
#'
#' Optional columns:
#' - "Mean_Square_residuals" - if *not* present, is calculated as Sum_Squares / df
#'
#' Pass the table to `.es_aovlist()`: {FIX - export this function}
effectsize:::.es_aovlist(
  min_aovlist, c("D"),
  type = c("eta", "omega", "epsilon"),
  partial = TRUE,
  generalized = FALSE,
  ci = 0.95, alternative = "greater",
  verbose = TRUE,
  include_intercept = FALSE
)
#' The output is a data.frame with
#' - Group
#' - Parameter
#' - {effect size}
#' - Optional: CI + CI_low + CI_high
#'
#' And with the following attributes:
#' - partial
#' - generalized
#' - ci
#' - alternative
#' - approximate
#' - anova_type (NULL)
#'
#'
#' You should then set the `anova_type` attribute to {1,2,3,NULL}, and
#' optionally the `approximate` attribute, and return the output.
#' BAM!



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

# parameters_model --------------------------------------------------------

