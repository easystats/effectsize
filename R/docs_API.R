#' `effectsize` API
#'
#' @section Adding support for ANOVA effect sizes (`eta_squared()` functions):
#'
#' To add support for you model, create a new `.anova_es()` method function.
#' This functions should generally do 3 things:
#' 1. Build a data frame with all the required information.
#' 2. Pass the data frame to one of the 3 functions.
#' 3. Set some attributes to the output.
#'
#' ## `.es_aov()` for simple ANOVA tables
#'
#' The input data frame must have these columns:
#' - `Parameter` (char) - The name of the parameter or, more often, the term.
#' - `Sum_Squares` (num) - The sum of squares.
#' - `df` (num) - The degrees of freedom associated with the `Sum_Squares`.
#' - `Mean_Square_residuals` (num; optional) - if *not* present, is calculated as `Sum_Squares / df`.
#' (Any other column is ignored.)
#'
#' And exactly *1* row Where `Parameter` is `Residual`.
#'
#' Optionally, one of the rows can have a `(Intercept)` value for `Parameter`.
#'
#' E.g. of a minimally valid data.frame:
#' ```{r}
#' min_aov <- data.frame(
#'   Parameter = c("(Intercept)", "A", "B", "Residuals"),
#'   Sum_Squares = c(30, 40, 10, 100),
#'   df = c(1, 1, 2, 50)
#' )
#' ```
#'
#' Pass the data frame to `.es_aov()`:
#' ```{r}
#' .es_aov(
#'   min_aov,
#'   type = "eta",
#'   partial = TRUE,
#'   generalized = FALSE,
#'   include_intercept = FALSE,
#'   ci = 0.95, alternative = "greater",
#'   verbose = TRUE
#' )
#' ```
#'
#' The output is a data frame with the columns: `Parameter`, the effect size,
#' and (optionally) `CI` + `CI_low` + `CI_high`,
#'
#' And with the following attributes: `partial`, `generalized`, `ci`,
#' `alternative`, `anova_type` (`NA` or `NULL`), `approximate`.
#'
#' You can then set the `anova_type` attribute to {1, 2, 3, or `NA`} and return
#' the output.
#'
#'
#' @rdname effectsize_API
#' @name effectsize_API
NULL