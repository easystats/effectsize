#' Standardization (Z-scoring)
#'
#' Performs a standardization of data (z-scoring), i.e., centering and scaling,
#' so that the data is expressed in terms of standard deviation (i.e., mean = 0,
#' SD = 1) or Median Absolute Deviance (median = 0, MAD = 1). When applied to a
#' statistical model, this function extracts the dataset, standardizes it, and
#' refits the model with this standardized version of the dataset. The
#' [normalize()] function can also be used to scale all numeric variables within
#' the 0 - 1 range.
#'
#' @param x A data frame, a vector or a statistical model.
#' @param robust Logical, if `TRUE`, centering is done by subtracting the
#'   median from the variables and dividing it by the median absolute deviation
#'   (MAD). If `FALSE`, variables are standardized by subtracting the
#'   mean and dividing it by the standard deviation (SD).
#' @param two_sd If `TRUE`, the variables are scaled by two times the deviation
#'   (SD or MAD depending on `robust`). This method can be useful to obtain
#'   model coefficients of continuous parameters comparable to coefficients
#'   related to binary predictors, when applied to **the predictors** (not the
#'   outcome) (Gelman, 2008).
#' @param weights Can be `NULL` (for no weighting), or:
#' - For model: if `TRUE` (default), a weighted-standardization is carried out.
#' - For `data.frame`s: a numeric vector of weights, or a character of the name of a column in the `data.frame` that contains the weights.
#' - For numeric vectors: a numeric vector of weights.
#' @param verbose Toggle warnings on or off.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The standardized object (either a standardize data frame or a
#'   statistical model fitted on standardized data).
#'
#' @note When `x` is a vector or a data frame with `na_action = "column")`,
#'   missing values are preserved, so the return value has the same length /
#'   number of rows as the original input.
#'
#' @details
#' If `x` is a model object, standardization is done by completely refitting the
#' model on the standardized data. Hence, this approach is equal to
#' standardizing the variables *before* fitting the model and will return a new
#' model object. However, this method is particularly recommended for complex
#' models that include interactions or transformations (e.g., polynomial or
#' spline terms). The `robust` (default to `FALSE`) argument enables a robust
#' standardization of data, i.e., based on the `median` and `MAD` instead of the
#' `mean` and `SD`. See [standardize_parameters()] for other methods of
#' standardizing model coefficients.
#'
#' ## Transformed Variables
#' When the model's formula contains transformations (e.g. `y ~ exp(X)`) the
#' transformation effectively takes place after standardization (e.g.,
#' `exp(scale(X))`). Some transformations are undefined for negative values,
#' such as `log()` and `sqrt()`. To avoid dropping these values, the
#' standardized data is shifted by `Z - min(Z) + 1` or `Z - min(Z)`
#' (respectively).
#'
#' @seealso [normalize()] [standardize_parameters()]
#'
#' @examples
#' # Data frames
#' summary(standardize(iris))
#'
#' # Models
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' coef(standardize(model))
#' @export
standardize <- function(x, robust = FALSE, two_sd = FALSE, weights = NULL, verbose = TRUE, ...) {
  UseMethod("standardize")
}
