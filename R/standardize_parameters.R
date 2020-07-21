#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param parameters An optional table containing the parameters to standardize. If `NULL`, will automatically retrieve it from the model.
#' @param method The method used for standardizing the parameters. Can be `"refit"` (default), `"posthoc"`, `"smart"` or `"basic"`. See 'Details'.
#' @inheritParams standardize
#' @inheritParams chisq_to_phi
#' @param centrality For Bayesian models, which point-estimates (centrality indices) to compute. Character (vector) or list with one or more of these options: "median", "mean", "MAP" or "all".
#'
#' @details \subsection{Methods:}{
#' \itemize{
#'  \item **refit**: This method is based on a complete model re-fit with a standardized version of data. Hence, this method is equal to standardizing the variables before fitting the model. It is the "purest" and the most accurate (Neter et al., 1989), but it is also the most computationally costly and long (especially for heavy models such as, for instance, for Bayesian models). This method is particularly recommended for complex models that include interactions or transformations (e.g., polynomial or spline terms). The `robust` (default to `FALSE`) argument enables a robust standardization of data, i.e., based on the `median` and `MAD` instead of the `mean` and `SD`.
#'  \item **posthoc**: Post-hoc standardization of the parameters, aiming at emulating the results obtained by "refit" without refitting the model. The coefficients are divided by the standard deviation (or MAD if `robust`) of the outcome (which becomes their expression 'unit'). Then, the coefficients related to numeric variables are additionally multiplied by the standard deviation (or MAD if `robust`) of the related terms, so that they correspond to changes of 1 SD of the predictor (e.g., "A change in 1 SD of `x` is related to a change of 0.24 of the SD of `y`). This does not apply to binary variables or factors, so the coefficients are still related to changes in levels. This method is not accurate and tend to give aberrant results when interactions are specified.
#'  \item **smart** (Standardization of Model's parameters with Adjustment, Reconnaissance and Transformation): Similar to `method = "posthoc"` in that it does not involve model refitting. The difference is that the SD of the response is computed on the relevant section of the data. For instance, if a factor with 3 levels A (the intercept), B and C is entered as a predictor, the effect corresponding to B vs. A will be scaled by the variance of the response at the intercept only. As a results, the coefficients for effects of factors are similar to a Glass' delta.
#'  \item **basic**: This method is similar to `method = "posthoc"`, but treats all variables as continuous: it also scales the coefficient by the standard deviation of model's matrix' parameter of factors levels (transformed to integers) or binary predictors. Although being inappropriate for these cases, this method is the one implemented by default in other software packages, such as `lm.beta::lm.beta()`.
#' }
#' When `method = "smart"` or `method = "classic"`, `standardize_parameters()`
#' also returns the standard errors for the standardized coefficients. Then, `ci()` can be
#' used to calculate confidence intervals for the standardized coefficients. See 'Examples'.
#' }
#'
#' @examples
#' library(effectsize)
#' data(iris)
#'
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' standardize_parameters(model, method = "refit")
#'
#' \donttest{
#' standardize_parameters(model, method = "posthoc")
#' standardize_parameters(model, method = "smart")
#' standardize_parameters(model, method = "basic")
#'
#' # Robust and 2 SD
#' standardize_parameters(model, robust = TRUE)
#' standardize_parameters(model, two_sd = TRUE)
#'
#'
#' iris$binary <- ifelse(iris$Sepal.Width > 3, 1, 0)
#' model <- glm(binary ~ Species * Sepal.Length, data = iris, family = "binomial")
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "posthoc")
#' standardize_parameters(model, method = "smart")
#' standardize_parameters(model, method = "basic")
#' }
#'
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Length ~ Species + Petal.Width, data = iris, iter = 500, refresh = 0)
#'   standardize_posteriors(model, method = "refit")
#'   standardize_posteriors(model, method = "posthoc")
#'   standardize_posteriors(model, method = "smart")
#'   standardize_posteriors(model, method = "basic")
#'
#'   standardize_parameters(model, method = "refit")
#'   standardize_parameters(model, method = "posthoc")
#'   standardize_parameters(model, method = "smart")
#'   standardize_parameters(model, method = "basic")
#' }
#' }
#' @importFrom stats mad sd predict cor model.matrix
#' @importFrom insight get_parameters model_info get_data get_response
#' @importFrom utils tail
#' @importFrom bayestestR describe_posterior
#'
#' @seealso standardize_info
#'
#' @return Standardized parameters.
#' @references \itemize{
#'   \item Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear regression models.
#'   \item Gelman, A. (2008). Scaling regression inputs by dividing by two standard deviations. Statistics in medicine, 27(15), 2865-2873.
#' }
#' @importFrom bayestestR describe_posterior
#' @importFrom parameters ci
#' @export
standardize_parameters <- function(model, parameters = NULL, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, centrality = "median", ...) {
  object_name <- deparse(substitute(model), width.cutoff = 500)
  std_params <-
    .standardize_parameters(
      model = model,
      parameters = parameters,
      method = method,
      ci = ci,
      robust = robust,
      two_sd = two_sd,
      verbose = verbose,
      object_name = object_name,
      ...
    )

  # Summarise for Bayesian models
  if (insight::model_info(model)$is_bayesian) {
    std_params <- bayestestR::describe_posterior(
      std_params, centrality = centrality, dispersion = FALSE,
      ci = ci, ci_method = "hdi",
      test = NULL, diagnostic = NULL, priors = FALSE
    )
    std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP","CI", "CI_low", "CI_high")]
    i <- colnames(std_params) %in% c("Coefficient", "Median", "Mean", "MAP")
    colnames(std_params)[i] <- paste0("Std_", colnames(std_params)[i])
  }

  attr(std_params, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  std_params
}


#' @rdname standardize_parameters
#' @export
standardize_posteriors <- function(model, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  out <- .standardize_parameters(model = model, method = method, robust = robust, two_sd = two_sd, verbose = verbose, ...)
  class(out) <- c("effectsize_std_params", "data.frame")
  out
}



# Internal Wrapper -------------------------------------------------------------------

#' @keywords internal
.standardize_parameters <- function(model, parameters = NULL, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, object_name = NULL, ...) {
  method <- match.arg(method, choices = c("default", "refit", "posthoc", "smart", "partial", "basic"))

  # Refit
  if (method %in% c("refit")) {
    std_params <- .standardize_parameters_refit(model, ci = ci, robust = robust, verbose = verbose, ...)

    # Posthoc
  } else if (method %in% c("posthoc", "smart", "basic", "classic")) {
    std_params <- .standardize_parameters_posthoc(model, parameters = parameters, method = method, ci = ci, robust = robust, two_sd = two_sd, verbose = verbose, object_name = object_name, ...)

    # Partial
  } else if (method == "partial") {
    stop("`method = 'partial'` not implemented yet :(")
  }

  class(std_params) <- c("effectsize_table", "see_effectsize_table",class(std_params))
  std_params
}




# REFIT -------------------------------------------------------------------
#' @importFrom parameters standard_error ci
#' @keywords internal
.standardize_parameters_refit <- function(model, ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  std_model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose, ...)
  std_params <- .extract_parameters(std_model)

  if (!is.null(ci) && !insight::model_info(model)$is_bayesian) {
    CIs <- parameters::ci(std_model, ci = ci)
    std_params$CI <- CIs$CI / 100
    std_params$CI_low <- CIs$CI_low
    std_params$CI_high <- CIs$CI_high
  }

  attr(std_params, "standard_error") <- parameters::standard_error(std_model)

  std_params
}











# POST-HOC -------------------------------------------------------------------
#' @importFrom parameters standard_error
#' @importFrom insight model_info get_data
#' @keywords internal
.standardize_parameters_posthoc <- function(model, parameters = NULL, method = "smart", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, object_name = NULL, ...) {

  # Sanity Checks
  if (verbose && insight::model_info(model)$is_zero_inflated) {
    warning("Non-refit parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
  }

  # Get parameters
  if (is.null(parameters)) {
    parameters <- .extract_parameters(model)
    if (insight::model_info(model)$is_bayesian) {
      parameters <- as.data.frame(t(parameters))
    }
  }

  # Get names of parameters
  if ("Parameter" %in% names(parameters)) {
    param_names <- parameters$Parameter
    parameters$Parameter <- NULL
  } else {
    param_names <- row.names(parameters)
  }

  # Remove non-numeric things
  parameters <- parameters[sapply(parameters, is.numeric)]



  # Get info
  deviations <- standardize_info(model, robust = robust)
  if (method == "basic") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Basic"
  } else if (method == "posthoc") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "smart") {
    col_dev_resp <- "Deviation_Response_Smart"
    col_dev_pred <- "Deviation_Smart"
  } else {
    stop("'method' must be one of 'basic', 'posthoc' or 'smart'.")
  }

  # Sapply standardization
  if (two_sd) {
    std_params <- sapply(parameters, function(x) x * (2 * deviations[[col_dev_pred]]) / deviations[[col_dev_resp]])
  } else {
    std_params <- sapply(parameters, function(x) x * deviations[[col_dev_pred]] / deviations[[col_dev_resp]])
  }


  # Clean
  if (insight::model_info(model)$is_bayesian) {
    std_params <- as.data.frame(t(std_params))
    row.names(std_params) <- NULL
    names(std_params) <- param_names
  } else {
    std_params <- cbind(
      data.frame(Parameter = param_names),
      as.data.frame(std_params)
    )
  }


  # Standardize SE if possible
  std_error <- tryCatch({
    se <- parameters::standard_error(model, component = "conditional")
    se$SE[unique(match(se$Parameter, param_names))]
  },
  error = function(e) {
    NULL
  }
  )

  # add standardized standard errors as attribute
  if (!is.null(std_error)) {
    std_error <- std_error * deviations[[col_dev_pred]] / deviations[[col_dev_resp]]
    attr(std_params, "standard_error") <- std_error
    class(std_params) <- c("effectsize_std_params", class(std_params))
  }

  attr(std_params, "object_name") <- object_name
  if (!is.null(ci) && !insight::model_info(model)$is_bayesian) {
    CIs <- parameters::ci(std_params, ci = ci)
    if (!is.null(CIs)) {
      std_params$CI <- CIs$CI / 100
      std_params$CI_low <- CIs$CI_low
      std_params$CI_high <- CIs$CI_high
    }
  }
  std_params
}





#' @keywords internal
.extract_parameters <- function(model, ...) {
  if (insight::model_info(model)$is_bayesian) {
    params <- insight::get_parameters(model, ...)
  } else {
    params <- insight::get_parameters(model, ...)
    names(params)[2] <- "Std_Coefficient"
  }
  params
}
