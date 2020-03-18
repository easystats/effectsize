#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param parameters An optional table containing the parameters to standardize. If \code{NULL}, will automatically retrieve it from the model.
#' @param method The method used for standardizing the parameters. Can be \code{"refit"} (default), \code{"posthoc"}, \code{"smart"} or \code{"basic"}. See 'Details'.
#' @inheritParams standardize
#' @param centrality For Bayesian models, which point-estimates (centrality indices) to compute. Character (vector) or list with one or more of these options: "median", "mean", "MAP" or "all".
#'
#' @details \subsection{Methods:}{
#' \itemize{
#'  \item \strong{refit}: This method is based on a complete model re-fit with a standardized version of data. Hence, this method is equal to standardizing the variables before fitting the model. It is the "purest" and the most accurate (Neter et al., 1989), but it is also the most computationally costly and long (especially for heavy models such as, for instance, for Bayesian models). This method is particularly recommended for complex models that include interactions or transformations (e.g., polynomial or spline terms). The \code{robust} (default to \code{FALSE}) argument enables a robust standardization of data, i.e., based on the \code{median} and \code{MAD} instead of the \code{mean} and \code{SD}.
#'  \item \strong{posthoc}: Post-hoc standardization of the parameters, aiming at emulating the results obtained by "refit" without refitting the model. The coefficients are divided by the standard deviation (or MAD if \code{robust}) of the outcome (which becomes their expression 'unit'). Then, the coefficients related to numeric variables are additionally multiplied by the standard deviation (or MAD if \code{robust}) of the related terms, so that they correspond to changes of 1 SD of the predictor (e.g., "A change in 1 SD of \code{x} is related to a change of 0.24 of the SD of \code{y}). This does not apply to binary variables or factors, so the coefficients are still related to changes in levels. This method is not accurate and tend to give aberrant results when interactions are specified.
#'  \item \strong{smart} (Standardization of Model's parameters with Adjustment, Reconnaissance and Transformation): Similar to \code{method = "posthoc"} in that it does not involve model refitting. The difference is that the SD of the response is computed on the relevant section of the data. For instance, if a factor with 3 levels A (the intercept), B and C is entered as a predictor, the effect corresponding to B vs. A will be scaled by the variance of the response at the intercept only. As a results, the coefficients for effects of factors are similar to a Glass' delta.
#'  \item \strong{basic}: This method is similar to \code{method = "posthoc"}, but treats all variables as continuous: it also scales the coefficient by the standard deviation of model's matrix' parameter of factors levels (transformed to integers) or binary predictors. Although being inappropriate for these cases, this method is the one implemented by default in other software packages, such as \code{lm.beta::lm.beta()}.
#' }
#' When \code{method = "smart"} or \code{method = "classic"}, \code{standardize_parameters()}
#' also returns the standard errors for the standardized coefficients. Then, \code{ci()} can be
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
#' # show CI
#' library(parameters)
#' params <- standardize_parameters(model, method = "smart", robust = TRUE)
#' ci(params)
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
#'   model <- stan_glm(Sepal.Length ~ Species * Petal.Width, data = iris, iter = 500, refresh = 0)
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
#' @export
standardize_parameters <- function(model, parameters = NULL, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, centrality = "median", ...) {
  std_params <- .standardize_parameters(model = model, parameters = parameters, method = method, robust = robust, two_sd = two_sd, verbose = verbose, ...)

  # Summarise for Bayesian models
  if (insight::model_info(model)$is_bayesian) {
    std_params <- bayestestR::describe_posterior(std_params, centrality = centrality, dispersion = FALSE, ci = NULL, test = NULL, diagnostic = NULL, priors = FALSE)
    std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]
    names(std_params)[-1] <- paste0("Std_", names(std_params)[-1])
  }

  std_params
}


#' @rdname standardize_parameters
#' @export
standardize_posteriors <- function(model, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  .standardize_parameters(model = model, method = method, robust = robust, two_sd = two_sd, verbose = verbose, ...)
}



# Internal Wrapper -------------------------------------------------------------------

#' @keywords internal
.standardize_parameters <- function(model, parameters = NULL, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  method <- match.arg(method, choices = c("default", "refit", "posthoc", "smart", "partial", "basic"))

  # Refit
  if (method %in% c("refit")) {
    std_params <- .standardize_parameters_refit(model, robust = robust, verbose = verbose, ...)

    # Posthoc
  } else if (method %in% c("posthoc", "smart", "basic", "classic")) {
    std_params <- .standardize_parameters_posthoc(model, parameters = parameters, method = method, robust = robust, two_sd = two_sd, verbose = verbose, ...)

    # Partial
  } else if (method == "partial") {
    stop("`method = 'partial'` not implemented yet :(")
  }

  std_params
}




# REFIT -------------------------------------------------------------------
#' @importFrom parameters standard_error ci
#' @keywords internal
.standardize_parameters_refit <- function(model, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  std_model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose, ...)
  std_params <- .extract_parameters(std_model)

  attr(std_params, "standard_error") <- parameters::standard_error(std_model)
  attr(std_params, "ci") <- parameters::ci(std_model)

  std_params
}











# POST-HOC -------------------------------------------------------------------
#' @importFrom parameters standard_error
#' @importFrom insight model_info get_data
#' @keywords internal
.standardize_parameters_posthoc <- function(model, parameters = NULL, method = "smart", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {

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
