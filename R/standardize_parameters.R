#' Parameters standardization
#'
#' Compute standardized model parameters (coefficients).
#'
#' @param model A statistical model.
#' @param method The method used for standardizing the parameters. Can be
#'   `"refit"` (default), `"posthoc"`, `"smart"`, `"basic"` or `"pseudo"`. See
#'   'Details'.
#' @inheritParams standardize
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to [parameters::model_parameters], such as:
#' - For Bayesian models: `ci_method`, `centrality `, ...
#' - For Mixed models: `df_method`, ...
#' - etc.
#' @param parameters Deprecated.
#'
#' @details
#' ## Methods:
#' - **refit**: This method is based on a complete model re-fit with a
#' standardized version of the data. Hence, this method is equal to
#' standardizing the variables before fitting the model. It is the "purest" and
#' the most accurate (Neter et al., 1989), but it is also the most
#' computationally costly and long (especially for heavy models such as Bayesian
#' models). This method is particularly recommended for complex models that
#' include interactions or transformations (e.g., polynomial or spline terms).
#' The `robust` (default to `FALSE`) argument enables a robust standardization
#' of data, i.e., based on the `median` and `MAD` instead of the `mean` and
#' `SD`. **See [standardize()] for more details.**
#' - **posthoc**: Post-hoc standardization of the parameters, aiming at
#' emulating the results obtained by "refit" without refitting the model. The
#' coefficients are divided by the standard deviation (or MAD if `robust`) of
#' the outcome (which becomes their expression 'unit'). Then, the coefficients
#' related to numeric variables are additionally multiplied by the standard
#' deviation (or MAD if `robust`) of the related terms, so that they correspond
#' to changes of 1 SD of the predictor (e.g., "A change in 1 SD of `x` is
#' related to a change of 0.24 of the SD of `y`). This does not apply to binary
#' variables or factors, so the coefficients are still related to changes in
#' levels. This method is not accurate and tend to give aberrant results when
#' interactions are specified.
#' - **smart** (Standardization of Model's parameters with Adjustment,
#' Reconnaissance and Transformation): Similar to `method = "posthoc"` in that
#' it does not involve model refitting. The difference is that the SD (or MAD if
#' `robust`) of the response is computed on the relevant section of the data.
#' For instance, if a factor with 3 levels A (the intercept), B and C is entered
#' as a predictor, the effect corresponding to B vs. A will be scaled by the
#' variance of the response at the intercept only. As a results, the
#' coefficients for effects of factors are similar to a Glass' delta.
#' - **basic**: This method is similar to `method = "posthoc"`, but treats all
#' variables as continuous: it also scales the coefficient by the standard
#' deviation of model's matrix' parameter of factors levels (transformed to
#' integers) or binary predictors. Although being inappropriate for these cases,
#' this method is the one implemented by default in other software packages,
#' such as [lm.beta::lm.beta()].
#' - **pseudo** (*for 2-level (G)LMMs only*): In this (post-hoc) method, the
#' response and the predictor are standardized based on the level of prediction
#' (levels are detected with [parameters::check_heterogeneity()]): Predictors
#' are standardized based on their SD at level of prediction (see also
#' [parameters::demean()]); The outcome (in linear LMMs) is standardized based
#' on a fitted random-intercept-model, where `sqrt(random-intercept-variance)`
#' is used for level 2 predictors, and `sqrt(residual-variance)` is used for
#' level 1 predictors (Hoffman 2015, page 342).
#'
#' ## Transformed Variables
#' When the model's formula contains transformations (e.g. `y ~ exp(X)`) `method
#' = "refit"` might give different results compared to the other (post-hoc)
#' methods: where `"refit"` standardizes the data prior to the transformation
#' (e.g. equivalent to `exp(scale(X))`), the post-hoc methods standardize the
#' transformed data (e.g. equivalent to `scale(exp(X))`). See [standardize()]
#' for more details on how different transformations are dealt with.
#'
#' ## Generalized Linear Models
#' When standardizing coefficients of a generalized model (GLM, GLMM, etc), only
#' the predictors are standardized, maintaining the interpretability of the
#' coefficients (e.g., in a binomial model: the exponent of the standardized
#' parameter is the OR of a change of 1 SD in the predictor, etc.)
#'
#' @return A data frame with the standardized parameters and their CIs.
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
#' model <- glm(am ~ cyl * mpg, data = mtcars, family = "binomial")
#' standardize_parameters(model, method = "refit")
#' standardize_parameters(model, method = "posthoc")
#' standardize_parameters(model, method = "smart")
#' standardize_parameters(model, method = "basic")
#' }
#'
#' \donttest{
#' if (require("lme4")) {
#'   m <- lmer(mpg ~ cyl + am + vs + (1|cyl), mtcars)
#'   standardize_parameters(m, method = "pseudo")
#' }
#'
#'
#'
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Length ~ Species + Petal.Width, data = iris, refresh = 0)
#'   # standardize_posteriors(model, method = "refit")
#'   # standardize_posteriors(model, method = "posthoc")
#'   # standardize_posteriors(model, method = "smart")
#'   head(standardize_posteriors(model, method = "basic"))
#' }
#'
#' }
#'
#' @seealso [standardize_info()]
#'
#' @return Standardized parameters table.
#'
#' @references
#' - Hoffman, L. (2015). Longitudinal analysis: Modeling within-person fluctuation and change. Routledge.
#' - Neter, J., Wasserman, W., & Kutner, M. H. (1989). Applied linear regression models.
#' - Gelman, A. (2008). Scaling regression inputs by dividing by two standard deviations. Statistics in medicine, 27(15), 2865-2873.
#'
#' @export
standardize_parameters <- function(model, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, parameters, ...) {
  if (!missing(parameters)) {
    warning(
      "'parameters' argument is deprecated, and will not be used.",
      immediate. = TRUE
    )
  }

  UseMethod("standardize_parameters")
}

#' @importFrom parameters model_parameters
#' @export
standardize_parameters.default <- function(model, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, parameters, ...) {
  object_name <- deparse(substitute(model), width.cutoff = 500)

  if (method == "refit") {
    model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose)
  }

  pars <- parameters::model_parameters(model, ci = ci, standardize = NULL, ...)

  if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo")) {
    pars <- .standardize_parameters_posthoc(pars, method, model, robust, two_sd, verbose)

    method <- attr(pars, "std_method")
    robust <- attr(pars, "robust")
  }

  ## clean cols
  if (!is.null(ci)) pars$CI <- attr(pars, "ci")
  pars <- pars[,colnames(pars) %in% c("Parameter", "CI", .col_2_scale)]
  i <- colnames(pars) %in% c("Coefficient", "Median", "Mean", "MAP")
  colnames(pars)[i] <- paste0("Std_", colnames(pars)[i])

  ## SE attribute?
  if ("SE" %in% colnames(pars)) {
    attr(pars, "standard_error") <- pars$SE
    pars$SE <- NULL
  }

  ## attributes
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "object_name") <- object_name
  class(pars) <- c("effectsize_table", "see_effectsize_table", "effectsize_std_params", "data.frame")
  return(pars)
}

#' @export
standardize_parameters.parameters_model <- function(model, method = "refit", ci = NULL, robust = FALSE, two_sd = FALSE, verbose = TRUE, parameters, ...) {
  if (method == "refit") {
    stop("Method 'refit' not supported for 'model_parameters()", call. = TRUE)
  }

  if (!is.null(ci)) {
    warnings("Argument 'ci' argument not supported for 'model_parameters(). It is ignored.", call. = TRUE)
  }

  pars <- model
  ci <- attr(pars, "ci")
  obj_name <- attr(pars, "obj_name")
  model <- .get_object(model)

  pars <- .standardize_parameters_posthoc(pars, method, model, robust, two_sd, verbose)
  method <- attr(pars, "std_method")
  robust <- attr(pars, "robust")

  ## clean cols
  if ("CI_low" %in% colnames(pars)) pars$CI <- ci
  pars <- pars[,colnames(pars) %in% c("Parameter", "CI", .col_2_scale)]
  i <- colnames(pars) %in% c("Coefficient", "Median", "Mean", "MAP")
  colnames(pars)[i] <- paste0("Std_", colnames(pars)[i])

  ## SE attribute?
  if ("SE" %in% colnames(pars)) {
    attr(pars, "standard_error") <- pars$SE
    pars$SE <- NULL
  }

  ## attributes
  attr(pars, "two_sd") <- two_sd
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  class(pars) <- c("effectsize_table", "see_effectsize_table", "effectsize_std_params", "data.frame")
  return(pars)
}

#' @keywords internal
#' @importFrom insight model_info find_random
.standardize_parameters_posthoc <- function(pars, method, model, robust, two_sd, verbose) {
  # Sanity Check for ZI
  if (verbose && insight::model_info(model)$is_zero_inflated) {
    warning("Non-refit parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
  }

  # Sanity Check for "pseudo"
  if (method == "pseudo" &&
      !(insight::model_info(model)$is_mixed &&
        length(insight::find_random(model)$random) == 1)) {
    warning(
      "'pseudo' method only available for 2-level (G)LMMs.\n",
      "Setting method to 'basic'.",
      call. = FALSE
    )
    method <- "basic"
  }

  if (robust && method == "pseudo") {
    warning("'robust' standardization not available for 'pseudo' method.",
            call. = FALSE)
    robust <- FALSE
  }


  ## Get scaling factors
  deviations <- standardize_info(model, robust = robust, include_pseudo = method == "pseudo")
  i <- match(deviations$Parameter, pars$Parameter)
  pars <- pars[i,]

  if (method == "basic") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Basic"
  } else if (method == "posthoc") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "smart") {
    col_dev_resp <- "Deviation_Response_Smart"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "pseudo") {
    col_dev_resp <- "Deviation_Response_Pseudo"
    col_dev_pred <- "Deviation_Pseudo"
  } else {
    stop("'method' must be one of 'basic', 'posthoc', 'smart' or 'pseudo'.")
  }

  # Sapply standardization
  f <- if (two_sd) 2 else 1

  pars[,colnames(pars) %in% .col_2_scale] <- lapply(
    pars[, colnames(pars) %in% .col_2_scale, drop = FALSE],
    function(x) {
      x * (f * deviations[[col_dev_pred]] / deviations[[col_dev_resp]])
    }
  )

  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust

  return(pars)
}

#' @keywords internal
.col_2_scale <- c("Coefficient","Median", "Mean", "MAP", "SE", "CI_low", "CI_high")


# standardize_posteriors --------------------------------------------------



#' @rdname standardize_parameters
#' @export
standardize_posteriors <- function(model, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  object_name <- deparse(substitute(model), width.cutoff = 500)

  if (method == "refit") {
    model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose)
  }

  pars <- insight::get_parameters(model)


  if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo")) {
    pars <- .standardize_posteriors_posthoc(pars, method, model, robust, two_sd, verbose)

    method <- attr(pars, "std_method")
    robust <- attr(pars, "robust")
  }

  ## attributes
  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust
  attr(pars, "object_name") <- object_name
  class(pars) <- c("effectsize_std_params", class(pars))
  return(pars)
}



#' @keywords internal
#' @importFrom insight model_info find_random
.standardize_posteriors_posthoc <- function(pars, method, model, robust, two_sd, verbose) {
  # Sanity Check for ZI
  if (verbose && insight::model_info(model)$is_zero_inflated) {
    warning("Non-refit parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
  }

  # Sanity Check for "pseudo"
  if (method == "pseudo" &&
      !(insight::model_info(model)$is_mixed &&
        length(insight::find_random(model)$random) == 1)) {
    warning(
      "'pseudo' method only available for 2-level (G)LMMs.\n",
      "Setting method to 'basic'.",
      call. = FALSE
    )
    method <- "basic"
  }

  if (robust && method == "pseudo") {
    warning("'robust' standardization not available for 'pseudo' method.",
            call. = FALSE)
    robust <- FALSE
  }

  ## Get scaling factors
  deviations <- standardize_info(model, robust = robust, include_pseudo = method == "pseudo")
  i <- match(deviations$Parameter, colnames(pars))
  pars <- pars[,i]

  if (method == "basic") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Basic"
  } else if (method == "posthoc") {
    col_dev_resp <- "Deviation_Response_Basic"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "smart") {
    col_dev_resp <- "Deviation_Response_Smart"
    col_dev_pred <- "Deviation_Smart"
  } else if (method == "pseudo") {
    col_dev_resp <- "Deviation_Response_Pseudo"
    col_dev_pred <- "Deviation_Pseudo"
  } else {
    stop("'method' must be one of 'basic', 'posthoc', 'smart' or 'pseudo'.")
  }

  # Sapply standardization
  f <- if (two_sd) 2 else 1

  pars <- t(t(pars) * (f * deviations[[col_dev_pred]] / deviations[[col_dev_resp]]))
  pars <- as.data.frame(pars)

  attr(pars, "std_method") <- method
  attr(pars, "two_sd") <- two_sd
  attr(pars, "robust") <- robust

  return(pars)
}




#' # OLD ---------------------------------------------------------------------
#'
#'
#' standardize_parameters <- function(model, parameters = NULL, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, centrality = "median", ...) {
#'   object_name <- deparse(substitute(model), width.cutoff = 500)
#'   std_params <-
#'     .standardize_parameters(
#'       model = model,
#'       parameters = parameters,
#'       method = method,
#'       ci = ci,
#'       robust = robust,
#'       two_sd = two_sd,
#'       verbose = verbose,
#'       object_name = object_name,
#'       ...
#'     )
#'
#'   # Summarise for Bayesian models
#'   if (insight::model_info(model)$is_bayesian) {
#'     method <- attr(std_params, "std_method")
#'     robust <- attr(std_params, "robust")
#'     two_sd <- attr(std_params, "two_sd")
#'
#'     std_params <- bayestestR::describe_posterior(
#'       std_params, centrality = centrality, dispersion = FALSE,
#'       ci = ci, ci_method = "hdi",
#'       test = NULL, diagnostic = NULL, priors = FALSE
#'     )
#'     std_params$CI <- ci
#'     std_params <- std_params[names(std_params) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP","CI", "CI_low", "CI_high")]
#'     i <- colnames(std_params) %in% c("Coefficient", "Median", "Mean", "MAP")
#'     colnames(std_params)[i] <- paste0("Std_", colnames(std_params)[i])
#'
#'     attr(std_params, "std_method") <- method
#'     attr(std_params, "two_sd") <- two_sd
#'     attr(std_params, "robust") <- robust
#'   }
#'
#'   class(std_params) <- c("effectsize_table", "see_effectsize_table", class(std_params))
#'   attr(std_params, "object_name") <- object_name
#'   std_params
#' }
#'
#'
#' #' @rdname standardize_parameters
#' #' @export
#' standardize_posteriors <- function(model, method = "refit", robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
#'   out <- .standardize_parameters(model = model, method = method, robust = robust, two_sd = two_sd, verbose = verbose, ...)
#'   class(out) <- c("effectsize_std_params", "data.frame")
#'   out
#' }
#'
#'
#'
#' # Internal Wrapper -------------------------------------------------------------------
#'
#' #' @keywords internal
#' .standardize_parameters <- function(model, parameters = NULL, method = "refit", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, object_name = NULL, ...) {
#'   method <- match.arg(method, choices = c("default", "refit", "posthoc", "smart", "partial", "basic", "pseudo"))
#'
#'   if (method == "default") method <- "refit"
#'
#'   if (method %in% c("refit")) {
#'     # Refit
#'     std_params <- .standardize_parameters_refit(
#'       model,
#'       ci = ci,
#'       robust = robust,
#'       verbose = verbose,
#'       ...
#'     )
#'   } else if (method %in% c("posthoc", "smart", "basic", "classic", "pseudo")) {
#'     # Posthoc
#'
#'
#'     if (method == "pseudo" &&
#'         !(insight::model_info(model)$is_mixed &&
#'           length(insight::find_random(model)$random) == 1)) {
#'       warning(
#'         "'pseudo' method only available for 2-level (G)LMMs.\n",
#'         "Setting method to 'basic'.",
#'         call. = FALSE
#'       )
#'       method <- "basic"
#'     }
#'
#'     if (robust && method == "pseudo") {
#'       warning("'robust' standardization not available for 'pseudo' method.",
#'               call. = FALSE)
#'       robust <- FALSE
#'     }
#'
#'     std_params <- .standardize_parameters_posthoc(
#'       model,
#'       parameters = parameters,
#'       method = method,
#'       ci = ci,
#'       robust = robust,
#'       two_sd = two_sd,
#'       verbose = verbose,
#'       object_name = object_name,
#'       ...
#'     )
#'   } else if (method == "partial") {
#'     # Partial
#'     stop("`method = 'partial'` not implemented yet :(")
#'   }
#'
#'   attr(std_params, "std_method") <- method
#'   attr(std_params, "robust") <- robust
#'   attr(std_params, "two_sd") <- two_sd
#'   std_params
#' }
#'
#'
#'
#'
#' # REFIT -------------------------------------------------------------------
#' #' @importFrom parameters standard_error ci
#' #' @keywords internal
#' .standardize_parameters_refit <- function(model, ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
#'   std_model <- standardize(model, robust = robust, two_sd = two_sd, verbose = verbose, ...)
#'   std_params <- .extract_parameters(std_model)
#'
#'   if (!is.null(ci) && !insight::model_info(model)$is_bayesian) {
#'     CIs <- parameters::ci(std_model, ci = ci)
#'     std_params$CI <- CIs$CI / 100
#'     std_params$CI_low <- CIs$CI_low
#'     std_params$CI_high <- CIs$CI_high
#'   }
#'
#'   attr(std_params, "standard_error") <- parameters::standard_error(std_model)
#'
#'   std_params
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' # POST-HOC -------------------------------------------------------------------
#' #' @importFrom parameters standard_error
#' #' @importFrom insight model_info get_data
#' #' @keywords internal
#' .standardize_parameters_posthoc <- function(model, parameters = NULL, method = "smart", ci = 0.95, robust = FALSE, two_sd = FALSE, verbose = TRUE, object_name = NULL, ...) {
#'
#'   # Sanity Checks
#'   if (verbose && insight::model_info(model)$is_zero_inflated) {
#'     warning("Non-refit parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
#'   }
#'
#'   # Get parameters
#'   if (is.null(parameters)) {
#'     parameters <- .extract_parameters(model)
#'     if (insight::model_info(model)$is_bayesian) {
#'       parameters <- as.data.frame(t(parameters))
#'     }
#'   }
#'
#'   # Get names of parameters
#'   if ("Parameter" %in% names(parameters)) {
#'     param_names <- parameters$Parameter
#'     parameters$Parameter <- NULL
#'   } else {
#'     param_names <- row.names(parameters)
#'   }
#'
#'   # Remove non-numeric things
#'   parameters <- parameters[sapply(parameters, is.numeric)]
#'
#'
#'
#'   # Get info
#'   deviations <- standardize_info(model, robust = robust, include_pseudo = method == "pseudo")
#'   if (method == "basic") {
#'     col_dev_resp <- "Deviation_Response_Basic"
#'     col_dev_pred <- "Deviation_Basic"
#'   } else if (method == "posthoc") {
#'     col_dev_resp <- "Deviation_Response_Basic"
#'     col_dev_pred <- "Deviation_Smart"
#'   } else if (method == "smart") {
#'     col_dev_resp <- "Deviation_Response_Smart"
#'     col_dev_pred <- "Deviation_Smart"
#'   } else if (method == "pseudo") {
#'     col_dev_resp <- "Deviation_Response_Pseudo"
#'     col_dev_pred <- "Deviation_Pseudo"
#'   } else {
#'     stop("'method' must be one of 'basic', 'posthoc', 'smart' or 'pseudo'.")
#'   }
#'
#'   # Sapply standardization
#'   if (two_sd) {
#'     std_params <- sapply(parameters, function(x) x * (2 * deviations[[col_dev_pred]]) / deviations[[col_dev_resp]])
#'   } else {
#'     std_params <- sapply(parameters, function(x) x * deviations[[col_dev_pred]] / deviations[[col_dev_resp]])
#'   }
#'
#'
#'   # Clean
#'   if (insight::model_info(model)$is_bayesian) {
#'     std_params <- as.data.frame(t(std_params))
#'     row.names(std_params) <- NULL
#'     names(std_params) <- param_names
#'   } else {
#'     std_params <- cbind(
#'       data.frame(Parameter = param_names),
#'       as.data.frame(std_params)
#'     )
#'   }
#'
#'
#'   # add standardized standard errors as attribute (if possible)
#'   std_error <- tryCatch({
#'     se <- parameters::standard_error(model, component = "conditional")
#'     se$SE[unique(match(se$Parameter, param_names))]
#'   }, error = function(e) NULL)
#'
#'   if (!is.null(std_error)) {
#'     std_error <- std_error * deviations[[col_dev_pred]] / deviations[[col_dev_resp]]
#'     attr(std_params, "standard_error") <- std_error
#'     class(std_params) <- c("effectsize_std_params", class(std_params))
#'   }
#'
#'   attr(std_params, "object_name") <- object_name
#'   if (!is.null(ci) && !insight::model_info(model)$is_bayesian) {
#'     CIs <- parameters::ci(std_params, ci = ci)
#'     if (!is.null(CIs)) {
#'       std_params$CI <- CIs$CI / 100
#'       std_params$CI_low <- CIs$CI_low
#'       std_params$CI_high <- CIs$CI_high
#'     }
#'   }
#'
#'
#'   return(std_params)
#' }
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .extract_parameters <- function(model, ...) {
#'   params <- insight::get_parameters(model, ...)
#'   if (!insight::model_info(model)$is_bayesian) {
#'     names(params)[2] <- "Std_Coefficient"
#'   }
#'   params
#' }
#'
