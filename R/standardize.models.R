#' @rdname standardize
#' @param include_response For a model, if `TRUE` (default), the response value
#'   will also be standardized. If `FALSE`, only the predictors will be
#'   standardized. Note that for certain models (logistic regression, count
#'   models, ...), the response value will never be standardized, to make
#'   re-fitting the model work. (For `mediate` models, only applies to the y
#'   model; m model's response will always be standardized.)
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response get_response find_weights get_weights
#' @importFrom utils capture.output
#'
#' @inheritSection standardize_parameters Generalized Linear Models
#' @export
standardize.default <- function(x, robust = FALSE, two_sd = FALSE, weights = TRUE, verbose = TRUE,
                                include_response = TRUE, ...) {
  m_info <- insight::model_info(x)
  data <- insight::get_data(x)

  # for models with specific scale of the response value (e.g. count models
  # with positive integers, or beta with ratio between 0 and 1), we need to
  # make sure that the original response value will be restored after
  # standardizing, as these models also require a non-standardized response.
  if (.no_response_standardize(m_info) || !include_response) {
    resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  } else if (include_response && two_sd) {
    resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  } else {
    resp <- NULL
  }

  # Do not standardize weighting-variable, because negative weights will
  # cause errors in "update()"
  weight_variable <- insight::find_weights(x)

  if (!is.null(weight_variable) && !weight_variable %in% colnames(data) && "(weights)" %in% colnames(data)) {
    data$.missing_weight <- data[["(weights)"]]
    colnames(data)[ncol(data)] <- weight_variable
    weight_variable <- c(weight_variable, "(weights)")
  }

  # don't standardize random effects
  random_group_factor <- insight::find_random(x, flatten = TRUE, split_nested = TRUE)

  # standardize data
  dont_standardize <- c(resp, weight_variable, random_group_factor)
  do_standardize <- setdiff(colnames(data), dont_standardize)

  # can't std data$var variables
  # TODO what about "with"?
  if (any(doller_vars <- grepl("(.*)\\$(.*)", do_standardize))) {
    doller_vars <- colnames(data)[doller_vars]
    warning("Unable to standardize variables evaluated in the environment (i.e., not in `data`).\n",
            "The following variables will not be standardizd:\n\t",
            paste0(doller_vars, collapse = ", "), call. = FALSE)
    do_standardize <- setdiff(do_standardize, doller_vars)
  }


  if (length(do_standardize)) {
    w <- insight::get_weights(x, na_rm = TRUE)

    data_std <- standardize(data[do_standardize],
      robust = robust,
      two_sd = two_sd,
      weights = if (weights) w else NULL,
      verbose = verbose
    )

    if (!.no_response_standardize(m_info) && include_response && two_sd) {
      # if two_sd, it must not affect the response!
      data_std[resp] <- standardize(data[resp],
        robust = robust,
        two_sd = FALSE,
        weights = if (weights) w else NULL,
        verbose = verbose
      )
      dont_standardize <- setdiff(dont_standardize, resp)
    }
  } else {
    warning("No variables could be standardized.", call. = FALSE)
    return(x)
  }


  # if we standardize log-terms, standardization will fail (because log of
  # negative value is NaN). Do some back-transformation here

  log_terms <- .log_terms(x, data_std)
  if (length(log_terms) > 0) {
    data_std[log_terms] <- lapply(data_std[log_terms], function(i) {
      i - min(i, na.rm = TRUE) + 1
    })
  }

  # same for sqrt
  sqrt_terms <- .sqrt_terms(x, data_std)
  if (length(sqrt_terms) > 0) {
    data_std[sqrt_terms] <- lapply(data_std[sqrt_terms], function(i) {
      i - min(i, na.rm = TRUE)
    })
  }

  if (verbose && (length(log_terms) > 0 || length(sqrt_terms) > 0)) {
    message("Formula contains log- or sqrt-terms. See help(\"standardize\") for how such terms are standardized.")
  }


  # restore data that should not be standardized

  if (length(dont_standardize)) {
    remaining_columns <- intersect(colnames(data), dont_standardize)
    data_std <- cbind(data[, remaining_columns, drop = FALSE], data_std)
  }

  # update model with standardized data

  if (inherits(x, "brmsfit")) {
    text <- utils::capture.output(model_std <- stats::update(x, newdata = data_std))
  } else if (inherits(x, "biglm")) {
    text <- utils::capture.output(model_std <- stats::update(x, moredata = data_std))
  } else if (inherits(x, "mixor")) {
    data_std <- data_std[order(data_std[, random_group_factor, drop = FALSE]), ]
    text <- utils::capture.output(model_std <- stats::update(x, data = data_std))
  } else {
    text <- utils::capture.output(model_std <- stats::update(x, data = data_std))
  }

  model_std
}




# exceptions, models that cannot use the default-method --------------------


#' @export
standardize.mlm <- function(x, robust = FALSE, two_sd = FALSE, weights = TRUE, verbose = TRUE, ...) {
  standardize.default(x,
    robust = robust, two_sd = two_sd, weights = weights, verbose = verbose,
    include_response = FALSE, ...
  )
}


#' @export
standardize.coxph <- function(x, robust = FALSE, two_sd = FALSE, weights = TRUE, verbose = TRUE, ...) {

  # for some models, the DV cannot be standardized when using
  # "update()", so we only standardize model predictors
  #
  # survival models have some strange format for the response variable,
  # so we don't use the default standardize function here, but
  # use a different approach that only retrieves predictors that should
  # be standardized.

  pred <- insight::find_predictors(x, flatten = TRUE)
  data <- insight::get_data(x)

  # if we standardize log-terms, standardization will fail (because log of
  # negative value is NaN)

  log_terms <- .log_terms(x)
  if (length(log_terms)) pred <- setdiff(pred, log_terms)

  weight_variable <- insight::find_weights(x)
  if (length(weight_variable)) pred <- setdiff(pred, weight_variable)

  # standardize data, if we have anything left to standardize

  if (length(pred)) {
    w <- insight::get_weights(x, na_rm = TRUE)

    data_std <- standardize(data[pred],
      robust = robust,
      two_sd = two_sd,
      weights = if (weights) w else NULL,
      verbose = verbose
    )
    data[pred] <- data_std
  }

  text <- utils::capture.output(model_std <- stats::update(x, data = data))

  model_std
}


#' @export
standardize.coxme <- standardize.coxph


#' @export
#' @importFrom utils capture.output
#' @importFrom insight get_data
#' @importFrom stats update
standardize.mediate <- function(x, robust = FALSE, two_sd = FALSE, weights = TRUE, verbose = TRUE,
                                include_response = TRUE, ...) {

  # models and data
  y <- x$model.y
  m <- x$model.m
  y_data <- insight::get_data(y)
  m_data <- insight::get_data(m)

  # std models and data
  y_std <- standardize(y,
    robust = robust, two_sd = two_sd,
    weights = weights, verbose = verbose,
    include_response = include_response, ...
  )
  m_std <- standardize(m,
    robust = robust, two_sd = two_sd,
    weights = weights, verbose = verbose,
    include_response = TRUE, ...
  )
  y_data_std <- insight::get_data(y_std)
  m_data_std <- insight::get_data(m_std)

  # fixed values
  covs <- x$covariates
  control.value <- x$control.value
  treat.value <- x$treat.value


  if (!is.null(covs)) {
    covs <- mapply(.rescale_fixed_values, covs, names(covs),
      SIMPLIFY = FALSE,
      MoreArgs = list(
        y_data = y_data, m_data = m_data,
        y_data_std = y_data_std, m_data_std = m_data_std
      )
    )
    if (verbose) message("covariates' values have been rescaled to their standardized scales.")
  }

  # if (is.numeric(y_data[[x$treat]]) || is.numeric(m_data[[x$treat]])) {
  #   if (!(is.numeric(y_data[[x$treat]]) && is.numeric(m_data[[x$treat]]))) {
  #     stop("'treat' variable is not of same type across both y and m models.",
  #          "\nCannot consistently standardize.", call. = FALSE)
  #   }
  #
  #   temp_vals <- .rescale_fixed_values(c(control.value, treat.value), x$treat,
  #                                      y_data = y_data, m_data = m_data,
  #                                      y_data_std = y_data_std, m_data_std = m_data_std)
  #
  #   control.value <- temp_vals[1]
  #   treat.value <- temp_vals[2]
  #   if (verbose) message("control and treatment values have been rescaled to their standardized scales.")
  # }

  if (verbose && !all(c(control.value, treat.value) %in% c(0, 1))) {
    warning("control and treat values are not 0 and 1, and have not been re-scaled.",
      "\nInterpret results with caution.",
      call. = FALSE
    )
  }


  text <- utils::capture.output(
    model_std <- stats::update(x,
      model.y = y_std, model.m = m_std,
      # control.value = control.value, treat.value = treat.value
      covariates = covs
    )
  )

  model_std
}

#' @keywords internal
.rescale_fixed_values <- function(val, cov_nm,
                                  y_data, m_data, y_data_std, m_data_std) {
  if (cov_nm %in% colnames(y_data)) {
    temp_data <- y_data
    temp_data_std <- y_data_std
  } else {
    temp_data <- m_data
    temp_data_std <- m_data_std
  }

  change_scale(val,
    to = range(temp_data_std[[cov_nm]]),
    range = range(temp_data[[cov_nm]])
  )
}


# Cannot ------------------------------------------------------------------


#' @export
standardize.wbm <- function(x, robust = FALSE, two_sd = FALSE, weights = TRUE, verbose = TRUE, ...) {
  stop(paste0("Standardization of parameters not possible for models of class '", class(x)[1], "'."), call. = FALSE)
}

#' @export
standardize.Surv <- standardize.wbm

#' @export
standardize.clm2 <- standardize.wbm

#' @export
standardize.bcplm <- standardize.wbm

#' @export
standardize.wbgee <- standardize.wbm






# helper ----------------------------

# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.log_terms <- function(model, data) {
  x <- insight::find_terms(model, flatten = TRUE)
  # log_pattern <- "^log\\((.*)\\)"
  log_pattern <- "(log\\(log|log|log1|log10|log1p|log2)\\(([^,\\+)]*).*"
  out <- trimws(gsub(log_pattern, "\\2", x[grepl(log_pattern, x)]))
  intersect(colnames(data), out)
}

# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.sqrt_terms <- function(model, data) {
  x <- insight::find_terms(model, flatten = TRUE)
  pattern <- "sqrt\\(([^,\\+)]*).*"
  out <- trimws(gsub(pattern, "\\1", x[grepl(pattern, x)]))
  intersect(colnames(data), out)
}


#' @keywords internal
.no_response_standardize <- function(info) {
  # check if model has a response variable that should not be standardized.
  !info$is_linear | info$is_censored | info$family == "inverse.gaussian"

  ## TODO alternative would be to keep the below line for checking if no std possible
  ##      and then treat response for "Gamma()" or "inverse.gaussian" similar to log-terms

  # info$is_count | info$is_ordinal | info$is_multinomial | info$is_beta | info$is_censored | info$is_binomial | info$is_survival
}
