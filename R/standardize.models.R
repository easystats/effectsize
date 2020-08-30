#' @rdname standardize
#' @importFrom stats update
#' @importFrom insight get_data model_info find_response get_response find_weights
#' @importFrom utils capture.output
#' @export
standardize.default <- function(x, robust = FALSE, two_sd = FALSE, include_response = TRUE, verbose = TRUE, ...) {
  m_info <- insight::model_info(x)
  data <- insight::get_data(x)
  resp <- NULL

  # for models with specific scale of the response value (e.g. count models
  # with positive integers, or beta with ratio between 0 and 1), we need to
  # make sure that the original response value will be restored after
  # standardizing, as these models also require a non-standardized response.

  if (.no_response_standardize(m_info) || !include_response) {
    resp <- unique(c(insight::find_response(x), insight::find_response(x, combine = FALSE)))
  }


  # Do not standardize weighting-variable, because negative weights will
  # cause errors in "update()"

  weight_variable <- insight::find_weights(x)

  # don't standardize random effects

  random_group_factor <- insight::find_random(x, flatten = TRUE, split_nested = TRUE)

  # standardize data

  dont_standardize <- c(resp, weight_variable, random_group_factor)
  do_standardize <- setdiff(colnames(data), dont_standardize)

  if (length(do_standardize)) {
    data_std <- standardize(data[do_standardize], robust = robust, two_sd = two_sd, verbose = verbose)
  } else {
    if (verbose) {
      insight::print_color("No variables could be standardized.\n", "red")
    }
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

  if (length(log_terms) > 0 || length(sqrt_terms) > 0) {
    message("Formula contains log- or sqrt-terms. See help(\"standardize\") for how such terms are standardized.")
  }


  # restore data that should not be standardized

  if (length(dont_standardize)) {
    data_std <- cbind(data[, dont_standardize, drop = FALSE], data_std)
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




#' @keywords internal
.no_response_standardize <- function(info) {
  # check if model has a response variable that should not be standardized.
  info$is_count | info$is_ordinal | info$is_multinomial | info$is_beta | info$is_censored | info$is_binomial | info$is_survival
}





# exceptions, models that cannot use the default-method --------------------


#' @export
standardize.mlm <- function(x, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {
  standardize.default(x = x, robust = robust, two_sd = two_sd, include_response = FALSE, verbose = verbose, ...)
}

#' @export
standardize.wbm <- function(x, ...) {
  warning(paste0("Standardization of parameters not possible for models of class '", class(x)[1], "'."), call. = FALSE)
  x
}

#' @export
standardize.Surv <- function(x, ...) {
  insight::print_color("'Surv' objects cannot be standardized.\n", "red")
  x
}

#' @export
standardize.clm2 <- standardize.wbm

#' @export
standardize.bcplm <- standardize.wbm

#' @export
standardize.wbgee <- standardize.wbm




# models with special handling of response variables ---------------------------


#' @export
standardize.coxph <- function(x, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {

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
    data_std <- standardize(data[, pred, drop = FALSE], robust = robust, two_sd = two_sd, verbose = verbose)
    data[pred] <- data_std
  }

  text <- utils::capture.output(model_std <- stats::update(x, data = data))

  model_std
}


#' @export
standardize.coxme <- standardize.coxph






# helper ----------------------------

# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.log_terms <- function(model, data) {
  x <- insight::find_terms(model, flatten = TRUE)
  # log_pattern <- "^log\\((.*)\\)"
  log_pattern <- "(log\\(log|log|log1|log10|log1p|log2)\\(([^,)]*).*"
  out <- gsub(log_pattern, "\\2", x[grepl(log_pattern, x)])
  intersect(colnames(data), out)
}

# Find log-terms inside model formula, and return "clean" term names
#' @importFrom insight find_terms
.sqrt_terms <- function(model, data) {
  x <- insight::find_terms(model, flatten = TRUE)
  pattern <- "sqrt\\(([^,)]*).*"
  out <- gsub(pattern, "\\1", x[grepl(pattern, x)])
  intersect(colnames(data), out)
}
