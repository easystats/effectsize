#' Get Standardization Information
#'
#' This function extracts information, such as the deviations (SD or MAD) from parent variables, that are necessary for post-hoc standardization of parameters. This function gives a window on how standardized are obtained, i.e., by what they are devided. The "basic" method of standardization uses
#'
#' @inheritParams standardize_parameters
#'
#' @examples
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
#' standardize_info(model)
#' @export
standardize_info <- function(model, robust = FALSE, ...) {
  params <- insight::find_parameters(model, effects = "fixed", flatten = TRUE, ...)
  types <- parameters::parameters_type(model)
  model_matrix <- as.data.frame(stats::model.matrix(model))
  data <- insight::get_data(model)

  out <- data.frame(
    Parameter = params,
    Type = types$Type,
    Link = types$Link,
    Secondary_Parameter = types$Secondary_Parameter,
    stringsAsFactors = FALSE
  )

  # Type of effect size
  out$EffectSize_Type <- ifelse(types$Type == "interaction", "interaction",
    ifelse(types$Link == "Association", "r",
      ifelse(types$Link == "Difference", "d", NA)
    )
  )




  # Response - Basic
  out <- merge(
    out,
    .std_info_response_basic(model, params, robust = robust)
  )

  # Response - Smart
  out <- merge(
    out,
    .std_info_response_smart(model, data, model_matrix, types, robust = robust)
  )

  # Basic
  out <- merge(
    out,
    .std_info_predictors_basic(model_matrix, types, robust = robust)
  )

  # Smart
  out <- merge(
    out,
    .std_info_predictors_smart(data, params, types, robust = robust)
  )

  # Reorder
  out <- out[match(params, out$Parameter), ]
  row.names(out) <- NULL

  # Remove all means for now (because it's not used)
  out <- out[!grepl("Mean_", names(out))]

  # Select only desired columns
  # if(method == "all") method <- c("smart", "basic")
  # if(!any(method == "smart")){
  #   out <- out[!grepl("_Smart", names(out))]
  # }
  # if(!any(method == "basic")){
  #   out <- out[!grepl("_Basic", names(out))]
  # }

  out
}




# Predictors - Smart ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_smart <- function(data, params, types, robust = FALSE, ...) {

  # Get deviations for all parameters
  deviations <- c()
  means <- c()
  for (var in params) {
    info <- .std_info_predictor_smart(
      data = data,
      variable = types[types$Parameter == var, "Variable"],
      type = types[types$Parameter == var, "Type"],
      robust = robust
    )
    deviations <- c(deviations, info$sd)
    means <- c(means, info$mean)
  }

  # Out
  data.frame(
    Parameter = params,
    Deviation_Smart = deviations,
    Mean_Smart = means
  )
}



#' @keywords internal
.std_info_predictor_smart <- function(data, variable, type, robust = FALSE, ...) {
  if (type == "intercept") {
    info <- list(sd = 0, mean = 0)
  } else if (type == "numeric") {
    info <- .compute_std_info(data = data, variable = variable, robust = robust)
  } else if (type == "factor") {
    info <- list(sd = 1, mean = 0)

    # TO BE IMPROVED: Adjust if involved in interactions
    # interactions <- types[types$Type %in% c("interaction"), ]
    # if(variable %in% interactions$Secondary_Variable){
    #   interac_var <- unique(interactions[interactions$Secondary_Variable == variable, "Variable"])
    #   for(i in interac_var){
    #     if(types[types$Parameter == i, "Type"] == "numeric"){
    #       sd_x <- sd_x * .get_deviation(data, i, robust)
    #     }
    #   }
    # }
  } else if (type %in% c("interaction", "nested")) {
    if (is.numeric(data[, variable])) {
      info <- .compute_std_info(data = data, variable = variable, robust = robust)
    } else if (is.factor(data[, variable])) {
      info <- list(sd = 1, mean = 0)
    } else {
      info <- list(sd = 1, mean = 0)
    }
  } else {
    info <- list(sd = 1, mean = 0)
  }

  list(sd = info$sd, mean = info$mean)
}


# Predictors - Basic ------------------------------------------------------------


#' @keywords internal
.std_info_predictors_basic <- function(model_matrix, types, robust = FALSE, ...) {

  # Get deviations for all parameters
  deviations <- c()
  means <- c()
  for (var in names(model_matrix)) {
    if (types[types$Parameter == var, "Type"] == "intercept") {
      deviations <- c(deviations, 0)
      means <- c(means, 0)
    } else {
      std_info <- .compute_std_info(data = model_matrix, variable = var, robust = robust)
      deviations <- c(deviations, std_info$sd)
      means <- c(means, std_info$mean)
    }
  }

  # Out
  data.frame(
    Parameter = names(model_matrix),
    Deviation_Basic = deviations,
    Mean_Basic = means
  )
}





# Response ------------------------------------------------------------

#' @keywords internal
.std_info_response_smart <- function(model, data, model_matrix, types, robust = FALSE, ...) {
  info <- insight::model_info(model)

  if (info$is_linear) {
    response <- insight::get_response(model)
    deviations <- c()
    means <- c()
    for (var in names(model_matrix)) {
      if (types$Link[types$Parameter == var] == "Difference") {
        parent_var <- types$Variable[types$Parameter == var]
        intercept <- unique(data[[parent_var]])[1]
        response_at_intercept <- response[data[[parent_var]] == intercept]
        std_info <- .compute_std_info(response = response_at_intercept, robust = robust)
        deviations <- c(deviations, std_info$sd)
        means <- c(means, std_info$mean)
      } else {
        std_info <- .compute_std_info(response = response, robust = robust)
        deviations <- c(deviations, std_info$sd)
        means <- c(means, std_info$mean)
      }
    }
  } else {
    deviations <- 1
    means <- 0
  }

  # Out
  data.frame(
    Parameter = names(model_matrix),
    Deviation_Response_Smart = deviations,
    Mean_Response_Smart = means
  )
}



#' @keywords internal
.std_info_response_basic <- function(model, params, robust = FALSE, ...) {
  info <- insight::model_info(model)
  response <- insight::get_response(model)

  if (info$is_linear) {
    if (robust == FALSE) {
      sd_y <- stats::sd(response)
      mean_y <- mean(response)
    } else {
      sd_y <- stats::mad(response)
      mean_y <- stats::median(response)
    }
  } else {
    sd_y <- 1
    mean_y <- 0
  }

  # Out
  data.frame(
    Parameter = params,
    Deviation_Response_Basic = sd_y,
    Mean_Response_Basic = mean_y
  )
}





# Utils -------------------------------------------------------------------


#' @keywords internal
.compute_std_info <- function(data = NULL, variable = NULL, response = NULL, robust = FALSE) {
  if (is.null(response)) {
    response <- as.numeric(data[, variable])
  }

  if (robust == FALSE) {
    sd_x <- stats::sd(response, na.rm = TRUE)
    mean_x <- mean(response, na.rm = TRUE)
  } else {
    sd_x <- stats::mad(response, na.rm = TRUE)
    mean_x <- stats::median(response, na.rm = TRUE)
  }

  list(sd = sd_x, mean = mean_x)
}
