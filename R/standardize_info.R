#' Get Standardization Information
#'
#' This function extracts information, such as the deviations (SD or MAD) from parent variables, that are necessary for post-hoc standardization of parameters. This function gives a window on how standardized are obtained, i.e., by what they are devided. The "basic" method of standardization uses
#'
#' @inheritParams standardize_parameters
#' @param include_pseudo (For (G)LMMs) Should Pseudo-standardized information be included?
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' model <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
#' @importFrom parameters parameters_type
#' @export
standardize_info <- function(model, robust = FALSE, include_pseudo = FALSE, ...) {
  params <- insight::find_parameters(model, effects = "fixed", flatten = TRUE, ...)
  types <- parameters::parameters_type(model)
  model_matrix <- as.data.frame(stats::model.matrix(model))
  data <- insight::get_data(model)

  # Sanity Check for ZI
  if (insight::model_info(model)$is_zero_inflated) {
    warning("Non-refit parameter standardization is ignoring the zero-inflation component.", call. = FALSE)
    # would need to also get the binomial model matrix...
  }

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
    .std_info_response_basic(model, params, robust = robust),
    by = "Parameter", all = TRUE
  )

  # Response - Smart
  out <- merge(
    out,
    .std_info_response_smart(model, data, model_matrix, types, robust = robust),
    by = "Parameter", all = TRUE
  )

  # Basic
  out <- merge(
    out,
    .std_info_predictors_basic(model_matrix, types, robust = robust),
    by = "Parameter", all = TRUE
  )

  # Smart
  out <- merge(
    out,
    .std_info_predictors_smart(data, params, types, robust = robust),
    by = "Parameter", all = TRUE
  )

  # Pseudo (for LMM)
  if (include_pseudo &&
      insight::model_info(model)$is_mixed &&
      length(insight::find_random(model)$random) == 1) {
    out <- merge(
      out,
      .std_info_pseudo(model, params, model_matrix, types = types$Type, robust = robust)
    )
  }

  # Reorder
  out <- out[match(params, out$Parameter), ]
  out$Parameter <- params
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
  means <- deviations <- rep(NA_real_, times = length(params))
  for (i in seq_along(params)) {
    var <- params[i]
    info <- .std_info_predictor_smart(
      data = data,
      variable = types[types$Parameter == var, "Variable"],
      type = types[types$Parameter == var, "Type"],
      robust = robust
    )
    deviations[i] <- info$sd
    means[i] <- info$mean
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
  means <- deviations <- rep(NA_real_, length = length(names(model_matrix)))
  for (i in seq_along(names(model_matrix))) {
    var <- names(model_matrix)[i]
    if (types[i, "Type"] == "intercept") {
      means[i] <- deviations[i] <- 0
    } else {
      std_info <- .compute_std_info(data = model_matrix, variable = var, robust = robust)
      deviations[i] <- std_info$sd
      means[i] <- std_info$mean
    }
  }

  # Out
  data.frame(
    Parameter = types$Parameter[seq_along(names(model_matrix))],
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
    means <- deviations <- rep(NA_real_, length = length(names(model_matrix)))
    for (i in seq_along(names(model_matrix))) {
      var <- names(model_matrix)[i]
      if (types$Link[types$Parameter == var] == "Difference") {
        parent_var <- types$Variable[types$Parameter == var]
        intercept <- unique(data[[parent_var]])[1]
        response_at_intercept <- response[data[[parent_var]] == intercept]
        std_info <- .compute_std_info(response = response_at_intercept, robust = robust)
      } else {
        std_info <- .compute_std_info(response = response, robust = robust)
      }
      deviations[i] <- std_info$sd
      means[i] <- std_info$mean
    }
  } else {
    deviations <- 1
    means <- 0
  }

  # Out
  data.frame(
    Parameter = types$Parameter[seq_along(names(model_matrix))],
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



# Pseudo (GLMM) -----------------------------------------------------------


#' @importFrom insight clean_names get_random model_info find_formula get_variance get_data
#' @importFrom parameters check_heterogeneity demean
#' @importFrom stats as.formula sd
.std_info_pseudo <- function(model, params, model_matrix, types, robust = FALSE) {
  if (robust) {
    warning("'robust' standardization not available for 'pseudo' method.",
            call. = FALSE)
  }

  within_vars <- unclass(parameters::check_heterogeneity(model))
  id <- insight::get_random(model)[[1]]

  ## Find which parameters vary on level 1 ("within")
  is_within <- logical(length = length(params))
  is_within[] <- NA
  for (i in seq_along(params)) {
    if (types[i] == "intercept") {
      is_within[i] <- FALSE
    } else if (types[i] == "numeric") {
      is_within[i] <- insight::clean_names(params[i]) %in% within_vars
    } else if (types[i] == "factor") {
      is_within[i] <- any(sapply(paste0("^",within_vars), grepl, insight::clean_names(params[i])))
    } else if (types[i] == "interaction") {
      ints <- unlist(strsplit(params[i], ":", fixed = TRUE))
      is_within[i] <- any(sapply(ints, function(int) {
        int <- insight::clean_names(int)
        int %in% within_vars | # numeric
          any(sapply(paste0("^",within_vars), grepl, int)) # factor
      }))
    }
  }

  ## test "within"s are fully "within"
  # only relevant to numeric predictors that can have variance
  if (any(check_within <- is_within & types == "numeric")) {
    p_check_within <- params[check_within]
    temp_d <- data.frame(model_matrix[,p_check_within,drop = FALSE])
    colnames(temp_d) <- paste0("W",seq_len(ncol(temp_d))) # overwrite because can't deal with ":"

    dm <- parameters::demean(cbind(id,temp_d),
                             select = colnames(temp_d),
                             group = "id")
    dm <- dm[,paste0(colnames(temp_d), "_between"), drop = FALSE]

    has_lvl2_var <- sapply(seq_along(colnames(temp_d)), function (i) {
      # If more than 1% of the variance in the within-var is between:
      var(dm[,i]) /
         var(temp_d[,i])
    }) > 0.01
    also_between <- p_check_within[has_lvl2_var]

    if (length(also_between)) {
      warning(
        "The following within-group terms have between-group variance:\n\t",
        paste0(also_between, collapse = ", "),
        "\nThis can inflate standardized within-group parameters associated with",
        "\nthese terms. See help(\"demean\", package = \"parameters\") for modeling",
        "\nbetween- and within-subject effects.",
        call. = FALSE
      )
    }
  }


  ## Get 2 types of Deviation_Response_Pseudo
  sd_y_within <- sd_y_between <- 1
  if (insight::model_info(model)$is_linear) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("This function requires 'lme4' to work.", call. = FALSE)
    }
    rand_name <- insight::find_random(model)$random

    # maintain any y-transformations
    f <- insight::find_formula(model)
    f <- paste0(f$conditional[2], " ~ (1|",rand_name,")")

    m0 <- suppressWarnings(suppressMessages(
      lme4::lmer(stats::as.formula(f),
                 data = insight::get_data(model))
    ))
    m0v <- insight::get_variance(m0)

    sd_y_between <- unname(sqrt(m0v$var.intercept))
    sd_y_within <- unname(sqrt(m0v$var.residual))
  }


  ## Get scaling factors for each parameter
  Deviation_Response_Pseudo <- Deviation_Pseudo <- numeric(ncol(model_matrix))
  for (i in seq_along(params)) {
    if (types[i] == "intercept") {
      Deviation_Response_Pseudo[i] <- Deviation_Pseudo[i] <- NA
    } else {
      ## dumb way
      if (is_within[i]) {
        ## is within
        X <- model_matrix[[i]]
        Deviation_Response_Pseudo[i] <- sd_y_within
      } else {
        ## is between
        X <- tapply(model_matrix[[i]], id, mean)
        Deviation_Response_Pseudo[i] <- sd_y_between
      }
      Deviation_Pseudo[i] <- stats::sd(X)

      ## smart way?
      ## DONT USE: see corespondance with between Mattan and Eran BC
      # m <- suppressWarnings(suppressMessages(lme4::lmer(model_matrix[[i]] ~ (1|id))))
      # if (is_within[i]) {
      #   ## is within
      #   Deviation_Pseudo[i] <- sqrt(unname(unlist(suppressWarnings(
      #     insight::get_variance(m, component = "residual")
      #   ))))
      #   Deviation_Response_Pseudo[i] <- sd_y_within
      # } else {
      #   ## is between
      #   Deviation_Pseudo[i] <- sqrt(unname(unlist(suppressWarnings(
      #     insight::get_variance(m, component = "intercept")
      #   ))))
      #   Deviation_Response_Pseudo[i] <- sd_y_between
      # }
    }
  }

  data.frame(
    Parameter = params,
    Deviation_Response_Pseudo,
    Deviation_Pseudo
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
