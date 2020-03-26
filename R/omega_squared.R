#' @rdname eta_squared
#' @export
omega_squared <- function(model, partial = TRUE, ci = 0.9, ...) {
  UseMethod("omega_squared")
}

eta_squared_adj <- omega_squared

#' @importFrom stats anova
#' @export
omega_squared.aov <- function(model, partial = TRUE, ci = 0.9, ...) {

  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) {
    # Pass to ANOVA table method
    res <- omega_squared.anova(
      stats::anova(model),
      partial = partial,
      ci = ci
    )
    return(res)
  }

  params <- as.data.frame(parameters::model_parameters(model))
  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  values <- .values_aov(params)
  if (isFALSE(partial)) {
    params$Omega_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_total + values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
  } else {
    params$Omega_Sq_partial <- (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq_partial"] <- NA
  }

  out <- params[params$Parameter != "Residuals",
                intersect(c("Group", "Parameter", "Omega_Sq", "Omega_Sq_partial"),
                          names(params)),
                drop = FALSE]

  if (is.numeric(ci)) {
    df_error <- params$df[params$Parameter == "Residuals"]
    params <- params[params$Parameter!="Residuals", , drop = FALSE]

    if (isTRUE(partial)) {
      # use NCP
      omega_ci <- F_to_omega2(
        f = params$`F`,
        df = params$df,
        df_error = df_error,
        ci = ci
      )

      omega_ci$Omega_Sq_partial <- NULL

      out <- cbind(out, omega_ci)
    } else {
      # Make an F value that is just the effect compared to everything else
      SSE <- values$Sum_Squares_total - params$Sum_Squares
      dfE <- values$n - params$df - 1
      MSE <- SSE / dfE

      omega_ci <- F_to_omega2(
        f = params$Mean_Square / MSE,
        df = params$df[params$Parameter!="Residuals"],
        df_error = dfE,
        ci = ci
      )

      omega_ci$Omega_Sq_partial <- NULL

      out <- cbind(out, omega_ci)
    }
  }

  class(out) <- c(ifelse(isTRUE(partial), "partial_omega_squared", "omega_squared"),
                  "effectsize_table",
                  class(out))
  out
}


#' @export
omega_squared.lm <- omega_squared.aov

#' @export
omega_squared.glm <- omega_squared.aov

#' @export
omega_squared.anova <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!"DenDF" %in% colnames(model)) {
    # Pass to AOV method
    res <- omega_squared.aov(model,
                               partial = partial,
                               ci = ci)
    return(res)
  }

  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for mixed models.", call. = FALSE)
  }

  par_table <- as.data.frame(model)

  out <- cbind(
    Parameter = rownames(par_table),
    F_to_omega2(par_table$`F value`,
                  par_table$NumDF,
                  par_table$DenDF,
                  ci = ci
    )
  )

  class(out) <- c("partial_omega_squared", "effectsize_table", class(out))
  out
}



#' @export
omega_squared.aovlist <- function(model, partial = TRUE, ci = 0.9, ...) {

  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for repeated-measures ANOVAs.", call. = FALSE)
  }

  par_table <- as.data.frame(parameters::model_parameters(model))
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    .data$df_error <- .data$df[.data$Parameter == "Residuals"]
    .data
  })
  par_table <- do.call(rbind, par_table)

  par_table <- par_table[par_table$Parameter!="Residuals", , drop = FALSE]


  out <- cbind(
    Parameter = par_table$Parameter,
    F_to_omega2(par_table$`F`,
                  par_table$df,
                  par_table$df_error,
                  ci = ci)
  )

  class(out) <- c("partial_omega_squared", "effectsize_table", class(out))
  out
}


#' @export
omega_squared.merMod <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  omega_squared.anova(model, partial = partial, ci = ci, ...)
}



