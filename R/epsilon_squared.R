#' @rdname eta_squared
#' @export
epsilon_squared <- function(model, partial = TRUE, ci = 0.9, ...) {
  UseMethod("epsilon_squared")
}

eta_squared_adj <- epsilon_squared

#' @importFrom stats anova
#' @export
epsilon_squared.aov <- function(model, partial = TRUE, ci = 0.9, ...) {

  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) {
    # Pass to ANOVA table method
    res <- epsilon_squared.anova(
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
    params$Epsilon_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
      values$Sum_Squares_total
    params[params$Parameter == "Residuals", "Epsilon_sq"] <- NA
  } else {
    params$Epsilon_Sq_partial <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
      (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Epsilon_sq_partial"] <- NA
  }

  out <- params[params$Parameter != "Residuals",
                intersect(c("Group", "Parameter", "Epsilon_Sq", "Epsilon_Sq_partial"),
                          names(params)),
                drop = FALSE]

  if (is.numeric(ci)) {
    df_error <- params$df[params$Parameter == "Residuals"]
    params <- params[params$Parameter!="Residuals", , drop = FALSE]

    if (isTRUE(partial)) {
      # use NCP
      epsilon_ci <- F_to_epsilon2(
        f = params$`F`,
        df = params$df,
        df_error = df_error,
        ci = ci
      )

      epsilon_ci$Epsilon_Sq_partial <- NULL

      out <- cbind(out, epsilon_ci)
    } else {
      # Make an F value that is just the effect compared to everything else
      SSE <- values$Sum_Squares_total - params$Sum_Squares
      dfE <- values$n - params$df - 1
      MSE <- SSE / dfE

      epsilon_ci <- F_to_epsilon2(
        f = params$Mean_Square / MSE,
        df = params$df[params$Parameter!="Residuals"],
        df_error = dfE,
        ci = ci
      )

      epsilon_ci$Epsilon_Sq_partial <- NULL

      out <- cbind(out, epsilon_ci)
    }
  }

  class(out) <- c(ifelse(isTRUE(partial), "partial_epsilon_squared", "epsilon_squared"),
                  "effectsize_table",
                  class(out))
  out
}


#' @export
epsilon_squared.lm <- epsilon_squared.aov

#' @export
epsilon_squared.glm <- epsilon_squared.aov

#' @export
epsilon_squared.anova <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!"DenDF" %in% colnames(model)) {
    # Pass to AOV method
    res <- epsilon_squared.aov(model,
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
    F_to_epsilon2(par_table$`F value`,
                  par_table$NumDF,
                  par_table$DenDF,
                  ci = ci
    )
  )

  class(out) <- c("partial_epsilon_squared", "effectsize_table", class(out))
  out
}



#' @export
epsilon_squared.aovlist <- function(model, partial = TRUE, ci = 0.9, ...) {

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
    F_to_epsilon2(par_table$`F`,
                  par_table$df,
                  par_table$df_error,
                  ci = ci)
  )

  class(out) <- c("partial_epsilon_squared", "effectsize_table", class(out))
  out
}


#' @export
epsilon_squared.merMod <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  epsilon_squared.anova(model, partial = partial, ci = ci, ...)
}
