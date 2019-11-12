#' @rdname eta_squared
#' @export
omega_squared <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  UseMethod("omega_squared")
}



#' @export
omega_squared.aov <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) model <- stats::anova(model)
  m <- .omega_squared(model, partial = partial, ci = ci, iterations = iterations)
  class(m) <- c(ifelse(isTRUE(partial), "partial_omega_squared", "omega_squared"), class(m))
  m
}

#' @export
omega_squared.lm <- omega_squared.aov

#' @export
omega_squared.glm <- omega_squared.aov

#' @export
omega_squared.anova <- function(model, partial = TRUE, ci = NULL, iterations = 1000, ...) {
  if ("DenDF" %in% colnames(model)) {
    if (isFALSE(partial)) {
      warning("Currently only supports partial omega squared for mixed models.")
    }
    par_table <- as.data.frame(model)
    par_table$Parameter <- rownames(par_table)
    colnames(par_table)[colnames(par_table) == "NumDF"] <- "df"
    colnames(par_table)[colnames(par_table) == "DenDF"] <- "df2"
    colnames(par_table)[colnames(par_table) == "F value"] <- "F"

    .omega_square_from_F(par_table, ci = ci)
  } else {
    omega_squared.aov(model, partial = partial, ci = ci, iterations = iterations, ...)
  }
}

#' @export
omega_squared.aovlist <- function(model, partial = TRUE, ci = NULL, iterations = 1000) {
  if (isFALSE(partial)) {
    warning("Currently only supports partial omega squared for repeated-measures ANOVAs.")
  }


  par_table <- as.data.frame(parameters::model_parameters(model))
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    .data$df2 <- .data$df[.data$Parameter == "Residuals"]
    .data
  })
  par_table <- do.call(rbind, par_table)
  .omega_square_from_F(par_table, ci = ci)
}

#' @export
omega_squared.merMod <- function(model, partial = TRUE, ci = NULL) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }


  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  omega_squared.anova(model, partial = partial, ci = ci)
}


#' @keywords internal
.omega_squared <- function(model, partial, ci, iterations) {
  params <- as.data.frame(parameters::model_parameters(model))
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Omega squared can only be computed for simple `aov` models.")
  }

  eff_size <- .extract_omega_squared(params, values, partial)

  .ci_omega_squared(
    x = eff_size,
    partial = partial,
    ci.lvl = ci,
    df = params[["df"]],
    statistic = params[["F"]],
    model = model,
    iterations = iterations
  )
}

#' @keywords internal
.extract_omega_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params$Omega_Sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_total + values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
  } else {
    params$Omega_Sq_partial <- (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    params[params$Parameter == "Residuals", "Omega_Sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Omega_Sq", "Omega_Sq_partial"), names(params)), drop = FALSE]
}




#' @importFrom stats aov quantile
#' @importFrom insight get_data find_formula
#' @keywords internal
.ci_omega_squared <- function(x, partial, ci.lvl, df, statistic, model, iterations) {
  if (is.null(ci.lvl) || is.na(ci.lvl)) {
    return(x)
  }
  N <- sum(df) + 1

  if (partial == FALSE) {
    ci_omega <- lapply(
      1:nrow(x),
      function(.x) {
        if (!is.na(statistic[.x])) {
          ci <- .confint_ncg(
            F.value = statistic[.x],
            conf.level = ci.lvl,
            df.1 = df[.x],
            df.2 = df[nrow(x)]
          )
          ci.low <- ci$Lower.Limit / (ci$Lower.Limit + N)
          ci.high <- ci$Upper.Limit / (ci$Upper.Limit + N)
        } else {
          ci.low <- ci.high <- NA
        }

        data.frame(
          CI_low = ci.low,
          CI_high = ci.high
        )
      }
    )
    cbind(x, do.call(rbind, ci_omega))
  } else {
    if (inherits(model, "anova") || is.data.frame(model)) {
      warning("Confidence intervals can't be computed for data frames or objects of class 'anova'.")
      return(x)
    }

    if (!requireNamespace("boot", quietly = TRUE)) {
      stop("Package 'boot' needed for this function to work. Please install it.")
    }

    dat <- insight::get_data(model)

    boot_function <- function(model, data, indices) {
      d <- data[indices, ] # allows boot to select sample
      fit <- stats::aov(insight::find_formula(model)$conditional, data = d)
      params <- as.data.frame(parameters::model_parameters(fit))
      values <- .values_aov(params)
      osq <- .extract_omega_squared(params, values, partial = TRUE)
      return(osq[["Omega_Sq_partial"]])
    }

    results <- boot::boot(
      data = dat,
      statistic = boot_function,
      R = iterations,
      model = model,
      parallel = "multicore"
    )

    df <- as.data.frame(results$t)
    x$CI_low <- sapply(df[, 1:ncol(df)], stats::quantile, probs = (1 - ci.lvl) / 2, na.rm = TRUE)
    x$CI_high <- sapply(df[, 1:ncol(df)], stats::quantile, probs = (1 + ci.lvl) / 2, na.rm = TRUE)
    x
  }
}

.omega_square_from_F <- function(.data, ci = NULL) {
  .data$Omega_Sq_partial <- F_to_omega2(.data$`F`, .data$df, .data$df2)

  if (is.numeric(ci)) {
    warning("CI not implemented yet for Partial Omega squared.")
  }

  rownames(.data) <- NULL
  .data <- .data[, colnames(.data) %in% c("Parameter", "Omega_Sq_partial")]
  class(.data) <- c("partial_eta_squared", class(.data))
  .data
}
