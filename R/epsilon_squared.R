#' @rdname eta_squared
#' @export
epsilon_squared <- function(model, partial = TRUE) {
  UseMethod("epsilon_squared")
}

#' @rdname eta_squared
#' @export
eta_squared_adj <- epsilon_squared

#' @export
epsilon_squared.aov <- function(model, partial = TRUE) {
  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) model <- stats::anova(model)
  m <- .epsilon_squared(model, partial = partial)
  class(m) <- c("epsilon_squared", class(m))
  m
}

#' @export
epsilon_squared.lm <- epsilon_squared.aov

#' @export
epsilon_squared.glm <- epsilon_squared.aov

#' @export
epsilon_squared.anova <- function(model, partial = TRUE) {
  if ("DenDF" %in% colnames(model)) {
    if (isFALSE(partial)) {
      warning("Currently only supports partial epsilon squared for mixed models.")
    }
    par_table <- as.data.frame(model)
    par_table$Parameter <- rownames(par_table)
    colnames(par_table)[colnames(par_table) == "NumDF"] <- "df"
    colnames(par_table)[colnames(par_table) == "DenDF"] <- "df2"
    colnames(par_table)[colnames(par_table) == "F value"] <- "F"

    .epsilon_square_from_F(par_table)
  } else {
    epsilon_squared.aov(model, partial = partial)
  }
}


#' @export
epsilon_squared.aovlist <- function(model, partial = TRUE) {
  if (isFALSE(partial)) {
    warning("Currently only supports partial epsilon squared for repeated-measures ANOVAs.")
  }


  par_table <- as.data.frame(parameters::model_parameters(model))
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    .data$df2 <- .data$df[.data$Parameter == "Residuals"]
    .data
  })
  par_table <- do.call(rbind, par_table)
  .epsilon_square_from_F(par_table)
}

#' @export
epsilon_squared.merMod <- function(model, partial = TRUE) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }


  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  epsilon_squared.anova(model, partial = partial)
}

#' @keywords internal
.epsilon_squared <- function(model, partial) {
  params <- as.data.frame(parameters::model_parameters(model))
  values <- .values_aov(params)

  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  .extract_epsilon_squared(params, values, partial)
}


#' @keywords internal
.extract_epsilon_squared <- function(params, values, partial) {
  if (partial == FALSE) {
    params$Epsilon_sq <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
      values$Sum_Squares_total
    params[params$Parameter == "Residuals", "Epsilon_sq"] <- NA
  } else {
    params$Epsilon_sq_partial <- (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
      (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Epsilon_sq_partial"] <- NA
  }

  params[, intersect(c("Group", "Parameter", "Epsilon_sq", "Epsilon_sq_partial"), names(params)), drop = FALSE]
}

#' @keywords internal
.epsilon_square_from_F <- function(.data) {
  .data <- cbind(.data, F_to_epsilon2(.data$`F`, .data$df, .data$df2, ci))

  rownames(.data) <- NULL
  .data <- .data[, colnames(.data) %in% c("Parameter", "Epsilon_Sq_partial", "CI", "CI_low", "CI_high")]
  class(.data) <- c("partial_epsilon_squared", class(.data))
  .data
}