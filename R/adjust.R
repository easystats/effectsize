#' Adjust data for the effect of other variable(s)
#'
#' This function can be used to adjust the data for the effect of other
#' variables present in the dataset. It is based on an underlying fitting of
#' regressions models, allowing for quite some flexibility, such as including
#' factors as random effects in mixed models (multilevel partialization),
#' continuous variables as smooth terms in general additive models (non-linear
#' partialization) and/or fitting these models under a Bayesian framework. The
#' values returned by this function are the residuals of the regression models.
#' Note that a regular correlation between two "adjusted" variables is
#' equivalent to the partial correlation between them.
#'
#' @param data A dataframe.
#' @param effect Character vector of column names to be adjusted for (regressed
#'   out). If `NULL` (the default), all variables will be selected.
#' @inheritParams standardize
#' @param multilevel If `TRUE`, the factors are included as random factors.
#'   Else, if `FALSE` (default), they are included as fixed effects in the
#'   simple regression model.
#' @param additive If `TRUE`, continuous variables as included as smooth terms
#'   in additive models. The goal is to regress-out potential non-linear
#'   effects.
#' @param bayesian If `TRUE`, the models are fitted under the Bayesian framework
#'   using `rstanarm`.
#' @param keep_intercept If `FALSE` (default), the intercept of the model is re-added.
#'
#' @return A data frame comparable to `data`, with adjusted variables.
#'
#' @examples
#' adjust(attitude)
#' adjust(attitude, effect = "complaints", select = "rating")
#'
#' \donttest{
#' \dontrun{
#' adjust(attitude, effect = "complaints", select = "rating", bayesian = TRUE)
#' adjust(attitude, effect = "complaints", select = "rating", additive = TRUE)
#' attitude$complaints_LMH <- cut(attitude$complaints, 3)
#' adjust(attitude, effect = "complaints_LMH", select = "rating", multilevel = TRUE)
#' }
#' }
#' if(require("bayestestR")){
#' # Generate data
#' data <- bayestestR::simulate_correlation(n=100, r=0.7)
#' data$V2 <- (5 * data$V2) + 20  # Add intercept
#'
#' # Adjust
#' adjusted <- adjust(data, effect="V1", select="V2")
#' adjusted_icpt <- adjust(data, effect="V1", select="V2", keep_intercept=TRUE)
#'
#' # Visualize
#' plot(data$V1, data$V2, pch = 19, col = "blue", ylim=c(min(adjusted$V2), max(data$V2)))
#' abline(lm(V2 ~ V1, data = data), col = "blue")
#' points(adjusted$V1, adjusted$V2, pch = 19, col = "green")
#' abline(lm(V2 ~ V1, data = adjusted), col = "green")
#' points(adjusted_icpt$V1, adjusted_icpt$V2, pch = 19, col = "red")
#' abline(lm(V2 ~ V1, data = adjusted_icpt), col = "red")
#' }
#' @export
adjust <- function(data, effect = NULL, select = NULL, exclude = NULL, multilevel = FALSE, additive = FALSE, bayesian = FALSE, keep_intercept = FALSE) {
  if (!all(colnames(data) == make.names(colnames(data), unique = TRUE))) {
    warning("Bad column names (e.g., with spaces) have been detected which might create issues in many functions.\n",
      "Please fix it (you can run `names(mydata) <- make.names(names(mydata))` for a quick fix).",
      call. = FALSE
    )
  }

  # check for formula notation, convert to character vector
  if (inherits(effect, "formula")) {
    effect <- all.vars(effect)
  }
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  # Find predictors
  if (is.null(effect)) {
    effect <- names(data)
  }

  # Factors
  formula_random <- NULL
  facs <- names(data[effect][!sapply(data[effect], is.numeric)])
  if (length(facs) >= 1) {
    if (multilevel) {
      if (additive) {
        formula_random <- as.formula(paste("~", paste(paste0("(1|", facs, ")"), collapse = " + ")))
      } else {
        formula_random <- paste("+", paste(paste0("(1|", facs, ")"), collapse = " + "))
      }
      effect <- effect[!effect %in% facs]
    }
  }

  nums <- sapply(data, is.numeric)
  # Find outcomes
  if (is.null(select)) {
    select <- names(data[nums])
  }
  if (!is.null(exclude)) {
    select <- select[!select %in% c(exclude)]
  }

  # Fit models
  out <- data.frame(.ID = 1:nrow(data))
  for (var in select) {
    predictors <- effect[effect != var]
    if (additive) {
      predictors_num <- names(data[predictors][sapply(data[predictors], is.numeric)])
      predictors[predictors == predictors_num] <- paste0("s(", predictors_num, ")")
    }
    formula_predictors <- paste(c("1", predictors), collapse = " + ")
    formula <- paste(var, "~", formula_predictors)

    x <- .model_adjust_for(data = data[unique(c(var, effect, facs))], formula, multilevel = multilevel, additive = additive, bayesian = bayesian, formula_random = formula_random, keep_intercept = keep_intercept)
    out[var] <- x
  }
  out[names(data)[!names(data) %in% names(out)]] <- data[names(data)[!names(data) %in% names(out)]]
  out[names(data)]
}

#' @rdname adjust
#' @export
data_adjust <- adjust


#' @importFrom stats lm residuals as.formula complete.cases
#' @keywords internal
.model_adjust_for <- function(data, formula, multilevel = FALSE, additive = FALSE, bayesian = FALSE, formula_random = NULL, keep_intercept = FALSE) {

  # Additive -----------------------
  if (additive) {
    # Bayesian
    if (bayesian) {
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      model <- rstanarm::stan_gamm4(as.formula(formula), random = formula_random, data = data, refresh = 0)
      # Frequentist
    } else {
      if (!requireNamespace("gamm4")) {
        stop("This function needs `gamm4` to be installed. Please install by running `install.packages('gamm4')`.")
      }
      model <- gamm4::gamm4(as.formula(formula), random = formula_random, data = data)
    }

    # Linear -------------------------
  } else {
    # Bayesian
    if (bayesian) {
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      if (multilevel) {
        model <- rstanarm::stan_lmer(paste(formula, formula_random), data = data, refresh = 0)
      } else {
        model <- rstanarm::stan_glm(formula, data = data, refresh = 0)
      }
      # Frequentist
    } else {
      if (multilevel) {
        if (!requireNamespace("lme4")) {
          stop("This function needs `lme4` to be installed. Please install by running `install.packages('lme4')`.")
        }
        model <- residuals(lme4::lmer(paste(formula, formula_random), data = data))
      } else {
        model <- lm(formula, data = data)
      }
    }
  }

  adjusted <- insight::get_residuals(model)

  # Re-add intercept if need be
  if(keep_intercept){
    intercept <- insight::get_intercept(model)
    if(length(intercept) > 1) intercept <- median(intercept)  # For bayesian model
    if(is.na(intercept)) intercept <- 0
    adjusted <- adjusted + intercept
  }

  # Deal with missing data
  out <- rep(NA, nrow(data))
  out[complete.cases(data)] <- as.vector(adjusted)

  out
}
