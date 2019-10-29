#' Automated Interpretation of Effect Sizes
#'
#' Automated interpretation of effect sizes.
#'
#' @inheritParams standardize_parameters
#' @param interpretation Interpretation grid (i.e., the set of rules of thumb) used to interpret the effects.
#' @param parameters A custom parameters table. If \code{NULL}, will use \code{\link{standardize_parameters}} to get it.
#' @param standardize_method See \code{\link{standardize_parameters}}.
#' @param standardize_robust See \code{\link{standardize_parameters}}.
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#'
#' @export
interpret_parameters <- function(model, ...) {
  UseMethod("interpret_parameters")
}




#' @rdname interpret_parameters
#' @export
interpret_parameters.lm <- function(model, parameters = NULL, interpretation = "funder2019", standardize_method = "refit", standardize_robust = FALSE, ...) {
  .interpret_parameters_regressions(model, interpretation = interpretation, parameters = parameters, standardize_method = standardize_method, standardize_robust = standardize_robust)
}







#' @keywords internal
.interpret_parameters_regressions <- function(model, parameters = NULL, interpretation = "funder2019", standardize_method = "refit", standardize_robust = FALSE, ...) {
  type <- parameters::parameters_type(model)
  std_es <- .standardize_standardized(model, standardize_method = standardize_method, standardize_robust = standardize_robust, type = type, centrality = "Median")

  data.frame(
    Parameter = type$Parameter,
    Effect_Size = std_es,
    Interpretation = interpret_r(std_es, rules = interpretation)
  )
}








#' @keywords internal
.standardize_standardized <- function(model, parameters = NULL, standardize_method = "refit", standardize_robust = FALSE, type = NULL, centrality = "Median", ...) {

  # Get type of parameters
  info <- standardize_info(model, robust = standardize_robust)

  # Compute std parameters
  if (is.null(parameters)) {
    parameters <- standardize_parameters(model, method = standardize_method, robust = standardize_robust, centrality = centrality, ...)
  }

  # Standardize standardized parameters (Correlation r)
  std_es <- parameters[names(parameters) %in% c("Std_Coefficient", "Std_Median", "Std_Mean", "Std_MAP")][[1]]

  #---- Non-applicable
  std_es[is.na(info$EffectSize_Type)] <- NA

  #---- Interactions
  out <- .standardize_standardized_interactions(model, info, type, std_es, centrality, method = "absolute", ...)
  std_es <- out$std_es
  info <- out$info

  #---- Cohen's d
  d <- std_es[!is.na(info$EffectSize_Type) & info$EffectSize_Type == "d"]
  std_es[!is.na(info$EffectSize_Type) & info$EffectSize_Type == "d"] <- convert_d_to_r(d)

  std_es
}



#' @keywords internal
.standardize_standardized_interactions <- function(model, info, type, std_es, centrality = "Median", method = "absolute", ...) {
  # Get parameters
  parameters <- insight::get_parameters(model)
  if (insight::model_info(model)$is_bayesian) {
    parameters <- bayestestR::describe_posterior(parameters, centrality = centrality, dispersion = FALSE, ci = NULL, test = NULL, diagnostic = NULL, priors = FALSE, ...)
    parameters <- parameters[names(parameters) %in% c("Parameter", "Coefficient", "Median", "Mean", "MAP")]
  } else {
    names(parameters) <- c("Parameter", "Coefficient")
  }
  params <- parameters[names(parameters) %in% c("estimate", "Coefficient", "Median", "Mean", "MAP")][[1]]

  interactions <- info$Parameter[info$Type == "interaction"]
  if (length(interactions) > 0) {
    parent_effect <- type[type$Parameter == interactions, "Secondary_Term"]

    if (method == "absolute") {
      # Absolute method
      info[info$Parameter %in% interactions, "EffectSize_Type"] <- info[info$Parameter %in% parent_effect, "EffectSize_Type"]
    } else {
      # Relative method (compute percentage of change based on parent effect)
      parent_effect <- params[parameters$Parameter %in% parent_effect]
      interactions <- params[!is.na(info$EffectSize_Type) & info$EffectSize_Type == "interaction"]
      percentage <- (interactions + parent_effect) / abs(parent_effect)
      std_es[!is.na(info$EffectSize_Type) & info$EffectSize_Type == "interaction"] <- percentage
    }
  }

  list(info = info, std_es = std_es)
}
