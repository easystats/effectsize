#' Simulate Eta Squared from Posterior Predictive Distribution
#'
#' This function simulates data from the posterior predictive distribution (ppd)
#' and for each simulation the Eta Squared is computed for the model's fixed
#' effects. This means that the returned values are the population level effect
#' size as implied by the posterior model (and not the effect size in the sample
#' data). See [rstantools::posterior_predict()] for more info.
#' \cr\cr
#' Effect sizes are computed using the sums of squares obtained from
#' `car::Anova(model, ...)` which might not always be appropriate (**_Yeah...
#' ANOVAs are hard..._**), e.g., when factors aren't 0-mean codded and
#' covariates aren't centered; a warning will be given in such predictors are
#' detected. See *details* section in [eta_squared()].
#'
#' @param model A Bayesian linear model, fit with `brms` or `rstanarm`.
#' @param partial Whether Partial Eta should be returned.
#' @param type Type of sum-of-squares to use. See [car::Anova()].
#' @param draws An integer indicating the number of draws from the posterior
#'   predictive distribution to return. Larger numbers take longer to run, but
#'   provide estimates that are more stable.
#' @param verbose Show messages / warning about centering.
#' @param ... Currently not used.
#'
#' @return A data frame containing the ppd of the Eta squared for each fixed
#'   effect, which can then be passed to [bayestestR::describe_posterior()] for
#'   summary stats.
#'
#' @examples
#' \donttest{
#' if (require(rstanarm) && require(bayestestR)) {
#'   fit_bayes <- stan_glm(mpg ~ factor(cyl) * wt + qsec,
#'                         data = mtcars,
#'                         family = gaussian(),
#'                         refresh = 0)
#'
#'   es <- eta_squared_posterior(fit_bayes)
#'
#'   bayestestR::describe_posterior(es)
#' }
#'
#' # compare to:
#' if (require(car)) {
#'   fit_freq <- lm(mpg ~ factor(cyl) * wt + qsec,
#'                  data = mtcars)
#'   aov_table <- car::Anova(fit_freq, type = 3)
#'   eta_squared(aov_table)
#' }
#' }
#'
#'
#' @export
eta_squared_posterior <- function(model,
                                  partial = TRUE,
                                  type = 3,
                                  draws = 500,
                                  verbose = TRUE,
                                  ...) {
  UseMethod("eta_squared_posterior")
}

#' @export
#' @importFrom stats lm setNames
#' @importFrom insight model_info find_formula get_predictors find_response
eta_squared_posterior.stanreg <- function(model,
                                  partial = TRUE,
                                  type = 3,
                                  draws = 500,
                                  verbose = TRUE,
                                  ...){

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function requires 'rstantools' to work.")
  }

  if (!requireNamespace("car", quietly = TRUE)) {
    stop("This function requires 'car' to work.")
  }

  mo_inf <- insight::model_info(model)
  if ((!mo_inf$is_linear) ||
      mo_inf$is_multivariate) {
    stop("Computation of Eta Squared is only applicable to univariate linear models.")
  }

  if (partial && mo_inf$is_mixed) {
    warning("Bayesian Partial Eta Squared not supported for mixed models.\n",
            "Returning Eta Squared instead.")
    partial <- FALSE
    # would need to account for random effects if present.
    # Too hard right now.
  }

  ## 1. get model data
  f <- insight::find_formula(model)$conditional
  X <- insight::get_predictors(model)
  resp_name <- insight::find_response(model)

  # test centered predictors
  .all_centered(X)

  ## 2. get ppd
  ppd <- rstantools::posterior_predict(model,
                                       draws = draws, # for rstanreg
                                       nsamples = draws) # for brms

  ## 3. Compute effect size...
  if (verbose) {
    message("Simulating effect size... This can take a while...")
  }
  res <- apply(ppd, 1, function(r) {
    # sampled outcome + predictors
    temp_dat <- X
    temp_dat[[resp_name]] <- r

    # fit a simple linear model
    temp_fit <- stats::lm(f, temp_dat)

    # compute effect size
    # es <- eta_squared(temp_fit, ci = NA, partial = partial)
    ANOVA <- car::Anova(temp_fit, type = type)
    es <- eta_squared(ANOVA, ci = NA, partial = partial)

    es <- stats::setNames(es[[if (partial) "Eta2_partial" else "Eta2"]],
                          es$Parameter)
    data.frame(t(es), check.names = FALSE)
  })

  res <- do.call("rbind", res)
  return(res)
}

#' @export
eta_squared_posterior.brmsfit <- eta_squared_posterior.stanreg


#' @keywords internal
#' @importFrom stats contrasts
.all_centered <- function(X) {
  numeric <- sapply(X, inherits, what = c("numeric", "integer"))
  numerics <- colnames(X)[numeric]
  factors <- colnames(X)[!numeric]

  numerics_centered <- factors_centered <- logical(0)

  if (length(numerics)) {
    numerics_centered <- sapply(X[, numerics, drop = FALSE],
                                function(xi) isTRUE(all.equal(mean(xi),0)))
  }


  of <- options()$contrasts
  if (length(factors)) {
    factors_centered <- sapply(X[, factors, drop = FALSE], function(xi) {
      # if a contrast has negative and positive values, it is assumed to be one of:
      # "contr.sum", "contr.helmert", "contr.poly", "contr.bayes"
      (is.factor(xi) && (any(contrasts(xi) < 0) & any(contrasts(xi) > 0))) ||
        # Or if it is not a factor, is the default method one of these?
        (!is.factor(xi) && all(of %in% c('contr.sum', 'contr.poly', "contr.bayes", "contr.helmert")))
    })
  }


  if ((length(numerics_centered) && !all(numerics_centered)) ||
      length(factors_centered) && !all(factors_centered)) {

    non_centered <- !c(numerics_centered,factors_centered)
    non_centered <- names(non_centered)[non_centered]
    warning(
      "Not all variables are centered:\n ",
      paste(non_centered,collapse = ", "),
      "\n Results might be bogus if involved in interactions...",
      call. = FALSE, immediate. = TRUE
    )
  }

  return(invisible(NULL))
}