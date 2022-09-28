#' @param ss_function For Bayesian models, the function used to extract
#'   sum-of-squares. Uses [`anova()`] by default, but can also be `car::Anova()`
#'   for simple linear models.
#' @param draws For Bayesian models, an integer indicating the number of draws
#'   from the posterior predictive distribution to return. Larger numbers take
#'   longer to run, but provide estimates that are more stable.
#'
#' @export
#' @rdname eta_squared
eta_squared_posterior <- function(model,
                                  partial = TRUE,
                                  generalized = FALSE,
                                  ss_function = stats::anova,
                                  draws = 500,
                                  verbose = TRUE,
                                  ...) {
  UseMethod("eta_squared_posterior")
}

#' @export
#' @importFrom stats lm setNames
#' @importFrom insight find_formula get_predictors find_response check_if_installed
eta_squared_posterior.stanreg <- function(model,
                                          partial = TRUE,
                                          generalized = FALSE,
                                          ss_function = stats::anova,
                                          draws = 500,
                                          verbose = TRUE,
                                          ...) {
  insight::check_if_installed("rstantools")

  mi <- .get_model_info(model, ...)
  if (!mi$is_linear || mi$is_multivariate) {
    stop("Computation of Eta Squared is only applicable to univariate linear models.", call. = FALSE)
  }

  if (mi$is_mixed) {
    if (partial) {
      if (verbose) {
        warning(
          "Bayesian Partial Eta Squared not supported for mixed models.\n",
          "Returning Eta Squared instead."
        )
      }
      partial <- FALSE
      # would need to account for random effects if present.
      # Too hard right now.
    }

    if (isTRUE(generalized) || is.character(generalized)) {
      if (verbose) {
        warning(
          "Bayesian Generalized Eta Squared not supported for mixed models.\n",
          "Returning Eta Squared instead."
        )
      }
      generalized <- FALSE
    }
  }


  ## 1. get model data
  f <- insight::find_formula(model)$conditional
  X <- insight::get_predictors(model)
  resp_name <- insight::find_response(model)

  # test centered predictors
  # if (verbose) .all_centered(X)

  ## 2. get ppd
  ppd <- rstantools::posterior_predict(model,
    draws = draws, # for rstanreg
    nsamples = draws # for brms
  )

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
    if (isTRUE(verbose)) {
      ANOVA <- ss_function(temp_fit, ...)
    } else {
      ANOVA <- suppressWarnings(ss_function(temp_fit, ...))
    }

    es <- eta_squared(ANOVA, ci = NULL, partial = partial, generalized = generalized, verbose = verbose)

    es <- stats::setNames(
      es[[if (partial) "Eta2_partial" else "Eta2"]],
      es$Parameter
    )
    data.frame(t(es), check.names = FALSE)
  })

  res <- do.call("rbind", res)
  attr(res, "generalized") <- generalized
  return(res)
}

#' @export
eta_squared_posterior.brmsfit <- eta_squared_posterior.stanreg


#' #' @keywords internal
#' #' @importFrom stats contrasts
#' .all_centered <- function(X) {
#'   numeric <- sapply(X, inherits, what = c("numeric", "integer"))
#'   numerics <- colnames(X)[numeric]
#'   factors <- colnames(X)[!numeric]
#'
#'   numerics_centered <- factors_centered <- logical(0)
#'
#'   if (length(numerics)) {
#'     numerics_centered <- sapply(
#'       X[, numerics, drop = FALSE],
#'       function(xi) isTRUE(all.equal(mean(xi), 0))
#'     )
#'   }
#'
#'
#'   of <- options()$contrasts
#'   if (length(factors)) {
#'     factors_centered <- sapply(X[, factors, drop = FALSE], function(xi) {
#'       # if a contrast has negative and positive values, it is assumed to be one of:
#'       # "contr.sum", "contr.helmert", "contr.poly", "contr.bayes"
#'       (is.factor(xi) && (any(contrasts(xi) < 0) & any(contrasts(xi) > 0))) ||
#'         # Or if it is not a factor, is the default method one of these?
#'         (!is.factor(xi) && all(of %in% c("contr.sum", "contr.poly", "contr.bayes", "contr.helmert")))
#'     })
#'   }
#'
#'
#'   if ((length(numerics_centered) && !all(numerics_centered)) ||
#'     length(factors_centered) && !all(factors_centered)) {
#'     non_centered <- !c(numerics_centered, factors_centered)
#'     non_centered <- names(non_centered)[non_centered]
#'     warning(
#'       "Not all variables are centered:\n ",
#'       paste(non_centered, collapse = ", "),
#'       "\n Results might be bogus if involved in interactions...",
#'       call. = FALSE
#'     )
#'   }
#'
#'   return(invisible(NULL))
#' }
