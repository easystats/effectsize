library(rstanarm)
library(effectsize)
library(bayestestR)

#' Simulate Eta squared from posterior predictive distribution
#'
#' @inheritParams eta_squared
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample.
#' @param verbose Show messages.
#'
#' @example
#' Model averaging, using with bayestestR
#'
#' @importFrom insight model_info find_formula get_predictors find_response
#' @importFrom rstantools posterior_predict
#' @importFrom stats lm setNames
#' @importFrom car Anova
eta_squared_posterior <- function(model, partial = FALSE, draws = 500, verbose = TRUE, ...) {
  if (!insight::model_info(model)$is_linear) {
    stop("Only applicable to linear models")
  }

  ## test all vars centered?
  # give warning

  if (partial) {
    warning("Only support non partial PVE.")
    partial <- FALSE
    # would need to account for random effects if present.
    # Too hard right now.
  }

  # get ppd
  ppd <- rstantools::posterior_predict(model,
                                       draws = draws, # for rstanreg
                                       nsamples = draws, # for brms
                                       ...)

  # get model data
  f <- insight::find_formula(model)$conditional
  X <- insight::get_predictors(model)
  resp_name <- insight::find_response(model)

  # test centered
  .all_centered(X)
  # should also center? Why not...?


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
    ANOVA <- car::Anova(temp_fit, type = 3)
    es <- eta_squared(ANOVA, ci = NA, partial = partial)

    es <- stats::setNames(es$Eta_Sq, es$Parameter)
    data.frame(t(es), check.names = FALSE)
  })

  res <- do.call("rbind", res)
  return(res)
}

#' @keywords internal
.all_centered <- function(X) {
  numeric <- sapply(X, class) == "numeric"
  numerics <- colnames(X)[numeric]
  factors <- colnames(X)[!numeric]


  numerics_centered <- sapply(X[, numerics, drop = FALSE],
                              function(xi) isTRUE(all.equal(mean(xi),0)))

  # if a contrast has negative and positive values, it is assumed to be one of:
  # "contr.sum", "contr.helmert", "contr.poly", "contr.bayes"
  factors_centered <- sapply(X[, factors, drop = FALSE],
                             function (xi) any(contrasts(xi) < 0) & any(contrasts(xi) > 0))

  if (!all(c(numerics_centered,factors_centered))) {
    non_centered <- !c(numerics_centered,factors_centered)
    non_centered <- names(non_centered)[non_centered]
    warning(
      "Not all variables are centered:\n ",
      paste(non_centered,collapse = ", "),
      "\n Results might be bogus...",
      call. = FALSE, immediate. = TRUE
    )
  }

  return(invisible(NULL))
}

# fit model
model0 <- stan_lmer(mpg ~ wt + qsec * factor(am) + (1|cyl),
                    data = mtcars, refresh = 0,
                    diagnostic_file = file.path(tempdir(), "df0.csv"))
model1 <- stan_lmer(mpg ~ qsec * factor(am) + (1|cyl),
                    data = mtcars, refresh = 0,
                    diagnostic_file = file.path(tempdir(), "df1.csv"))


# ppd effect size
pp_eta2_0 <- eta_squared_posterior(model0)
pp_eta2_1 <- eta_squared_posterior(model1)

# distribution
BF <- bayesfactor_models(model0, model1)
debugonce(bayestestR:::weighted_posteriors.data.frame)
pp_eta2_w <- weighted_posteriors(pp_eta2_0, pp_eta2_1, prior_odds = BF$BF[2])

describe_posterior(pp_eta2_w, ci = 0.89,
                   test = c("rope"), rope_range = c(0, 0.1), rope_ci = 1)


# Compare to
library(magrittr)
lm(mpg ~ wt + qsec * factor(am), data = mtcars) %>%
  car::Anova(type = 3) %>%
  eta_squared(partial = FALSE)

describe_posterior(pp_eta2_0, ci = 0.89,
                   test = c("rope"), rope_range = c(0, 0.1), rope_ci = 1)



model0p <- update(model0, prior_PD = TRUE)
pp_eta2_0p <- eta_squared_posterior(model0p)

si <- si(pp_eta2_0, pp_eta2_0p, BF = 1, lbound = 0, ubound = 1)
plot(si, support_only = T) + ggplot2::coord_cartesian(xlim = c(0,1))

bf <- bayesfactor_parameters(pp_eta2_0, pp_eta2_0p, null = c(0, 0.05), lbound = 0, ubound = 1)
plot(bf) + ggplot2::coord_cartesian(xlim = c(0,1))
