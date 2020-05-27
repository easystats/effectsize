library(rstanarm)
library(effectsize)
library(bayestestR)

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
    # would need to account for random effects if present.
    # Too hard right now.
  }

  # get ppd
  ppd <- rstantools::posterior_predict(model, draws = draws, ...)

  # get model data
  f <- insight::find_formula(model)$conditional
  X <- insight::get_predictors(model)
  resp_name <- insight::find_response(model)


  if (verbose) {
    message("Sampleing effect size... This can take a while...")
  }
  res <- apply(ppd, 1, function(r) {
    # sampled outcome + predictors
    temp_dat <- X
    temp_dat[[resp_name]] <- r

    # fit a simple linear model
    temp_fit <- stats::lm(f, temp_dat)

    # compute effect size
    # es <- eta_squared(temp_fit, ci = NA, partial = FALSE)
    ANOVA <- car::Anova(temp_fit, type = 3)
    es <- eta_squared(ANOVA, ci = NA, partial = FALSE)

    es <- stats::setNames(es$Eta_Sq, es$Parameter)
    data.frame(t(es), check.names = FALSE)
  })

  res <- do.call("rbind", res)
  return(res)
}

# fit model
model <- stan_lmer(mpg ~ wt + qsec * factor(am) + (1|cyl),
                   data = mtcars, refresh = 0)
model_p <- bayestestR:::.update_to_priors.stanreg(model) # make this an exported function?

# ppd effect size
pp_eta2 <- eta_squared_posterior(model)
pp_eta2_p <- eta_squared_posterior(model_p)

# distribution
describe_posterior(pp_eta2_p, ci = 0.89,
                   test = c("rope"), rope_range = c(0, 0.1), rope_ci = 1)
describe_posterior(pp_eta2, ci = 0.89,
                   test = c("rope"), rope_range = c(0, 0.1), rope_ci = 1)

# Compare to
library(magrittr)
lm(mpg ~ wt + qsec * factor(am), data = mtcars) %>%
  car::Anova(type = 3) %>%
  eta_squared(partial = FALSE)




# BF + SI
(bf <- bayesfactor_parameters(pp_eta2, pp_eta2_p,
                              null = c(0, 0.1),
                              lbound = 0, ubound = 1))
plot(bf)

(si <- si(pp_eta2, pp_eta2_p,
          BF = c(1/3,1,3, 10),
          lbound = 0, ubound = 1))
plot(si, support_only = T)





