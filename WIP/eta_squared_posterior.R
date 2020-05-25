library(rstanarm)
library(effectsize)

eta_squared_posterior <- function(model, partial = FALSE, nsamps = 1000, verbose = TRUE, ...) {
  if (!insight::model_info(model)$is_linear) {
    stop("Only applicable to linear models")
  }

  if (partial) {
    warning("Only support non partial PVE.")
  }

  # get ppd
  ppd <- rstantools::posterior_predict(model)
  nsamps <- min(nsamps, nrow(ppd))
  i <- sample(nrow(ppd), size = nsamps)
  ppd <- ppd[i,]

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
    temp_fit <- lm(f, temp_dat)

    # compute effect size
    ANOVA <- car::Anova(temp_fit, type = 3)
    es <- eta_squared(ANOVA, ci = NA, partial = FALSE)

    data.frame(t(setNames(es$Eta_Sq, es$Parameter)), check.names = F)
  })

  res <- do.call("rbind", res)
  return(res)
}


model <- stan_lmer(mpg ~ wt + qsec * factor(am) + (1|cyl),
                   data = mtcars, refresh = 0)
pp_eta2 <- eta_squared_posterior(model)


bayestestR::describe_posterior(pp_eta2)

library(magrittr)
lm(mpg ~ wt + qsec * factor(am), data = mtcars) %>%
  car::Anova(type = 3) %>%
  eta_squared(partial = FALSE)



