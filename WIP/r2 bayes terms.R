
term_R2 <- function(model){

  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))

  y <- insight::get_response(model)

  par_names <- insight::find_parameters(model)$conditional
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  pars <- insight::get_parameters(model, component = "conditional")

  y_hat <- apply(pars, 1, function(.x) {
    mm %*% .x
  })
  r2 <- rstantools:::bayes_R2.default(t(y_hat), y)

  res <- matrix(NA,nrow = nrow(pars), ncol = n_factors-1)
  colnames(res) <- bayestestR:::.make_terms(bayestestR:::.find_full_formula(model))

  for (f in seq_len(n_factors)-1) {
    if (f==0) next()

    i <- factors == f
    temp_mm <- mm
    temp_mm[,i] <- 0

    temp_y_hat <- apply(pars, 1, function(.x) {
      temp_mm %*% .x
    })
    temp_r2 <- rstantools:::bayes_R2.default(t(temp_y_hat), y)

    res[,f] <- r2 - temp_r2
  }
  return(as.data.frame(res))
}

library(rstanarm)
library(effectsize)
library(bayestestR)

freq_mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length * Species,
               data = iris)


eta_squared(freq_mod, partial = F)


# Bayesian take 1:

bayes_mod1 <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length * Species,
                       data = iris,
                       refresh = 0)

R2s <- term_R2(bayes_mod1)

describe_posterior(R2s, ci = 0.9, test = NULL)

#' But if you center all the variables...
#' Including the factors!
options(contrasts=c('contr.sum', 'contr.bayes'))

bayes_mod2 <- stan_glm(Sepal.Length ~ scale(Sepal.Width) + scale(Petal.Length) * Species,
                       data = iris,
                       refresh = 0)

R2s <- term_R2(bayes_mod2)
describe_posterior(R2s, ci = 0.9, test = NULL)
