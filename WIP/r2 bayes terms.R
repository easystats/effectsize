# r2_bayes_term <- function(model) {
#
#   mm <- model.matrix(model)
#   factors <- attr(mm,"assign")
#   n_factors <- length(unique(factors))
#
#   y <- insight::get_response(model)
#
#   par_names <- insight::find_parameters(model)$conditional
#   mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]
#
#   pars <- insight::get_parameters(model, component = "conditional")
#
#   y_hat <- apply(pars, 1, function(.x) {
#     mm %*% .x
#   })
#   r2 <- rstantools:::bayes_R2.default(t(y_hat), y)
#   #
#   res <- matrix(NA,nrow = nrow(pars), ncol = n_factors-1)
#   colnames(res) <- bayestestR:::.make_terms(bayestestR:::.find_full_formula(model))
#
#   for (f in seq_len(n_factors)-1) {
#     if (f==0) next()
#
#     i <- factors == f
#     temp_mm <- mm
#     temp_mm[,i] <- 0
#
#
#     temp_y_hat <- apply(pars, 1, function(.x) {
#       temp_mm %*% .x
#     })
#     temp_r2 <- rstantools:::bayes_R2.default(t(temp_y_hat), y)
#
#     res[,f] <- r2 - temp_r2
#   }
#   return(as.data.frame(res))
# }



r2_bayes_term <- function(model) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("This function requires 'emmeans' to work.")
  }

  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("This function requires 'bayestestR' to work.")
  }

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function requires 'rstantools' to work.")
  }

  linfct <- emmeans::emmeans(model, ~ 1)@linfct
  par_names <- colnames(linfct)
  term_names <- bayestestR:::.make_terms(bayestestR:::.find_full_formula(model))

  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")

  ## DV
  y <- insight::get_response(model)
  # y_hat <- mm %*% t(pars)
  # r2 <- rstantools:::bayes_R2.default(t(y_hat), y)


  ## Interaction table
  int_table <- matrix(NA, nrow = length(term_names), ncol = length(term_names))
  colnames(int_table) <- term_names
  row.names(int_table) <- term_names
  for (eff in term_names) {
    int_table[,eff] <- sapply(term_names, function(x) bayestestR:::.includes_interaction(x, eff))
  }

  ## Get Predicted Values
  y_hats <- vector("list", length = n_factors - 1)

  for (f in seq_len(n_factors)-1) {
    if (f==0) next()
    ## Model metirx
    temp_mm <- mm

    spec <- as.formula(paste0("~",term_names[f]))
    linfct_temp <- suppressMessages(emmeans::emmeans(model, specs = spec, cov.red = range)@linfct)
    let_var <- apply(linfct_temp, 2, function(x) length(unique(x))) > 1
    # where more than 1, let varry, otherwise fix to linfct
    temp_mm[,!let_var] <- rep(linfct[1,!let_var], each = nrow(temp_mm))

    y_hats[[f]] <- temp_mm %*% t(pars)
  }



  # Get R2s
  r2s <- matrix(NA, nrow = nrow(pars), ncol = n_factors - 1)
  colnames(r2s) <- term_names

  for (trm in seq_len(ncol(int_table))) {
    y_hat_temp <- y_hats[[trm]]
    if (any(int_table[,trm])) {
      subt <- y_hats[int_table[,trm]]
      subt <- Reduce(`+`, subt)
      warning("for higher order interactions A:B:C, we will double dip ",
              "as the lower interaction A:B has also A+B")

      y_hat_temp <- y_hat_temp - subt
    }

    r2s[,trm] <- rstantools:::bayes_R2.default(t(y_hat_temp), y)
  }

  return(as.data.frame(r2s))
}







library(rstanarm)
library(effectsize)
library(bayestestR)


# Bayesian take 1:
bayes_mod2 <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length * Species,
                       data = iris,
                       refresh = 0)

options(contrasts = c('contr.sum', 'contr.poly'))
freq_mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length * Species,
               data = iris)




eta_squared(car::Anova(freq_mod, type = 3), partial = F)

R2s <- r2_bayes_term(bayes_mod2)
############# NOPE ####################
describe_posterior(R2s, ci = 0.9, test = NULL)
round(.Last.value$Median,3)
performance::r2(bayes_mod2)
