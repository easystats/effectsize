#' (For each posterior draw)
#'
#' Partial R2 (what's partialled: the var unexplained by the fixed terms!)
#' 1. Get for each term, a Y predicted by that term's parameters.
#' 2. Get the partial correlation between Y and the Y~term{i} (controlling for all other Y~term{i}s)
#'
#'
#' Possible problems arise when interactions are present!
#' 1. When variables aren't centered (effects coding, mean centered), the correct Y~term{i}s
#'    is a mish-mash of several prameters.
#' 2. Also when vars are ventered, results are funkey...
#'
#'
#'
#'
#' Non partial R2
#' 1. Get for each term, a Y predicted by that term's parameters.
#' 2. Get for each term, a Y predicted by all params but that term's parameters.
#' 3. Get the partial correlation between Y and the Y~term{i} (controlling for its Y~NOTterm{i}s)


#  Maybe? https://doi.org/10.1111/stan.12173

library(rstanarm)
library(effectsize)
library(bayestestR)


.all_centered <- function(model) {
  trm <- delete.response(terms(model))
  mf <- model.frame(model)
  mm <- model.matrix(model)


  if (!any(grepl(":", attr(trm,"term.labels")))) {
    return(invisible(NULL))
  }

  types <- attr(trm,"dataClasses")
  types <- types[names(types) %in% attr(trm,"term.labels")]

  numerics <- sapply(mf[names(types)[types!="factor"]],
                     function(x) isTRUE(all.equal(mean(x),0)))

  sum_to_zero_contrs <- c("contr.sum","contr.helmert","contr.poly","contr.bayes")
  factors <- sapply(attr(mm, "contrasts"), `%in%`, sum_to_zero_contrs)
  # browser()
  if (!all(c(numerics,factors))) {
    non_centered <- c(numerics,factors)
    non_centered <- names(non_centered)[!non_centered]
    warning(
      "Not all variables are centered:\n",
      paste(non_centered,collapse = ", "),
      call. = FALSE, immediate. = TRUE
    )
  }

  return(invisible(NULL))
}


r2_bayes_term1 <- function(model) {
  .all_centered(model)

  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("This function requires 'bayestestR' to work.")
  }

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function requires 'rstantools' to work.")
  }


  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")
  par_names <- colnames(pars)
  term_names <- attr(delete.response(terms(model)),"term.labels")

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }


  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## DV
  y <- insight::get_response(model)

  ## Get Predicted Values
  y_hats <- vector("list", length = n_factors - 1)

  for (f in seq_len(n_factors)-1) {
    if (f==0) next()

    temp_mm <- mm

    i <- factors == f

    temp_mm[,!i] <- 0

    y_hats[[f]] <- temp_mm %*% t(pars)
  }

  y_hats_c <- Reduce(c, y_hats)
  y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))

  pr <- apply(y_hats_array, 2, function(prd) {
    tempx <- cbind(y,prd)

    pr <- ppcor::pcor(tempx)$estimate
    # pr <- ppcor::spcor(tempx)$estimate
    pr[-1,1] ^ 2
  })

  R2s <- as.data.frame(t(pr))
  colnames(R2s) <- term_names
  return(R2s)
}


r2_bayes_term2 <- function(model) {
  .all_centered(model)

  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("This function requires 'bayestestR' to work.")
  }

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function requires 'rstantools' to work.")
  }


  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")
  par_names <- colnames(pars)
  term_names <- attr(delete.response(terms(model)),"term.labels")

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }


  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## DV
  y <- insight::get_response(model)
  y_hat <- mm %*% t(pars)
  r2 <- rstantools:::bayes_R2.default(t(y_hat), y)

  ## Get R2s
  R2s <- matrix(NA, ncol = length(term_names), nrow = nrow(pars))
  colnames(R2s) <- term_names

  for (f in seq_len(n_factors)-1) {
    if (f==0) next()

    temp_mm <- mm

    i <- factors == f

    temp_mm[,i] <- 0

    temp_y_hat <- temp_mm %*% t(pars)
    temp_r2 <- rstantools:::bayes_R2.default(t(temp_y_hat), y)

    R2s[,f] <- r2 - temp_r2
  }

  return(as.data.frame(R2s))
}


r2_bayes_term3 <- function(model) {
  .all_centered(model)

  if (!requireNamespace("bayestestR", quietly = TRUE)) {
    stop("This function requires 'bayestestR' to work.")
  }

  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("This function requires 'rstantools' to work.")
  }


  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")
  par_names <- colnames(pars)
  term_names <- attr(delete.response(terms(model)),"term.labels")

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }


  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## DV
  y <- insight::get_response(model)
  y_hat <- mm %*% t(pars)
  r2 <- rstantools:::bayes_R2.default(t(y_hat), y)

  ## Get Predicted Values
  y_hats <- vector("list", length = n_factors - 1)

  for (f in seq_len(n_factors)-1) {
    if (f==0) next()

    temp_mm <- mm

    i <- factors == f

    temp_mm[,i] <- 0

    y_hats[[f]] <- temp_mm %*% t(pars)
  }


  y_hats_c <- Reduce(c, y_hats)
  y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))

  pr <- apply(y_hats_array, 2, function(prd) {
    tempx <- cbind(y,prd)
    dim(tempx)
    pr <- ppcor::pcor(tempx)$estimate
    # pr <- ppcor::spcor(tempx)$estimate
    pr <- pr[-1,1] ^ 2
    # pr[is.nan(pr)] <- 0
    # pr[pr > 1] <- 1
    pr
  })



  R2s <- as.data.frame(t(pr))
  colnames(R2s) <- term_names
  return(R2s)
}


r2_bayes_term4 <- function(model) {
  .all_centered(model)

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

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }

  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")

  ## DV
  y <- insight::get_response(model)

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


  # # Get Pred-ys
  # y_hats2 <- list()
  #
  # for (trm in seq_len(ncol(int_table))) {
  #   y_hat_temp <- y_hats[[trm]]
  #   if (any(int_table[,trm])) {
  #     subt <- y_hats[int_table[,trm]]
  #     subt <- Reduce(`+`, subt)
  #     warning("for higher order interactions A:B:C, we will double dip ",
  #             "as the lower interaction A:B has also A+B")
  #
  #     y_hat_temp <- y_hat_temp - subt
  #   }
  #
  #   y_hats2[[trm]] <- y_hat_temp
  # }
  #
  # y_hats_c <- Reduce(c, y_hats2)
  # y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))
  #
  # pr <- apply(y_hats_array, 2, function(prd) {
  #   tempx <- cbind(y,prd)
  #   dim(tempx)
  #   pr <- ppcor::pcor(tempx)$estimate
  #   pr[-1,1] ^ 2
  # })


  y_hats_c <- Reduce(c, y_hats)
  y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))

  pr <- apply(y_hats_array, 2, function(prd) {
    df <- as.data.frame(prd)

    for (trm in seq_len(ncol(int_table))) {
      if (any(int_table[,trm])) {
        subt <- df[int_table[,trm]]

        xfit <- lm(df[,trm] ~ as.matrix(subt))
        df[,trm] <- residuals(xfit)
      }
    }

    tempx <- cbind(y,df)
    dim(tempx)
    pr <- ppcor::pcor(tempx)$estimate
    # pr <- ppcor::spcor(tempx)$estimate
    pr <- pr[-1,1] ^ 2
    # pr[is.nan(pr)] <- 0
    # pr[pr > 1] <- 1
    pr
  })



  R2s <- as.data.frame(t(pr))
  colnames(R2s) <- term_names
  return(R2s)
}



r2_bayes_term5 <- function(model) {
  .all_centered(model)

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

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }

  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")

  ## DV
  y <- insight::get_response(model)

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

    temp_mm <- mm

    i <- factors == f

    temp_mm[,!i] <- rep(linfct[1,!i], each = nrow(temp_mm))

    y_hats[[f]] <- temp_mm %*% t(pars)
  }


  y_hats_c <- Reduce(c, y_hats)
  y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))

  pr <- apply(y_hats_array, 2, function(prd) {
    tempx <- cbind(y,prd)
    dim(tempx)
    pr <- ppcor::pcor(tempx)$estimate
    # pr <- ppcor::spcor(tempx)$estimate
    pr <- pr[-1,1] ^ 2
    # pr[is.nan(pr)] <- 0
    # pr[pr > 1] <- 1
    pr
  })



  R2s <- as.data.frame(t(pr))
  colnames(R2s) <- term_names
  return(R2s)
}

r2_bayes_term6 <- function(model) {
  .all_centered(model)

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

  if (length(term_names) == 1) {
    stop("Use performance::r2")
  }

  ## Set up model matrix
  mm <- model.matrix(model)
  factors <- attr(mm,"assign")
  n_factors <- length(unique(factors))
  mm <- mm[,colnames(mm) %in% par_names, drop = FALSE]

  ## Parameter estiamtes
  pars <- insight::get_parameters(model, component = "conditional")

  ## DV
  y <- insight::get_response(model)

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

    temp_mm <- mm

    i <- factors == f

    temp_mm[,!i] <- rep(linfct[1,!i], each = nrow(temp_mm))

    y_hats[[f]] <- temp_mm %*% t(pars)
  }


  y_hats_c <- Reduce(c, y_hats)
  y_hats_array <- array(y_hats_c, dim = c(dim(y_hats[[1]]), length(y_hats)))

  pr <- apply(y_hats_array, 2, function(prd) {
    tempx <- cbind(y,prd)

    rs <- cor(tempx)[-1,1]

    X <- cbind(1,scale(prd))
    Y <- scale(y)

    betas <- (solve(t(X) %*% X) %*% t(X) %*% Y)[-1]

    betas * rs
  })



  R2s <- as.data.frame(t(pr))
  colnames(R2s) <- term_names
  return(R2s)
}



# TEST additive -----------------------------------------------------------

options(contrasts = c('contr.treatment', 'contr.treatment'))
bayes_mod1 <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length,
                       data = iris,
                       refresh = 0)

r1 <- describe_posterior(r2_bayes_term1(bayes_mod1), ci = 0.9, test = NULL)
r2 <- describe_posterior(r2_bayes_term2(bayes_mod1), ci = 0.9, test = NULL)
r3 <- describe_posterior(r2_bayes_term3(bayes_mod1), ci = 0.9, test = NULL)
r4 <- describe_posterior(r2_bayes_term4(bayes_mod1), ci = 0.9, test = NULL)
r5 <- describe_posterior(r2_bayes_term5(bayes_mod1), ci = 0.9, test = NULL)
r6 <- describe_posterior(r2_bayes_term6(bayes_mod1), ci = 0.9, test = NULL)

r1 # good
r2
r3 # good but backwards....
r4 # good
r5 # good
r6 # intresting...

freq_mod <- lm(Sepal.Length ~ Sepal.Width + Petal.Length,
               data = iris)
eta_squared(car::Anova(freq_mod, type = 3), partial = T)
res1

performance::r2(freq_mod)
performance::r2(bayes_mod1)

# TEST additive w/ factor -----------------------------------------------------------

options(contrasts = c('contr.bayes', 'contr.bayes'))
bayes_mod1 <- stan_glm(Sepal.Length ~ Sepal.Width + Species,
                       data = iris,
                       refresh = 0)

r1 <- describe_posterior(r2_bayes_term1(bayes_mod1), ci = 0.9, test = NULL)
r2 <- describe_posterior(r2_bayes_term2(bayes_mod1), ci = 0.9, test = NULL)
r3 <- describe_posterior(r2_bayes_term3(bayes_mod1), ci = 0.9, test = NULL)
r4 <- describe_posterior(r2_bayes_term4(bayes_mod1), ci = 0.9, test = NULL)
r5 <- describe_posterior(r2_bayes_term5(bayes_mod1), ci = 0.9, test = NULL)
r6 <- describe_posterior(r2_bayes_term6(bayes_mod1), ci = 0.9, test = NULL)

r1 # good
r2
r3 # good but backwards....
r4 # good
r5 # good
r6 # intresting...

freq_mod <- lm(Sepal.Length ~ Sepal.Width + Species,
               data = iris)
eta_squared(car::Anova(freq_mod, type = 3), partial = T)
res1

performance::r2(freq_mod)
performance::r2(bayes_mod1)


# TEST INTERACTIONS --------------------------------------------------------------------


# # Bayesian take 1:
# options(contrasts = c('contr.treatment', 'contr.treatment'))
# bayes_mod1 <- stan_glm(Sepal.Length ~ Sepal.Width + Petal.Length * Species,
#                        data = iris,
#                        refresh = 0)
#
# R2s <- r2_bayes_term(bayes_mod1)
# res1 <- describe_posterior(R2s, ci = 0.9, test = NULL)


# Bayesian take 2:
options(contrasts = c('contr.bayes', 'contr.bayes'))
bayes_mod2 <- stan_glm(Sepal.Length ~ scale(Sepal.Width) + scale(Petal.Length) * Species,
                       data = iris,
                       refresh = 0)

r1 <- describe_posterior(r2_bayes_term1(bayes_mod2), ci = 0.9, test = NULL)
r4 <- describe_posterior(r2_bayes_term4(bayes_mod2), ci = 0.9, test = NULL)
r5 <- describe_posterior(r2_bayes_term5(bayes_mod2), ci = 0.9, test = NULL)

r1
r4
r5
# non are good.


performance::r2(bayes_mod1)



freq_mod <- lm(Sepal.Length ~ scale(Sepal.Width) + scale(Petal.Length) * Species,
               data = iris)
eta_squared(car::Anova(freq_mod, type = 3), partial = T)

