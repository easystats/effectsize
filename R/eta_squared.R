#' Effect size for ANOVA
#'
#' Functions to compute effect size measures for ANOVAs, such as Eta, Omega and Epsilon squared,
#' and Cohen's f (or their partialled versions) for `aov`, `aovlist` and `anova`
#' models. These indices represent an estimate of how much variance in the response variables
#' is accounted for by the explanatory variable(s).
#' \cr\cr
#' Effect sizes are computed using the sums of squares obtained from `anova(model)` which
#' might not always be appropriate (**_Yeah... ANOVAs are hard..._**). See details.
#'
#' @param model A model, ANOVA object, or the result of `parameters::model_parameters`.
#' @param partial If `TRUE`, return partial indices.
#' @param generalized If TRUE, returns generalized Eta Squared, assuming all
#'   variables are manipulated. Can also be a character vector of observed
#'   (non-manipulated) variables, in which case generalized Eta Squared is
#'   calculated taking these observed variables into account.
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to or from other methods (ignored).
#'
#' @return A data frame with the effect size(s) and confidence interval(s).
#'
#' @details
#'
#' For `aov` and `aovlist` models, the effect sizes are computed directly with
#' Sums-of-Squares. For all other model, the model is passed to `anova()`, and effect
#' sizes are approximated via test statistic conversion (see `[F_to_eta2] for
#' more details.`)
#'
#' ## Type of Sums of Squares
#' The sums of squares (or F statistics) used for the computation of the effect sizes is
#' based on those returned by `anova(model)` (whatever those may be - for `aov`
#' and `aovlist` these are *type-1* sums of squares; for `merMod` these are
#' *type-3* sums of squares). Make sure these are the sums of squares you are intrested
#' in (you might want to pass the result of `car::Anova(mode, type = 3)`).
#' \cr\cr
#' It is generally recommended to fit models with *`contr.sum` factor weights* and
#' *centered covariates*, for sensible results. See examples.
#'
#' ## Confidence Intervals
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best `ncp` (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these `ncp`s to the corresponding effect sizes.
#' \cr\cr
#' Special care should be taken when interpreting CIs with a lower bound equal
#' to (or small then) 0, and even more care should be taken when the
#' *upper* bound is equal to (or small then) 0 (Steiger, 2004; Morey et al., 2016).
#'
#' ## Un-Biased Estimate of Eta
#' Both ***Omega*** and ***Epsilon*** are unbiased estimators of the
#' population's ***Eta***, which is especially important is small samples. But
#' which to choose?
#' \cr\cr
#' Though Omega is the more popular choice (Albers \& Lakens, 2018), Epsilon is
#' analogous to adjusted R2 (Allen, 2017, p. 382), and has been found to be less
#' biased (Carroll & Nordholm, 1975).
#'
#' ## Cohen's f
#' Cohen's f can take on values between zero, when the population means are all
#' equal, and an indefinitely large number as standard deviation of means
#' increases relative to the average standard deviation within each group.
#' \cr\cr
#' When comparing two models in a sequential regression analysis, Cohen's f for
#' R-square change is the ratio between ratio between the increase in R-square
#' and the \% unexplained variance.
#' \cr\cr
#' Cohen has suggested that the values of 0.10, 0.25, and 0.40 represent small,
#' medium, and large effect sizes, respectively.
#'
#'
#' @seealso [F_to_eta2()]
#'
#' @examples
#' \donttest{
#' library(effectsize)
#' mtcars$am_f <- factor(mtcars$am)
#' mtcars$cyl_f <- factor(mtcars$cyl)
#'
#' model <- aov(mpg ~ am_f * cyl_f, data = mtcars)
#'
#' eta_squared(model)
#' eta_squared(model, generalized = "cyl_f")
#' omega_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#' (etas <- eta_squared(model, partial = FALSE))
#'
#' if(require(see)) plot(etas)
#'
#' model0 <- aov(mpg ~ am_f + cyl_f, data = mtcars)
#' cohens_f2(model0, model2 = model)
#'
#' model <- aov(mpg ~ cyl_f * am_f + Error(vs / am_f), data = mtcars)
#' epsilon_squared(model)
#'
#' # Recommended:
#' # Type-3 effect sizes + effects coding
#' if (require(car, quietly = TRUE)) {
#'   contrasts(mtcars$am_f) <- contr.sum
#'   contrasts(mtcars$cyl_f) <- contr.sum
#'
#'   model <- aov(mpg ~ am_f * cyl_f, data = mtcars)
#'   model_anova <- car::Anova(model, type = 3)
#'
#'   eta_squared(model_anova)
#' }
#'
#' if (require("parameters")) {
#'   model <- lm(mpg ~ wt + cyl, data = mtcars)
#'   mp <- model_parameters(model)
#'   eta_squared(mp)
#' }
#'
#' if (require(lmerTest, quietly = TRUE)) {
#'   model <- lmer(mpg ~ am_f * cyl_f + (1|vs), data = mtcars)
#'   omega_squared(model)
#' }
#' }
#'
#' @return A data frame containing the effect size values and their confidence intervals.
#'
#'
#' @references
#' - Albers, C., \& Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of experimental social psychology, 74, 187-195.
#' - Allen, R. (2017). Statistics and Experimental Design for Psychologists: A Model Comparison Approach. World Scientific Publishing Company.
#' - Carroll, R. M., & Nordholm, L. A. (1975). Sampling Characteristics of Kelley's epsilon and Hays' omega. Educational and Psychological Measurement, 35(3), 541-554.
#' - Kelley, T. (1935) An unbiased correlation ratio measure. Proceedings of the National Academy of Sciences. 21(9). 554-559.
#' - Morey, R. D., Hoekstra, R., Rouder, J. N., Lee, M. D., & Wagenmakers, E. J. (2016). The fallacy of placing confidence in confidence intervals. Psychonomic bulletin & review, 23(1), 103-123.
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis. Psychological Methods, 9, 164-182.
#'
#' @export
eta_squared <- function(model,
                        partial = TRUE,
                        generalized = FALSE,
                        ci = 0.9,
                        ...) {
  out <- .anova_es(model, type = "eta", partial = partial, generalized = generalized, ci = ci)
  class(out) <- unique(c("effectsize_table", "see_effectsize_table", class(out)))
  return(out)
}

#' @rdname eta_squared
#' @export
omega_squared <- function(model,
                          partial = TRUE,
                          ci = 0.9,
                          ...) {
  out <- .anova_es(model, type = "omega", partial = partial, ci = ci)
  class(out) <- unique(c("effectsize_table", "see_effectsize_table", class(out)))
  return(out)
}

#' @rdname eta_squared
#' @export
epsilon_squared <- function(model,
                            partial = TRUE,
                            ci = 0.9,
                            ...) {
  out <- .anova_es(model, type = "epsilon", partial = partial, ci = ci)
  class(out) <- unique(c("effectsize_table", "see_effectsize_table", class(out)))
  return(out)
}

#' @rdname eta_squared
#' @inheritParams F_to_f
#' @param model2 Second model. If specified, returns Cohen's *f* for
#'   R-squared-change between the two models.
#' @export
cohens_f <- function(model, partial = TRUE, ci = 0.9, squared = FALSE,
                     model2 = NULL, ...) {
  if (!is.null(model2)) {
    return(.cohens_f_delta(model, model2, ci = ci, squared = squared))
  }

  res <- eta_squared(model,
                     partial = partial,
                     ci = ci)

  if ("Eta_Sq_partial" %in% colnames(res)) {
    res$Eta_Sq_partial <- res$Eta_Sq_partial / (1 - res$Eta_Sq_partial)
    colnames(res)[colnames(res) == "Eta_Sq_partial"] <- "Cohens_f2_partial"
  } else {
    res$Eta_Sq <- res$Eta_Sq / (1 - res$Eta_Sq)
    colnames(res)[colnames(res) == "Eta_Sq"] <- "Cohens_f2"
  }

  if (is.numeric(ci)) {
    res$CI_low <- res$CI_low  / (1 - res$CI_low)
    res$CI_high <- res$CI_high  / (1 - res$CI_high)
  }


  if (!squared) {
    i <- colnames(res) %in% c("Cohens_f2", "Cohens_f2_partial", "CI_low", "CI_high")
    res[i] <- sqrt(res[i])
    colnames(res)[colnames(res) %in% c("Cohens_f2","Cohens_f2_partial")] <-
      if ("Cohens_f2" %in% colnames(res)) "Cohens_f" else "Cohens_f_partial"
  }


  res
}

#' @rdname eta_squared
#' @export
cohens_f2 <- function(model, partial = TRUE, ci = 0.9, squared = TRUE,
                      model2 = NULL, ...) {
  cohens_f(model, partial = partial, ci = ci, squared = squared, model2 = model2)
}


# Get ES ------------------------------------------------------------------

#' @keywords internal
.anova_es <-
  function(model,
           type = c("eta", "omega", "epsilon"),
           partial = TRUE,
           generalized = FALSE,
           ci = 0.9,
           ...) {
    UseMethod(".anova_es")
  }

#' @keywords internal
#' @importFrom stats anova
.anova_es.default <- function(model,
                              type = c("eta", "omega", "epsilon"),
                              partial = TRUE,
                              generalized = FALSE,
                              ci = 0.9,
                              ...) {
  .anova_es.anova(
    stats::anova(model),
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci
  )
}

#' @keywords internal
#' @importFrom parameters model_parameters
#' @importFrom stats anova
.anova_es.aov <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.9,
                          ...) {
  if (!inherits(model, c("Gam", "anova"))) {
    # Pass to ANOVA table method
    res <- .anova_es.anova(
      stats::anova(model),
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci
    )
    return(res)
  }

  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)

  params <- as.data.frame(parameters::model_parameters(model))
  params <- params[params$Parameter != "(Intercept)", ]
  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found - ",
         type,
         " squared can only be computed for simple `aov` models.")
  }

  values <- .values_aov(params)
  df_error <- params$df[params$Parameter == "Residuals"]
  params <- params[params$Parameter != "Residuals",,drop = FALSE]

  # We need these for the F statistic for CIs
  if (isTRUE(generalized) || is.character(generalized)) {
    ## copied from afex
    obs <- logical(nrow(params))
    if (is.character(generalized)) {
      for (o in generalized) {
        oi <- grepl(paste0("\\b", o, "\\b"), params$Parameter)

        if (!any(oi)) stop("Observed variable not in data: ", o, call. = FALSE)

        obs <- obs | oi
      }
    }
    obs_SSn1 <- sum(params$Sum_Squares * obs)
    obs_SSn2 <- params$Sum_Squares * obs

    ES <- params$Sum_Squares /
      (params$Sum_Squares + values$Sum_Squares_residuals + obs_SSn1 - obs_SSn2)

    generalized <- TRUE
  } else if (!isTRUE(partial)) {
    ES <- params$Sum_Squares /
      values$Sum_Squares_total
  } else {
    ES <-
      params$Sum_Squares /
      (params$Sum_Squares + values$Sum_Squares_residuals)
  }



  if (type == "eta") {
    if (isTRUE(generalized)) {
      params$Eta_Sq_generalized <- ES
    } else if (!isTRUE(partial)) {
      params$Eta_Sq <- ES
    } else {
      params$Eta_Sq_partial <- ES
    }
  } else if (type == "omega") {
    if (!isTRUE(partial)) {
      params$Omega_Sq <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
        (values$Sum_Squares_total + values$Mean_Square_residuals)
    } else {
      params$Omega_Sq_partial <-
        (params$df * (params$Mean_Square - values$Mean_Square_residuals)) /
        (params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals)
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      params$Epsilon_Sq <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
        values$Sum_Squares_total
    } else {
      params$Epsilon_Sq_partial <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
        (params$Sum_Squares + values$Sum_Squares_residuals)
    }
  }



  ES <- ES
  out <- params

  if (is.numeric(ci)) {
    f <- (ES / out$df) / ((1 - ES) / df_error)

    out <- cbind(out,
                 es_fun(f,
                        out$df,
                        df_error,
                        ci = ci)[-1])
  }


  out <- out[, colnames(out) %in% c("Group", "Parameter",
                                    "Eta_Sq", "Eta_Sq_partial", "Eta_Sq_generalized",
                                    "Omega_Sq", "Omega_Sq_partial",
                                    "Epsilon_Sq", "Epsilon_Sq_partial",
                                    "CI", "CI_low", "CI_high"), drop = FALSE]

  out
}

#' @keywords internal
.anova_es.lm <- .anova_es.aov

#' @keywords internal
.anova_es.glm <- .anova_es.aov


#' @keywords internal
.anova_es.anova <- function(model,
                            type = c("eta", "omega", "epsilon"),
                            partial = TRUE,
                            generalized = FALSE,
                            ci = 0.9,
                            ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)

  F_val <- c("F value", "approx F", "F-value")
  numDF <- c("NumDF", "num Df", "numDF")
  denDF <- c("DenDF", "den Df", "denDF")

  if (!any(denDF %in% colnames(model))) {
    # Pass to AOV method
    res <- .anova_es.aov(model,
                         partial = partial,
                         type = type,
                         generalized = generalized,
                         ci = ci)
    return(res)
  }
  model <- model[rownames(model) != "(Intercept)", ]
  model <- model[rownames(model) != "Residuals", ]

  F_val <- F_val[F_val %in% colnames(model)]
  numDF <- numDF[numDF %in% colnames(model)]
  denDF <- denDF[denDF %in% colnames(model)]


  if (!isTRUE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for this class of objects.",
      call. = FALSE
    )
  }

  par_table <- as.data.frame(model)

  out <- cbind(
    Parameter = rownames(par_table),
    es_fun(par_table[[F_val]],
           par_table[[numDF]],
           par_table[[denDF]],
           ci = ci)
  )

  out
}

#' @keywords internal
.anova_es.anova.lme <- .anova_es.anova

#' @importFrom stats na.omit
#' @keywords internal
.anova_es.parameters_model <- function(model,
                                       type = c("eta", "omega", "epsilon"),
                                       partial = TRUE,
                                       generalized = FALSE,
                                       ci = 0.9,
                                       ...) {
  type <- match.arg(type)

  if ("Group" %in% colnames(model) && sum(model$Parameter == "Residuals") > 1) {
    x <- split(model, model$Group)
    out <- do.call(rbind, lapply(x, function(i) {
      f <- i[["F"]]
      df_num <- i[["df"]][!is.na(f)]
      df_error <- i[i$Parameter == "Residuals", "df"]
      cbind(
        data.frame(Group = unique(i$Group), stringsAsFactors = FALSE),
        .anova_es_model_params(i, f, df_num, df_error, type, ci)
      )
    }))
  } else {
    if ("t" %in% colnames(model)) {
      f <- model[["t"]]^2
    }
    if ("F" %in% colnames(model)) {
      f <- model[["F"]]
    }
    if ("z" %in% colnames(model)) {
      stop("Cannot compute effect size from models with no proper residual variance. Consider the estimates themselves as indices of effect size.")
    }

    df_col <- colnames(model)[colnames(model) %in% c("df", "Df", "NumDF")]
    if (length(df_col)) {
      df_num <- model[[df_col]][!is.na(f)]
    } else {
      df_num <- 1
    }

    if ("df_error" %in% colnames(model)) {
      df_error <- model$df_error
    } else if ("Residuals" %in% model$Parameter) {
      df_error <- model[model$Parameter == "Residuals", df_col]
    } else {
      stop("Cannot extract degrees of freedom for the error term. Try passing the model object directly to 'eta_squared()'.")
    }
    out <- .anova_es_model_params(model, f, df_num, df_error, type, ci)
  }

  out
}


# Specific models ---------------------------------------------------------

#' @keywords internal
#' @importFrom parameters model_parameters
.anova_es.aovlist <- function(model,
                              type = c("eta", "omega", "epsilon"),
                              partial = TRUE,
                              generalized = FALSE,
                              ci = 0.9,
                              ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)

  params <- as.data.frame(parameters::model_parameters(model))
  params <- params[params$Parameter != "(Intercept)", ]
  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found - ",
         type,
         " squared can only be computed for simple `aov` models.")
  }

  values <- .values_aov(params)
  params <- params[params$Parameter != "Residuals" & !is.na(params$`F`), ,drop = FALSE]
  Sum_Squares_total <- sum(sapply(values, "[[", "Sum_Squares_total"))
  Sum_Squares_residuals <- sapply(values[params$Group], "[[", "Sum_Squares_residuals")
  Mean_Square_residuals <- sapply(values[params$Group], "[[", "Mean_Square_residuals")
  df_residuals <- sapply(values[params$Group], "[[", "df_residuals")
  ns <- sapply(values[params$Group], "[[", "n")

  if (isTRUE(generalized) || is.character(generalized)) {
    ## copied from afex
    obs <- logical(nrow(params))
    if (is.character(generalized)) {
      for (o in generalized) {
        oi <- grepl(paste0("\\b", o, "\\b"), params$Parameter)

        if (!any(oi)) stop("Observed variable not in data: ", o, call. = FALSE)

        obs <- obs | oi
      }
    }
    obs_SSn1 <- sum(params$Sum_Squares * obs)
    obs_SSn2 <- params$Sum_Squares * obs

    ES <- params$Sum_Squares /
      (params$Sum_Squares + sum(sapply(values, "[[", "Sum_Squares_residuals")) + obs_SSn1 - obs_SSn2)

    generalized <- TRUE
  } else if (!isTRUE(partial)) {
    ES <- params$Sum_Squares / Sum_Squares_total
  } else {
    ES <- F_to_eta2(params[["F"]], params[["df"]], df_residuals, ci = NULL)[[1]]
  }


  if (type == "eta") {
    if (isTRUE(generalized) || is.character(generalized)) {
      params$Eta_Sq_generalized <- ES
    } else if (!isTRUE(partial)) {
      params$Eta_Sq <- ES
    } else {
      params$Eta_Sq_partial <- ES
    }
  } else if (type == "omega") {
    if (!isTRUE(partial)) {
      params$Omega_Sq <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        (Sum_Squares_total + Mean_Square_residuals)
    } else {
      params$Omega_Sq_partial <-
        F_to_omega2(params[["F"]], params[["df"]], df_residuals, ci = NULL)[[1]]
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      params$Epsilon_Sq <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        Sum_Squares_total
    } else {
      params$Epsilon_Sq_partial <-
        F_to_epsilon2(params[["F"]], params[["df"]], df_residuals, ci = NULL)[[1]]
    }
  }

  out <- params

  if (!is.null(ci)) {
    f <- (ES / out$df) / ((1 - ES)/df_residuals)

    out <- cbind(out,
                 es_fun(f,
                        out$df,
                        df_residuals,
                        ci = ci)[-1])
  }

  out <- out[, colnames(out) %in% c(
    "Group",
    "Parameter",
    "Eta_Sq_generalized",
    "Eta_Sq_partial",
    "Omega_Sq_partial",
    "Epsilon_Sq_partial",
    "Eta_Sq",
    "Omega_Sq",
    "Epsilon_Sq",
    "CI",
    "CI_low",
    "CI_high"
  ), drop = FALSE]
  rownames(out) <- NULL

  out
}

#' @keywords internal
#' @importFrom stats anova
.anova_es.merMod <- function(model,
                             type = c("eta", "omega", "epsilon"),
                             partial = TRUE,
                             generalized = FALSE,
                             ci = 0.9,
                             ...) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. ",
         "Please install it by running `install.packages('lmerTest')`.")
  }

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  .anova_es.anova(model, type = type, partial = partial, generalized = generalized, ci = ci, ...)
}

#' @keywords internal
#' @importFrom stats anova
.anova_es.gam <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.9,
                          ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)

  if (!isTRUE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for repeated-measures / multi-variate ANOVAs",
      call. = FALSE
    )
  }

  model <- stats::anova(model)

  tab <- data.frame(model$s.table)

  out <- cbind(
    Parameter = rownames(tab),
    es_fun(
      f = tab$`F`,
      df = tab$Ref.df,
      df_error = model$residual.df,
      ci = ci
    )
  )

  out
}

#' @keywords internal
.anova_es.afex_aov <- function(model,
                               type = c("eta", "omega", "epsilon"),
                               partial = TRUE,
                               generalized = FALSE,
                               ci = 0.9,
                               ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)


  # if (!is.null(model$aov)) {
  #   out <- .anova_es(model$aov, type = type, partial = partial, ci = ci)
  #   return(out)
  # }

  if (!isTRUE(partial)) {
    warning("Currently only supports partial ",
            type,
            " squared for afex-models.",
            call. = FALSE)
  }


  if (!requireNamespace("afex", quietly = TRUE)) {
    stop(
      "Package 'afex' required for this function to work. ",
      "Please install it by running `install.packages('afex')`."
    )
  }
  model <- afex::nice(model,
                      correction = "none",
                      sig_symbols  = rep("", 4))

  f <- as.numeric(model$`F`)
  dfs <- lapply(strsplit(model$df, ","), as.numeric)
  df1 <- sapply(dfs, `[`, 1)
  df2 <- sapply(dfs, `[`, 2)

  out <- cbind(
    Parameter = model$Effect,
    es_fun(
      f,
      df1,
      df2,
      ci = ci
    )
  )

  return(out)
}



#' @keywords internal
#' @importFrom stats anova
.anova_es.rms <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.9,
                          ...) {

  if (!inherits(model, "anova.rms")) {
    model <- stats::anova(model)
  }

  model <- as.data.frame(model)

  colnames(model) <- gsub("F", "F value", colnames(model), fixed = TRUE)
  colnames(model) <- gsub("d.f.", "NumDF", colnames(model), fixed = TRUE)
  model$DenDF <- model$NumDF[rownames(model) == "ERROR"]

  model <- model[rownames(model) != "ERROR", ]

  out <- .anova_es.anova(model, type = type, partial = partial, generalized = generalized, ci = ci)
  return(out)
}

.anova_es.anova.rms <- .anova_es.rms


#' @keywords internal
#' @importFrom insight model_info
.cohens_f_delta <- function(model, model2, ci = 0.9, squared = FALSE) {
  # check
  if (!inherits(model, "lm") ||
      !inherits(model2, "lm") ||
      !insight::model_info(model)$is_linear ||
      !insight::model_info(model2)$is_linear) {
    stop("Cohen's f for R2-change only supported for fixed effect linear models.",
         call. = FALSE)
  }

  # Anova
  ANOVA <- anova(model, model2)
  out <- F_to_f(ANOVA[2, "F"], abs(ANOVA[2, "Df"]), min(ANOVA["Res.Df"]),
                ci = ci, squared = squared)

  if (requireNamespace("performance")) {
    R2d <- performance::r2(model)[[1]] - performance::r2(model2)[[1]]
    out$R2_delta <- abs(R2d)
  }

  return(out)
}
# Utils -------------------------------------------------------------------

#' @keywords internal
.anova_es_model_params <- function(model, f, df_num, df_error, type, ci) {
  #used by .anova_es.parameters_model
  out <- .F_to_pve(stats::na.omit(f), df = df_num, df_error = df_error, ci = ci, es = paste0(type, "2"))
  out$Parameter <- model$Parameter[!is.na(f)]
  out[c(ncol(out), 1:(ncol(out) - 1))]
}

