#' Effect size for ANOVA
#'
#' Functions to compute effect size measures for ANOVAs, such as Eta-
#' (\eqn{\eta}), Omega- (\eqn{\omega}) and Epsilon- (\eqn{\epsilon}) squared,
#' and Cohen's f (or their partialled versions) for ANOVA tables. These indices
#' represent an estimate of how much variance in the response variables is
#' accounted for by the explanatory variable(s).
#' \cr\cr
#' When passing models, effect sizes are computed using the sums of squares
#' obtained from `anova(model)` which might not always be appropriate. See
#' details.
#'
#' @param model A model, ANOVA object, or the result of `parameters::model_parameters`.
#' @param partial If `TRUE`, return partial indices.
#' @param generalized If TRUE, returns generalized Eta Squared, assuming all
#'   variables are manipulated. Can also be a character vector of observed
#'   (non-manipulated) variables, in which case generalized Eta Squared is
#'   calculated taking these observed variables into account. For `afex_aov`
#'   model, when `generalized = TRUE`, the observed variables are extracted
#'   automatically from the fitted model, if they were provided then.
#' @param verbose Toggle warnings and messages on or off.
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to or from other methods.
#'   - Can be `include_intercept = TRUE` to include the effect size for the intercept.
#'   - For Bayesian models, arguments passed to `ss_function`.
#'
#' @return
#' A data frame with the effect size(s) between 0-1 (`Eta2`, `Epsilon2`,
#' `Omega2`, `Cohens_f` or `Cohens_f2`, possibly with the `partial` or
#' `generalized` suffix), and their CIs (`CI_low` and `CI_high`).
#' \cr\cr
#' For `eta_squared_posterior()`, a data frame containing the ppd of the Eta
#' squared for each fixed effect, which can then be passed to
#' [bayestestR::describe_posterior()] for summary stats.
#'
#' @details
#'
#' For `aov`, `aovlist` and `afex_aov` models, and for `anova` objects that
#' provide Sums-of-Squares, the effect sizes are computed directly using
#' Sums-of-Squares (for `mlm` / `maov` models, effect sizes are computed for
#' each response separately). For all other model, effect sizes are approximated
#' via test statistic conversion of the omnibus *F* statistic provided by the
#' appropriate `anova()` method (see [`F_to_eta2()`] for more details.)
#'
#' ## Type of Sums of Squares
#' The sums of squares (or *F* statistics) used for the computation of the
#' effect sizes is based on those returned by `anova(model)` (whatever those may
#' be - for `aov` and `aovlist` these are *type-1* sums of squares; for
#' `lmerMod` (and `lmerModLmerTest`) these are *type-3* sums of squares). Make
#' sure these are the sums of squares you are interested in; You might want to
#' pass the result of `car::Anova(mode, type = 2)` or `type = 3` instead of the
#' model itself, or use the `afex` package to fit ANOVA models.
#' \cr\cr
#' For type 3 sum of squares, it is generally recommended to fit models with
#' *`contr.sum` factor weights* and *centered covariates*, for sensible results.
#' See examples and the `afex` package.
#'
#' ## Un-Biased Estimate of Eta
#' Both ***Omega*** and ***Epsilon*** are unbiased estimators of the
#' population's ***Eta***, which is especially important is small samples. But
#' which to choose?
#' \cr\cr
#' Though Omega is the more popular choice (Albers \& Lakens, 2018), Epsilon is
#' analogous to adjusted R2 (Allen, 2017, p. 382), and has been found to be less
#' biased (Carroll & Nordholm, 1975).
#' \cr\cr
#' (Note that for Omega- and Epsilon-squared it is possible to compute a
#' negative number; even though this doesn't make any practical sense, it is
#' recommended to report the negative number and not a 0.)
#'
#' ## Cohen's f
#' Cohen's f can take on values between zero, when the population means are all
#' equal, and an indefinitely large number as standard deviation of means
#' increases relative to the average standard deviation within each group.
#' \cr\cr
#' When comparing two models in a sequential regression analysis, Cohen's f for
#' R-square change is the ratio between the increase in R-square
#' and the percent of unexplained variance.
#' \cr\cr
#' Cohen has suggested that the values of 0.10, 0.25, and 0.40 represent small,
#' medium, and large effect sizes, respectively.
#'
#' ## Eta Squared from Posterior Predictive Distribution
#' For Bayesian models (fit with `brms` or `rstanarm`),
#' `eta_squared_posterior()` simulates data from the posterior predictive
#' distribution (ppd) and for each simulation the Eta Squared is computed for
#' the model's fixed effects. This means that the returned values are the
#' population level effect size as implied by the posterior model (and not the
#' effect size in the sample data). See [rstantools::posterior_predict()] for
#' more info.
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @seealso [F_to_eta2()]
#' @family effect size indices
#'
#' @examples
#' \donttest{
#' data(mtcars)
#' mtcars$am_f <- factor(mtcars$am)
#' mtcars$cyl_f <- factor(mtcars$cyl)
#'
#' model <- aov(mpg ~ am_f * cyl_f, data = mtcars)
#'
#' (eta2 <- eta_squared(model))
#'
#' # More types:
#' eta_squared(model, partial = FALSE)
#' eta_squared(model, generalized = "cyl_f")
#' omega_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#'
#' if (require(see)) plot(eta2)
#'
#' model0 <- aov(mpg ~ am_f + cyl_f, data = mtcars) # no interaction
#' cohens_f_squared(model0, model2 = model)
#'
#' ## Interpretation of effect sizes
#' ## -------------------------------------
#'
#' interpret_omega_squared(0.10, rules = "field2013")
#' interpret_eta_squared(0.10, rules = "cohen1992")
#' interpret_epsilon_squared(0.10, rules = "cohen1992")
#'
#' interpret(eta2, rules = "cohen1992")
#'
#' # Recommended: Type-3 effect sizes + effects coding
#' # -------------------------------------------------
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
#' # afex takes care of both type-3 effects and effects coding:
#' if (require(afex)) {
#'   data(obk.long, package = "afex")
#'   model <- aov_car(value ~ treatment * gender + Error(id / (phase)),
#'     data = obk.long, observed = "gender"
#'   )
#'   eta_squared(model)
#'   epsilon_squared(model)
#'   omega_squared(model)
#'   eta_squared(model, partial = FALSE)
#'   epsilon_squared(model, partial = FALSE)
#'   omega_squared(model, partial = FALSE)
#'   eta_squared(model, generalized = TRUE) # observed vars are pulled from the afex model.
#' }
#'
#'
#'
#' ## Approx. effect sizes for mixed models
#' ## -------------------------------------
#' if (require(lmerTest, quietly = TRUE)) {
#'   model <- lmer(mpg ~ am_f * cyl_f + (1 | vs), data = mtcars)
#'   omega_squared(model)
#' }
#'
#'
#'
#'
#' ## Bayesian Models (PPD)
#' ## ---------------------
#' \dontrun{
#' if (require(rstanarm) && require(bayestestR) && require(car)) {
#'   fit_bayes <- stan_glm(mpg ~ factor(cyl) * wt + qsec,
#'     data = mtcars,
#'     family = gaussian(),
#'     refresh = 0
#'   )
#'
#'   es <- eta_squared_posterior(fit_bayes,
#'     ss_function = car::Anova, type = 3
#'   )
#'   bayestestR::describe_posterior(es)
#'
#'
#'   # compare to:
#'   fit_freq <- lm(mpg ~ factor(cyl) * wt + qsec,
#'     data = mtcars
#'   )
#'   aov_table <- car::Anova(fit_freq, type = 3)
#'   eta_squared(aov_table)
#' }
#' }
#' }
#'
#' @return A data frame containing the effect size values and their confidence
#'   intervals.
#'
#' @references
#' - Albers, C., \& Lakens, D. (2018). When power analyses based on pilot data
#' are biased: Inaccurate effect size estimators and follow-up bias. Journal of
#' experimental social psychology, 74, 187-195.
#'
#' - Allen, R. (2017). Statistics and Experimental Design for Psychologists: A
#' Model Comparison Approach. World Scientific Publishing Company.
#'
#' - Carroll, R. M., & Nordholm, L. A. (1975). Sampling Characteristics of
#' Kelley's epsilon and Hays' omega. Educational and Psychological Measurement,
#' 35(3), 541-554.
#'
#' - Kelley, T. (1935) An unbiased correlation ratio measure. Proceedings of the
#' National Academy of Sciences. 21(9). 554-559.
#'
#' - Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
#' statistics: measures of effect size for some common research designs.
#' Psychological methods, 8(4), 434.
#'
#' - Steiger, J. H. (2004). Beyond the F test: Effect size confidence intervals
#' and tests of close fit in the analysis of variance and contrast analysis.
#' Psychological Methods, 9, 164-182.
#'
#' @export
eta_squared <- function(model,
                        partial = TRUE,
                        generalized = FALSE,
                        ci = 0.95, alternative = "greater",
                        verbose = TRUE,
                        ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  out <- .anova_es(
    model,
    type = "eta",
    partial = partial,
    generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )
  class(out) <- unique(c("effectsize_anova","effectsize_table", "see_effectsize_table", class(out)))
  if ("CI" %in% colnames(out)) attr(out, "ci_method") <- list(method = "ncp", distribution = "F")
  attr(out, "approximate") <- isTRUE(attr(out, "approximate", exact = TRUE))
  return(out)
}

#' @rdname eta_squared
#' @export
omega_squared <- function(model,
                          partial = TRUE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  out <- .anova_es(model, type = "omega", partial = partial, ci = ci, alternative = alternative, verbose = verbose, ...)
  class(out) <- unique(c("effectsize_anova","effectsize_table", "see_effectsize_table", class(out)))
  if ("CI" %in% colnames(out)) attr(out, "ci_method") <- list(method = "ncp", distribution = "F")
  attr(out, "approximate") <- isTRUE(attr(out, "approximate", exact = TRUE))
  return(out)
}

#' @rdname eta_squared
#' @export
epsilon_squared <- function(model,
                            partial = TRUE,
                            ci = 0.95, alternative = "greater",
                            verbose = TRUE,
                            ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  out <- .anova_es(model, type = "epsilon", partial = partial, ci = ci, alternative = alternative, verbose = verbose, ...)
  class(out) <- unique(c("effectsize_anova","effectsize_table", "see_effectsize_table", class(out)))
  if ("CI" %in% colnames(out)) attr(out, "ci_method") <- list(method = "ncp", distribution = "F")
  attr(out, "approximate") <- isTRUE(attr(out, "approximate", exact = TRUE))
  return(out)
}

#' @rdname eta_squared
#' @inheritParams F_to_f
#' @param model2 Optional second model for Cohen's f (/squared). If specified,
#'   returns the effect size for R-squared-change between the two models.
#' @export
cohens_f <- function(model, partial = TRUE, ci = 0.95, alternative = "greater", squared = FALSE,
                     verbose = TRUE,
                     model2 = NULL, ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))
  if (!is.null(model2)) {
    return(.cohens_f_delta(model, model2, ci = ci, alternative = alternative, squared = squared, verbose = verbose))
  }

  res <- eta_squared(model,
    partial = partial,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )

  if ("Eta2_partial" %in% colnames(res)) {
    res$Eta2_partial <- res$Eta2_partial / (1 - res$Eta2_partial)
    colnames(res)[colnames(res) == "Eta2_partial"] <- "Cohens_f2_partial"
  } else {
    res$Eta2 <- res$Eta2 / (1 - res$Eta2)
    colnames(res)[colnames(res) == "Eta2"] <- "Cohens_f2"
  }

  if (is.numeric(ci)) {
    res$CI_low <- res$CI_low / (1 - res$CI_low)
    res$CI_high <- res$CI_high / (1 - res$CI_high)
  }


  if (!squared) {
    i <- colnames(res) %in% c("Cohens_f2", "Cohens_f2_partial", "CI_low", "CI_high")
    res[i] <- sqrt(res[i])
    colnames(res)[colnames(res) %in% c("Cohens_f2", "Cohens_f2_partial")] <-
      if ("Cohens_f2" %in% colnames(res)) "Cohens_f" else "Cohens_f_partial"
  }

  if ("CI" %in% colnames(res)) attr(res, "ci_method") <- list(method = "ncp", distribution = "F")
  class(res) <- unique(c("effectsize_anova","effectsize_table", "see_effectsize_table", class(res)))
  attr(res, "approximate") <- isTRUE(attr(res, "approximate", exact = TRUE))
  res
}

#' @rdname eta_squared
#' @export
cohens_f_squared <- function(model, partial = TRUE, ci = 0.95, alternative = "greater", squared = TRUE,
                             verbose = TRUE,
                             model2 = NULL, ...) {
  cohens_f(
    model,
    partial = partial,
    ci = ci, alternative = alternative,
    squared = squared,
    verbose = verbose,
    model2 = model2,
    ...
  )
}


#' @keywords internal
#' @importFrom insight model_info
.cohens_f_delta <- function(model, model2, ci = 0.95, alternative = "greater", squared = FALSE, verbose = TRUE) {
  # check
  if (!inherits(model, "lm") ||
      !inherits(model2, "lm") ||
      !insight::model_info(model)$is_linear ||
      !insight::model_info(model2)$is_linear) {
    stop("Cohen's f for R2-change only supported for fixed effect linear models.",
         call. = FALSE
    )
  }

  # Anova
  ANOVA <- anova(model, model2)
  out <- F_to_f(ANOVA[2, "F"], abs(ANOVA[2, "Df"]), min(ANOVA["Res.Df"]),
                ci = ci, alternative = alternative,
                squared = squared)

  R2d <- performance::r2(model)[[1]] - performance::r2(model2)[[1]]
  out$R2_delta <- abs(R2d)

  return(out)
}

# Get ES ------------------------------------------------------------------

#' @keywords internal
.anova_es <-
  function(model,
           type = c("eta", "omega", "epsilon"),
           partial = TRUE,
           generalized = FALSE,
           ci = 0.95, alternative = "greater",
           verbose = TRUE,
           ...) {
    UseMethod(".anova_es")
  }

#' @keywords internal
#' @importFrom stats anova
.anova_es.default <- function(model,
                              type = c("eta", "omega", "epsilon"),
                              partial = TRUE,
                              generalized = FALSE,
                              ci = 0.95, alternative = "greater",
                              verbose = TRUE,
                              ...) {
  .anova_es.anova(
    stats::anova(model),
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci,
    alternative = alternative,
    verbose = verbose
  )
}

#' @keywords internal
#' @importFrom parameters model_parameters
#' @importFrom stats anova
.anova_es.aov <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  if (!inherits(model, c("Gam", "anova"))) {
    # Pass to ANOVA table method
    res <- .anova_es.anova(
      stats::anova(model),
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose,
      ...
    )
    return(res)
  }

  type <- match.arg(type)

  params <- parameters::model_parameters(model, verbose = verbose, effects = "fixed")
  out <- .es_aov(as.data.frame(params), type, partial, generalized, ci, alternative, verbose = verbose, ...)
  if (is.null(attr(out, "anova_type"))) attr(out, "anova_type") <- attr(params, "anova_type")
  out
}

#' @keywords internal
.es_aov <- function(params,
                    type = c("eta", "omega", "epsilon"),
                    partial = TRUE,
                    generalized = FALSE,
                    ci = 0.95, alternative = "greater",
                    verbose = TRUE,
                    include_intercept = FALSE,
                    ...) {

  if (!"Residuals" %in% params$Parameter) {
    stop(
      "No residuals data found - ",
      type,
      " squared can only be computed for simple `aov` models."
    )
  }

  if (include_intercept) {
    values <- .values_aov(params[params$Parameter != "(Intercept)", ])
  } else {
    params <- params[params$Parameter != "(Intercept)", ]
    values <- .values_aov(params)
  }

  df_error <- params$df[params$Parameter == "Residuals"]
  params <- params[params$Parameter != "Residuals", , drop = FALSE]

  if (nrow(params) == 1L &&
    (partial || isTRUE(generalized) || is.character(generalized))) {
    if (verbose) {
      txt_type <- ifelse(isTRUE(generalized) || is.character(generalized), "generalized", "partial")
      message(
        "For one-way between subjects designs, ", txt_type, " ", type, " squared is equivalent to ", type, " squared.\n",
        "Returning ", type, " squared."
      )
    }
    partial <- FALSE
    anova_type <- NA
  } else {
    anova_type <- NULL
  }



  if (type == "eta") {
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

      params$Eta2_generalized <- params$Sum_Squares /
        (params$Sum_Squares + values$Sum_Squares_residuals + obs_SSn1 - obs_SSn2)
    } else if (!isTRUE(partial)) {
      params$Eta2 <- params$Sum_Squares /
        values$Sum_Squares_total
    } else {
      params$Eta2_partial <-
        params$Sum_Squares /
          (params$Sum_Squares + values$Sum_Squares_residuals)
    }
  } else if (type == "omega") {
    if (!isTRUE(partial)) {
      params$Omega2 <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
          (values$Sum_Squares_total + values$Mean_Square_residuals)
    } else {
      params$Omega2_partial <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
          (params$Sum_Squares + (values$n - params$df) * values$Mean_Square_residuals)
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      params$Epsilon2 <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
          values$Sum_Squares_total
    } else {
      params$Epsilon2_partial <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
          (params$Sum_Squares + values$Sum_Squares_residuals)
    }
  }



  out <- params

  if (is.numeric(ci)) {
    # based on MBESS::ci.R2
    ES <- pmax(0, out[[ncol(out)]])
    f <- (ES / out$df) / ((1 - ES) / df_error)

    out <- cbind(
      out,
      # This really is a generic F_to_R2
      F_to_eta2(f,
        out$df,
        df_error,
        ci = ci, alternative = alternative
      )[-1]
    )
  } else {
    alternative <- NULL
  }


  out <- out[, colnames(out) %in% c(
    "Group", "Response", "Parameter",
    "Eta2", "Eta2_partial", "Eta2_generalized",
    "Omega2", "Omega2_partial",
    "Epsilon2", "Epsilon2_partial",
    "CI", "CI_low", "CI_high"
  ), drop = FALSE]

  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "anova_type") <- anova_type
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  out
}

.anova_es.lm <- .anova_es.aov

.anova_es.glm <- .anova_es.aov

.anova_es.manova <- .anova_es.aov

#' @keywords internal
#' @importFrom parameters model_parameters
#' @importFrom insight find_predictors
.anova_es.aovlist <- function(model,
                              type = c("eta", "omega", "epsilon"),
                              partial = TRUE,
                              generalized = FALSE,
                              ci = 0.95, alternative = "greater",
                              verbose = TRUE,
                              include_intercept = FALSE,
                              ...) {
  params <- parameters::model_parameters(model, verbose = verbose, effects = "fixed")
  anova_type <- attr(params, "anova_type")
  params <- as.data.frame(params)

  IVs <- insight::find_predictors(model)[[1]]

  out <-
    .es_aovlist(
      params,
      IVs = IVs,
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose,
      include_intercept = include_intercept
    )
  attr(out, "anova_type") <- anova_type
  out
}

#' @keywords internal
.es_aovlist <- function(params, IVs,
                        type = c("eta", "omega", "epsilon"),
                        partial = TRUE,
                        generalized = FALSE,
                        ci = 0.95, alternative = "greater",
                        verbose = TRUE,
                        include_intercept = FALSE) {
  type <- match.arg(type)

  if (!"Residuals" %in% params$Parameter) {
    stop(
      "No residuals data found - ",
      type,
      " squared can only be computed for simple `aov` models."
    )
  }

  if (include_intercept) {
    values <- .values_aov(params[params$Parameter != "(Intercept)", ])
  } else {
    params <- params[params$Parameter != "(Intercept)", ]
    values <- .values_aov(params)
  }

  params <- params[params$Parameter != "Residuals" & !is.na(params$`F`), , drop = FALSE]
  Sum_Squares_total <- sum(sapply(values, "[[", "Sum_Squares_total"))
  Sum_Squares_residuals <- sapply(values[params$Group], "[[", "Sum_Squares_residuals")
  Mean_Square_residuals <- sapply(values[params$Group], "[[", "Mean_Square_residuals")
  df_residuals <- sapply(values[params$Group], "[[", "df_residuals")
  ns <- sapply(values[params$Group], "[[", "n")


  if (type == "eta") {
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

      params$Eta2_generalized <- params$Sum_Squares /
        (params$Sum_Squares + sum(sapply(values, "[[", "Sum_Squares_residuals")) +
           obs_SSn1 - obs_SSn2)
    } else if (!isTRUE(partial)) {
      params$Eta2 <- params$Sum_Squares / Sum_Squares_total
    } else {
      params$Eta2_partial <-
        params$Sum_Squares /
        (params$Sum_Squares + Sum_Squares_residuals)
    }
  } else if (type == "omega") {
    SSS_values <- values[[which(names(values) %in% IVs)]]
    is_within <- !params$Group %in% IVs
    Sum_Squares_Subjects <- SSS_values$Sum_Squares_residuals
    Mean_Squares_Subjects <- SSS_values$Mean_Square_residuals

    # implemented from https://www.jasonfinley.com/tools/OmegaSquaredQuickRef_JRF_3-31-13.pdf
    if (!isTRUE(partial)) {
      params$Omega2 <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        (Sum_Squares_total + Mean_Squares_Subjects)
    } else {
      params$Omega2_partial <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        (params$Sum_Squares + is_within * Sum_Squares_residuals +
           Sum_Squares_Subjects + Mean_Squares_Subjects)
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      params$Epsilon2 <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        Sum_Squares_total
    } else {
      params$Epsilon2_partial <-
        (params$Sum_Squares - params$df * Mean_Square_residuals) /
        (params$Sum_Squares + Sum_Squares_residuals)
    }
  }

  out <- params

  if (!is.null(ci)) {
    # based on MBESS::ci.R2
    ES <- pmax(0, out[[ncol(out)]])
    f <- (ES / out$df) / ((1 - ES) / df_residuals)

    out <- cbind(
      out,
      # This really is a generic F_to_R2
      F_to_eta2(f,
                out$df,
                df_residuals,
                ci = ci, alternative = alternative
      )[-1]
    )
  } else {
    alternative <- NULL
  }

  out <- out[, colnames(out) %in% c(
    "Group",
    "Response",
    "Parameter",
    "Eta2_generalized",
    "Eta2_partial",
    "Omega2_partial",
    "Epsilon2_partial",
    "Eta2",
    "Omega2",
    "Epsilon2",
    "CI",
    "CI_low",
    "CI_high"
  ), drop = FALSE]
  rownames(out) <- NULL

  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  out
}

#' @keywords internal
#' @importFrom stats aov
#' @importFrom utils packageVersion
.anova_es.mlm <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  model <- stats::aov(model)
  params <- parameters::model_parameters(model, verbose = verbose, effects = "fixed")
  anova_type <- attr(params, "anova_type")

  params <- as.data.frame(params)
  params <- split(params, params$Response)
  params <- lapply(params, .es_aov,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )
  out <- do.call("rbind", params)
  rownames(out) <- NULL

  attr(out, "partial") <- attr(params[[1]], "partial")
  attr(out, "generalized") <- attr(params[[1]], "generalized")
  attr(out, "ci") <- attr(params[[1]], "ci", exact = TRUE)
  attr(out, "anova_type") <- anova_type
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- if (is.numeric(attr(out, "ci"))) alternative
  out
}

.anova_es.maov <- .anova_es.mlm

#' @keywords internal
.anova_es.anova <- function(model,
                            type = c("eta", "omega", "epsilon"),
                            partial = TRUE,
                            generalized = FALSE,
                            ci = 0.95, alternative = "greater",
                            verbose = TRUE,
                            include_intercept = FALSE,
                            ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
    eta = F_to_eta2,
    omega = F_to_omega2,
    epsilon = F_to_epsilon2
  )

  F_val <- c("F value", "approx F", "F-value")
  numDF <- c("NumDF", "num Df", "numDF", "npar")
  denDF <- c("DenDF", "den Df", "denDF", "df_error")

  if (!any(denDF %in% colnames(model))) {
    # Pass to AOV method
    res <- .anova_es.aov(model,
      partial = partial,
      type = type,
      generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose,
      include_intercept = include_intercept,
      ...
    )
    return(res)
  }

  anova_type <- tryCatch(attr(parameters::model_parameters(model, verbose = FALSE, effects = "fixed"), "anova_type"),
                         error = function(...) 1)

  if (!include_intercept) model <- model[rownames(model) != "(Intercept)", , drop = FALSE]
  model <- model[rownames(model) != "Residuals", , drop = FALSE]

  F_val <- F_val[F_val %in% colnames(model)]
  numDF <- numDF[numDF %in% colnames(model)]
  denDF <- denDF[denDF %in% colnames(model)]


  if (verbose && !isTRUE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for this class of objects.",
      call. = FALSE
    )
    partial <- TRUE
  }

  if (verbose && (isTRUE(generalized) || is.character(generalized))) {
    warning(
      "generalized ", type, " squared ",
      "is not supported for this class of object."
    )
    generalized <- FALSE
  }

  par_table <- as.data.frame(model)

  out <- cbind(
    Parameter = rownames(par_table),
    es_fun(par_table[[F_val]],
      par_table[[numDF]],
      par_table[[denDF]],
      ci = ci, alternative = alternative
    )
  )

  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "anova_type") <- anova_type
  attr(out, "alternative") <- if (is.numeric(ci)) alternative
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
                                       ci = 0.95, alternative = "greater",
                                       verbose = TRUE,
                                       ...) {
  if (verbose && !isTRUE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for this class of objects.",
      call. = FALSE
    )
    partial <- TRUE
  }

  if (verbose && (isTRUE(generalized) || is.character(generalized))) {
    warning(
      "generalized ", type, " squared ",
      "is not supported for this class of object."
    )
    generalized <- FALSE
  }


  type <- match.arg(type)

  if ("Group" %in% colnames(model) && sum(model$Parameter == "Residuals") > 1) {
    x <- split(model, model$Group)
    out <- do.call(rbind, lapply(x, function(i) {
      f <- i[["F"]]
      df_num <- i[["df"]][!is.na(f)]
      df_error <- i[i$Parameter == "Residuals", "df"]
      cbind(
        data.frame(Group = unique(i$Group), stringsAsFactors = FALSE),
        .anova_es_model_params(i, f, df_num, df_error, type, ci, alternative)
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
    out <- .anova_es_model_params(model, f, df_num, df_error, type, ci, alternative)
  }

  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "alternative") <- if (is.numeric(ci)) alternative
  out
}


# Specific models ---------------------------------------------------------

#' @keywords internal
.anova_es.htest <- function(model,
                            type = c("eta", "omega", "epsilon"),
                            partial = TRUE,
                            generalized = FALSE,
                            ci = 0.95, alternative = "greater",
                            verbose = TRUE,
                            ...){
  if (!grepl("One-way", model$method))
    stop("'model' is not a one-way test!", call. = FALSE)

  if (verbose && (partial || isTRUE(generalized) || is.character(generalized))) {
    txt_type <- ifelse(isTRUE(generalized) || is.character(generalized), "generalized", "partial")
    message(
      "For one-way between subjects designs, ", txt_type, " ", type, " squared is equivalent to ", type, " squared.\n",
      "Returning ", type, " squared."
    )
  }

  effectsize(model, type = type, ci = ci, alternative = alternative, verbose = verbose, ...)
}

#' @keywords internal
.anova_es.Anova.mlm <- function(model,
                                type = c("eta", "omega", "epsilon"),
                                partial = TRUE,
                                generalized = FALSE,
                                ci = 0.95, alternative = "greater",
                                verbose = TRUE,
                                include_intercept = FALSE,
                                ...) {
  # ## For the univariate test
  # model <- summary(mlm1.aov)$univariate.tests
  # .anova_es.anova(model, type = type,
  #                 partial = partial,
  #                 generalized = generalized,
  #                 ci = ci, alternative = alternative,
  #                 verbose = verbose,
  #                 ...)

  ## For the multivariate test
  model <- parameters::model_parameters(model, verbose = verbose, effects = "fixed", ...)
  anova_type <- attr(model, "anova_type")

  if ("df_num" %in% colnames(model))
    model$df <- model$df_num
  if (!include_intercept)
    model <- model[model$Parameter != "(Intercept)", , drop = FALSE]
  out <- .anova_es.parameters_model(model, type = type,
                                    partial = partial,
                                    generalized = generalized,
                                    ci = ci, alternative = alternative,
                                    verbose = verbose,
                                    ...)
  attr(out, "anova_type") <- anova_type
  out
}

#' @keywords internal
#' @importFrom stats anova
#' @importFrom insight check_if_installed
.anova_es.merMod <- function(model,
                             type = c("eta", "omega", "epsilon"),
                             partial = TRUE,
                             generalized = FALSE,
                             ci = 0.95, alternative = "greater",
                             verbose = TRUE,
                             ...) {
  insight::check_if_installed("lmerTest")

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  .anova_es.anova(model, type = type, partial = partial, generalized = generalized, ci = ci, alternative = alternative, ...)
}

#' @keywords internal
#' @importFrom stats anova
.anova_es.gam <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  type <- match.arg(type)
  es_fun <- switch(type,
    eta = F_to_eta2,
    omega = F_to_omega2,
    epsilon = F_to_epsilon2
  )

  if (verbose && !isTRUE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for repeated-measures / multi-variate ANOVAs",
      call. = FALSE
    )
    partial <- TRUE
  }

  if (verbose && (isTRUE(generalized) || is.character(generalized))) {
    warning(
      "generalized ", type, " squared ",
      "is not supported for this class of object."
    )
    generalized <- FALSE
  }

  model <- stats::anova(model)

  p.table <- as.data.frame(model$pTerms.table)
  s.table <- as.data.frame(model$s.table)
  colnames(s.table)[colnames(s.table)=="Ref.df"] <- "df"
  s.table[setdiff(colnames(p.table), colnames(s.table))] <- NA
  p.table[setdiff(colnames(s.table), colnames(p.table))] <- NA
  tab <- rbind(p.table, s.table)
  colnames(tab)[colnames(tab)=="F"] <- "F-value"
  colnames(tab)[colnames(tab)=="df"] <- "npar"
  tab$df_error <- model$residual.df

  out <-
    .anova_es.anova(
      tab,
      type = type,
      generalized = generalized,
      partial = partial,
      ci = ci, alternative = alternative,
      verbose = verbose
    )

  attr(out, "anova_type") <- 3
  out
}

#' @keywords internal
.anova_es.afex_aov <- function(model,
                               type = c("eta", "omega", "epsilon"),
                               partial = TRUE,
                               generalized = FALSE,
                               ci = 0.95, alternative = "greater",
                               verbose = TRUE,
                               include_intercept = FALSE,
                               ...) {
  type <- match.arg(type)
  if (type == "eta" && isTRUE(generalized) && length(attr(model$anova_table, "observed"))) {
    generalized <- attr(model$anova_table, "observed")
  }

  # For completely between, covers all
  if (!inherits(model$Anova, "Anova.mlm")) {
    out <-
      .anova_es(
        model$Anova,
        type = type,
        partial = partial,
        generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = FALSE,
        include_intercept = include_intercept,
        ...
      )
  } else {
    # Faking the model_parameters.aovlist output:
    aov_tab <- summary(model$Anova)$univariate.tests
    aov_tab <- as.data.frame(unclass(aov_tab))
    aov_tab$Parameter <- rownames(aov_tab)
    colnames(aov_tab)[colnames(aov_tab)== "Sum Sq"] <- "Sum_Squares"
    colnames(aov_tab)[colnames(aov_tab)== "num Df"] <- "df"
    aov_tab <- aov_tab[c("Parameter", "Sum_Squares","Error SS", "df", "den Df")]

    id <- attr(model, "id")

    within <- c(NA,names(attr(model, "within")))
    within <- utils::combn(within, length(within) - 1)
    within <- apply(within, 2, as.list)
    within <- Filter(f = function(x) !all(is.na(x)), within)
    within <- lapply(within, Filter, f = Negate(is.na))
    within <- sapply(within, paste0, collapse = ":")
    l <- sapply(within, grepl, x = aov_tab$Parameter, simplify = TRUE)
    l <- apply(l, 1, function(x) if (!any(x)) 0 else max(which(x)))
    l <- c(NA, within)[l+1]
    l <- sapply(l, function(x) paste0(na.omit(c(id, x)), collapse = ":"))
    aov_tab$Group <- l

    aov_tab <- split(aov_tab, aov_tab$Group)
    aov_tab <- lapply(aov_tab, function (x) {
      x <- x[c(seq_len(nrow(x)), 1), ]
      x$Sum_Squares[nrow(x)] <- x[["Error SS"]][1]
      x$df[nrow(x)] <- x[["den Df"]][1]
      x$Parameter[nrow(x)] <- "Residuals"
      x
    })
    aov_tab <- do.call(rbind, aov_tab)
    aov_tab[["Error SS"]] <- NULL
    aov_tab[["den Df"]] <- NULL
    aov_tab$`F` <- ifelse(aov_tab$Parameter == "Residuals", NA, 1)
    aov_tab$Mean_Square <- aov_tab$Sum_Squares/aov_tab$df

    IVs <- c(id, names(attr(model, "within")), names(attr(model, "between")))

    out <-
      .es_aovlist(
        aov_tab,
        IVs = IVs,
        type = type,
        partial = partial,
        generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose,
        include_intercept = include_intercept
      )
    out$Group <- NULL
  }

  # Reorder rows
  orig_terms <- rownames(model$anova_table)
  if (include_intercept && !"(Intercept)" %in% orig_terms) {
    orig_terms <- c("(Intercept)", orig_terms)
  }
  out <- out[match(out$Parameter, orig_terms),]
  attr(out, "anova_type") <- attr(model, "type", exact = TRUE)
  attr(out, "approximate") <- FALSE
  out
}



#' @keywords internal
#' @importFrom stats anova
.anova_es.rms <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          ...) {
  if (!inherits(model, "anova.rms")) {
    model <- stats::anova(model, test = "F")
  }
  i <- rownames(model)
  model <- as.data.frame(model)

  colnames(model) <- gsub("F", "F value", colnames(model), fixed = TRUE)
  colnames(model) <- gsub("d.f.", "NumDF", colnames(model), fixed = TRUE)
  model$DenDF <- model$NumDF[rownames(model) == "ERROR"]

  model <- model[rownames(model) != "ERROR", ]

  out <- .anova_es.anova(model, type = type, partial = partial, generalized = generalized, ci = ci, alternative = alternative, ...)
  out$Parameter <- i[match(make.names(i), out$Parameter, nomatch = 0)]
  attr(out, "anova_type") <- 2
  out
}

.anova_es.anova.rms <- .anova_es.rms


#' @export
.anova_es.model_fit <- function(model,
                                type = c("eta", "omega", "epsilon"),
                                partial = TRUE,
                                generalized = FALSE,
                                ci = 0.95, alternative = "greater",
                                verbose = TRUE,
                                ...) {
  .anova_es(
    model$fit,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )
}

# Utils -------------------------------------------------------------------

#' @keywords internal
.anova_es_model_params <- function(model, f, df_num, df_error, type, ci, alternative) {
  # used by .anova_es.parameters_model
  out <- .F_to_pve(stats::na.omit(f), df = df_num, df_error = df_error, ci = ci, alternative = alternative, es = paste0(type, "2"))
  out$Parameter <- model$Parameter[!is.na(f)]
  out[c(ncol(out), 1:(ncol(out) - 1))]
}
