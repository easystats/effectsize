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
#' ## ------------------------------
#'
#' interpret_omega_squared(0.10, rules = "field2013")
#' interpret_eta_squared(0.10, rules = "cohen1992")
#' interpret_epsilon_squared(0.10, rules = "cohen1992")
#'
#' interpret(eta2, rules = "cohen1992")
#'
#'
#' @examplesIf require("car") && require("afex")
#' # Recommended: Type-2 or -3 effect sizes + effects coding
#' # -------------------------------------------------------
#' contrasts(mtcars$am_f) <- contr.sum
#' contrasts(mtcars$cyl_f) <- contr.sum
#'
#' model <- aov(mpg ~ am_f * cyl_f, data = mtcars)
#' model_anova <- car::Anova(model, type = 3)
#'
#' eta_squared(model_anova)
#'
#' # afex takes care of both type-3 effects and effects coding:
#' data(obk.long, package = "afex")
#' model <- afex::aov_car(value ~ treatment * gender + Error(id / (phase)),
#'                        data = obk.long, observed = "gender")
#' eta_squared(model)
#' epsilon_squared(model)
#' omega_squared(model)
#' eta_squared(model, generalized = TRUE) # observed vars are pulled from the afex model.
#'
#'
#' @examplesIf require("lmerTest") && require("lme4")
#' ## Approx. effect sizes for mixed models
#' ## -------------------------------------
#' model <- lme4::lmer(mpg ~ am_f * cyl_f + (1 | vs), data = mtcars)
#' omega_squared(model)
#'
#'
#' @examplesIf require(rstanarm) && require(bayestestR) && require(car)
#' ## Bayesian Models (PPD)
#' ## ---------------------
#' \dontrun{
#' fit_bayes <- rstanarm::stan_glm(
#'   mpg ~ factor(cyl) * wt + qsec,
#'   data = mtcars, family = gaussian(),
#'   refresh = 0
#' )
#'
#' es <- eta_squared_posterior(fit_bayes, verbose = FALSE,
#'                             ss_function = car::Anova, type = 3)
#' bayestestR::describe_posterior(es, test = NULL)
#'
#'
#' # compare to:
#' fit_freq <- lm(mpg ~ factor(cyl) * wt + qsec,
#'                data = mtcars)
#' aov_table <- car::Anova(fit_freq, type = 3)
#' eta_squared(aov_table)
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
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
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
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
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
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
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
  alternative <- match.arg(alternative, c("greater", "two.sided", "less"))
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

#' @param aov_table Input data frame
#' @param type Which effect size to compute?
#' @param include_intercept Should the intercept (`(Intercept)`) be included?
#' @param partial,generalized,ci,alternative,verbose See [eta_squared()].
#'
#' @rdname effectsize_API
#' @export
.es_aov_simple <- function(aov_table,
                           type = c("eta", "omega", "epsilon"),
                           partial = TRUE,
                           generalized = FALSE,
                           ci = 0.95, alternative = "greater",
                           verbose = TRUE,
                           include_intercept = FALSE) {
  type <- match.arg(type)
  aov_table <- as.data.frame(aov_table)

  # Clean up data ---
  if (!"Mean_Square" %in% colnames(aov_table)) {
    aov_table[["Mean_Square"]] <- aov_table[["Sum_Squares"]] / aov_table[["df"]]
  }

  if (!"Residuals" %in% aov_table$Parameter) {
    stop(insight::format_message("No residuals data found - cannot compute effect size."))
  }


  # Include intercept? ---
  if (include_intercept) {
    values <- .values_aov(aov_table[aov_table$Parameter != "(Intercept)", ])
  } else {
    aov_table <- aov_table[aov_table$Parameter != "(Intercept)", ]
    values <- .values_aov(aov_table)
  }

  # Get error df ---
  df_error <- aov_table$df[aov_table$Parameter == "Residuals"]
  aov_table <- aov_table[aov_table$Parameter != "Residuals", , drop = FALSE]


  # Validate anova type (1,2,3) and partial ---
  anova_type <- NULL
  if (nrow(aov_table) == 1L &&
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
  }


  # Estimate effect size ---
  if (type == "eta") {
    if (isTRUE(generalized) || is.character(generalized)) {
      ## copied from afex
      obs <- logical(nrow(aov_table))
      if (is.character(generalized)) {
        for (o in generalized) {
          oi <- grepl(paste0("\\b", o, "\\b"), aov_table$Parameter)

          if (!any(oi)) stop("Observed variable not in data: ", o, call. = FALSE)

          obs <- obs | oi
        }
      }
      obs_SSn1 <- sum(aov_table$Sum_Squares * obs)
      obs_SSn2 <- aov_table$Sum_Squares * obs

      aov_table$Eta2_generalized <- aov_table$Sum_Squares /
        (aov_table$Sum_Squares + values$Sum_Squares_residuals + obs_SSn1 - obs_SSn2)
    } else if (!isTRUE(partial)) {
      aov_table$Eta2 <- aov_table$Sum_Squares /
        values$Sum_Squares_total
    } else {
      aov_table$Eta2_partial <-
        aov_table$Sum_Squares /
        (aov_table$Sum_Squares + values$Sum_Squares_residuals)
    }
  } else if (type == "omega") {
    if (!isTRUE(partial)) {
      aov_table$Omega2 <-
        (aov_table$Sum_Squares - aov_table$df * values$Mean_Square_residuals) /
        (values$Sum_Squares_total + values$Mean_Square_residuals)
    } else {
      aov_table$Omega2_partial <-
        (aov_table$Sum_Squares - aov_table$df * values$Mean_Square_residuals) /
        (aov_table$Sum_Squares + (values$n - aov_table$df) * values$Mean_Square_residuals)
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      aov_table$Epsilon2 <-
        (aov_table$Sum_Squares - aov_table$df * values$Mean_Square_residuals) /
        values$Sum_Squares_total
    } else {
      aov_table$Epsilon2_partial <-
        (aov_table$Sum_Squares - aov_table$df * values$Mean_Square_residuals) /
        (aov_table$Sum_Squares + values$Sum_Squares_residuals)
    }
  }

  out <- aov_table

  # Add CIs ---
  if (is.numeric(ci)) {
    # based on MBESS::ci.R2
    ES <- pmax(0, out[[ncol(out)]])
    f <- (ES / out$df) / ((1 - ES) / df_error)

    CI_tab <- # This really is a generic F_to_R2
      F_to_eta2(f,
                out$df,
                df_error,
                ci = ci, alternative = alternative,
                verbose = verbose
      )[-1]

    out[c("CI", "CI_low", "CI_high")] <- CI_tab[c("CI", "CI_low", "CI_high")]
  } else {
    alternative <- NULL
  }


  # Clean up output ---
  out <- out[, colnames(out) %in% c(
    "Parameter",
    "Eta2", "Eta2_partial", "Eta2_generalized",
    "Omega2", "Omega2_partial",
    "Epsilon2", "Epsilon2_partial",
    if (!is.null(ci)) c("CI", "CI_low", "CI_high")
  ), drop = FALSE]
  rownames(out) <- NULL

  # Set attributes ---
  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "anova_type") <- anova_type
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  out
}

#' @param DV_names A character vector with the names of all the predictors,
#'   including the grouping variable (e.g., `"Subject"`).
#'
#' @rdname effectsize_API
#' @export
.es_aov_strata <- function(aov_table, DV_names,
                           type = c("eta", "omega", "epsilon"),
                           partial = TRUE,
                           generalized = FALSE,
                           ci = 0.95, alternative = "greater",
                           verbose = TRUE,
                           include_intercept = FALSE) {
  type <- match.arg(type)
  aov_table <- as.data.frame(aov_table)

  # Clean up data ---
  if (!"Mean_Square" %in% colnames(aov_table)) {
    aov_table[["Mean_Square"]] <- aov_table[["Sum_Squares"]] / aov_table[["df"]]
  }

  if (!"Residuals" %in% aov_table$Parameter) {
    stop(insight::format_message("No residuals data found - cannot compute effect size."))
  }


  # Include intercept? ---
  if (include_intercept) {
    values <- .values_aov(aov_table[aov_table$Parameter != "(Intercept)", ], group = TRUE)
  } else {
    aov_table <- aov_table[aov_table$Parameter != "(Intercept)", ]
    values <- .values_aov(aov_table, group = TRUE)
  }


  # Get all the correct SSs... ---
  aov_table <- aov_table[aov_table$Parameter != "Residuals", , drop = FALSE]
  Sum_Squares_total <- sum(sapply(values, "[[", "Sum_Squares_total"))
  Sum_Squares_residuals <- sapply(values[aov_table$Group], "[[", "Sum_Squares_residuals")
  Mean_Square_residuals <- sapply(values[aov_table$Group], "[[", "Mean_Square_residuals")
  df_residuals <- sapply(values[aov_table$Group], "[[", "df_residuals")
  ns <- sapply(values[aov_table$Group], "[[", "n")


  # Estimate effect size ---
  if (type == "eta") {
    if (isTRUE(generalized) || is.character(generalized)) {
      ## copied from afex
      obs <- logical(nrow(aov_table))
      if (is.character(generalized)) {
        for (o in generalized) {
          oi <- grepl(paste0("\\b", o, "\\b"), aov_table$Parameter)

          if (!any(oi)) stop("Observed variable not in data: ", o, call. = FALSE)

          obs <- obs | oi
        }
      }
      obs_SSn1 <- sum(aov_table$Sum_Squares * obs)
      obs_SSn2 <- aov_table$Sum_Squares * obs

      aov_table$Eta2_generalized <- aov_table$Sum_Squares /
        (aov_table$Sum_Squares + sum(sapply(values, "[[", "Sum_Squares_residuals")) +
           obs_SSn1 - obs_SSn2)
    } else if (!isTRUE(partial)) {
      aov_table$Eta2 <- aov_table$Sum_Squares / Sum_Squares_total
    } else {
      aov_table$Eta2_partial <-
        aov_table$Sum_Squares /
        (aov_table$Sum_Squares + Sum_Squares_residuals)
    }
  } else if (type == "omega") {
    SSS_values <- values[[which(names(values) %in% DV_names)]]
    is_within <- !aov_table$Group %in% DV_names
    Sum_Squares_Subjects <- SSS_values$Sum_Squares_residuals
    Mean_Squares_Subjects <- SSS_values$Mean_Square_residuals

    # implemented from https://www.jasonfinley.com/tools/OmegaSquaredQuickRef_JRF_3-31-13.pdf/
    if (!isTRUE(partial)) {
      aov_table$Omega2 <-
        (aov_table$Sum_Squares - aov_table$df * Mean_Square_residuals) /
        (Sum_Squares_total + Mean_Squares_Subjects)
    } else {
      aov_table$Omega2_partial <-
        (aov_table$Sum_Squares - aov_table$df * Mean_Square_residuals) /
        (aov_table$Sum_Squares + is_within * Sum_Squares_residuals +
           Sum_Squares_Subjects + Mean_Squares_Subjects)
    }
  } else if (type == "epsilon") {
    if (!isTRUE(partial)) {
      aov_table$Epsilon2 <-
        (aov_table$Sum_Squares - aov_table$df * Mean_Square_residuals) /
        Sum_Squares_total
    } else {
      aov_table$Epsilon2_partial <-
        (aov_table$Sum_Squares - aov_table$df * Mean_Square_residuals) /
        (aov_table$Sum_Squares + Sum_Squares_residuals)
    }
  }

  out <- aov_table


  # Add CIs ---
  if (!is.null(ci)) {
    # based on MBESS::ci.R2
    ES <- pmax(0, out[[ncol(out)]])
    f <- (ES / out$df) / ((1 - ES) / df_residuals)

    CI_tab <- # This really is a generic F_to_R2
      F_to_eta2(f,
                out$df,
                df_residuals,
                ci = ci, alternative = alternative,
                verbose = verbose
      )[-1]

    out[c("CI", "CI_low", "CI_high")] <- CI_tab[c("CI", "CI_low", "CI_high")]
  } else {
    alternative <- NULL
  }


  # Clean up output ---
  out <- out[, colnames(out) %in% c(
    "Group",
    "Parameter",
    "Eta2", "Eta2_generalized", "Eta2_partial",
    "Omega2", "Omega2_partial",
    "Epsilon2", "Epsilon2_partial",
    if (!is.null(ci)) c("CI", "CI_low", "CI_high")
  ), drop = FALSE]
  rownames(out) <- NULL

  attr(out, "partial") <- partial
  attr(out, "generalized") <- generalized
  attr(out, "ci") <- ci
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  out
}

#' @rdname effectsize_API
#' @export
.es_aov_table <- function(aov_table,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          generalized = FALSE,
                          ci = 0.95, alternative = "greater",
                          verbose = TRUE,
                          include_intercept = FALSE) {
  aov_table <- as.data.frame(aov_table)

  # Get correct function ---
  type <- match.arg(type)
  es_fun <- switch(type,
                   eta = F_to_eta2,
                   omega = F_to_omega2,
                   epsilon = F_to_epsilon2)


  # Non-Partial / Generalized -> BAD ---
  if (verbose) {
    if (!isTRUE(partial))
      warning(
        "Currently only supports partial ",
        type,
        " squared for this class of objects.",
        call. = FALSE
      )

    if (isTRUE(generalized) || is.character(generalized))
      warning(
        "generalized ", type, " squared ",
        "is not supported for this class of object.",
        call. = FALSE
      )
  }


  # Turn ts to Fs (if needed) ---
  if (!"F" %in% colnames(aov_table))
    if ("t" %in% colnames(aov_table)) {
      aov_table[["F"]] <- aov_table[["t"]]^2
      aov_table[["df"]] <- 1
    } else {
      stop(insight::format_message("ANOVA table does not have F values - cannot compute effect size."))
    }


  # include_intercept? ---
  if (!include_intercept)
    aov_table <- aov_table[aov_table$Parameter != "(Intercept)", , drop = FALSE]

  ES_tab <- es_fun(aov_table[["F"]],
                   aov_table[["df"]],
                   aov_table[["df_error"]],
                   ci = ci, alternative = alternative,
                   verbose = verbose)

  out <- cbind(Parameter = aov_table[["Parameter"]], ES_tab)
  rownames(out) <- NULL

  # Set attributes ---
  attr(out, "partial") <- TRUE
  attr(out, "generalized") <- FALSE
  attr(out, "ci") <- if ("CI" %in% colnames(out)) ci
  attr(out, "alternative") <- if (!is.null(attr(out, "ci"))) alternative
  attr(out, "anova_type") <- NULL
  attr(out, "approximate") <- NULL
  out
}

# Clean up wrappers -------------------------------------------------------



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

  params <- parameters::model_parameters(model, verbose = verbose, effects = "fixed")
  out <- .es_aov_simple(as.data.frame(params), type, partial, generalized, ci, alternative, verbose = verbose, ...)
  if (is.null(attr(out, "anova_type"))) attr(out, "anova_type") <- attr(params, "anova_type")
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

  DV_names <- insight::find_predictors(model)[[1]]

  out <-
    .es_aov_strata(
      params,
      DV_names = DV_names,
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

  params <- split(params, factor(params$Response, levels = unique(params$Response))) # make sure row order is not changed
  params <- lapply(params, .es_aov_simple,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci, alternative = alternative,
    verbose = verbose,
    ...
  )

  params <- lapply(names(params), function(nm) {
    cbind(Response = nm, params[[nm]])
  })
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
  F.nm <- c("F value", "approx F", "F-value", "F")
  df.nm <- c("NumDF", "num Df", "numDF", "npar", "Df")
  df_error.nm <- c("DenDF", "den Df", "denDF", "df_error", "Df.res")

  # If there is no df_error *or* is there IS a residuals row...
  if (!any(df_error.nm %in% colnames(model))) {
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

  if (!any(F.nm %in% colnames(model)) || !any(df.nm %in% colnames(model))) {
    stop(insight::format_message("ANOVA table does not have F values or degrees of freedom - cannot compute effect size."))
  }

  Fi <- F.nm[F.nm %in% colnames(model)]
  dfi <- df.nm[df.nm %in% colnames(model)]
  df_errori <- df_error.nm[df_error.nm %in% colnames(model)]

  if (length(dfi) > 1L) {
    dfi <- dfi[1] # For MANOVA this should not use the MV-df
  }

  # Clean up table ---
  par_table <- data.frame(
    Parameter = rownames(model),
    F = model[,Fi],
    df = model[,dfi],
    df_error = model[,df_errori]
  )
  par_table <- par_table[!par_table[["Parameter"]] %in% "Residuals",]

  out <-
    .es_aov_table(
      par_table,
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci,
      alternative = alternative,
      verbose = verbose,
      include_intercept = include_intercept
    )

  attr(out, "anova_type") <- tryCatch(attr(parameters::model_parameters(model, verbose = FALSE, effects = "fixed"), "anova_type"),
                                      error = function(...) 1)
  attr(out, "approximate") <- TRUE
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
                                       by_response = TRUE,
                                       ...) {
  if (by_response && "Response" %in% colnames(model)) {
    out <- split(model, model[["Response"]])
    out <- lapply(out, .anova_es.parameters_model,
                  type = type, partial = partial, generalized = generalized,
                  ci = ci, alternative = alternative,
                  verbose = verbose,
                  by_response = FALSE,
                  ...)
    saved_attr <- attributes(out[[1]])
    out <- mapply(out, names(out),
                  FUN = function(x, nm) cbind(Response = nm, x),
                  SIMPLIFY = FALSE)
    out <- do.call(rbind, out)

    # Set attributes ---
    attr(out, "partial") <- saved_attr$partial
    attr(out, "generalized") <- saved_attr$generalized
    attr(out, "ci") <- saved_attr$ci
    attr(out, "alternative") <- saved_attr$alternative
    attr(out, "anova_type") <- attr(model, "anova_type")
    attr(out, "approximate") <- saved_attr$approximate
    return(out)
  }


  approximate <- FALSE
  if ("Sum_Squares" %in% colnames(model) && "Residuals" %in% model[["Parameter"]]) {
    if ("Group" %in% colnames(model)) {
      DVs <- unlist(insight::find_predictors(.get_object(model)))
      out <- .es_aov_strata(
        model, DV_names = DVs,
        type = type, partial = partial, generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose, ...
      )
    } else {
      out <- .es_aov_simple(
        model,
        type = type, partial = partial, generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose, ...
      )
    }
  } else {
    out <- .es_aov_table(
      model,
      type = type, partial = partial, generalized = generalized,
      ci = ci, alternative = alternative,
      verbose = verbose, ...
    )
    approximate <- TRUE
  }
  attr(out, "anova_type") <- attr(model, "anova_type")
  attr(out, "approximate") <- approximate
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
.anova_es.Anova.mlm <-
  function(model,
           type = c("eta", "omega", "epsilon"),
           partial = TRUE,
           generalized = FALSE,
           ci = 0.95, alternative = "greater",
           verbose = TRUE,
           include_intercept = FALSE,
           ...) {
    # Faking the model_parameters.aovlist output:
    suppressWarnings(aov_tab <- summary(model)$univariate.tests)
    if (is.null(aov_tab)) {
      aov_tab <- parameters::model_parameters(model)
      aov_tab$df <- aov_tab$df_num
      aov_tab$df_num <- NULL
      out <- .anova_es(aov_tab, type = type,
                       partial = partial, generalized = generalized,
                       ci = ci, alternative = alternative,
                       include_intercept = include_intercept,
                       verbose = verbose)
      attr(out, "anova_type") <- as.numeric(as.roman(model$type))
      attr(out, "approximate") <- FALSE
      return(out)
    }
    aov_tab <- as.data.frame(unclass(aov_tab))
    aov_tab$Parameter <- rownames(aov_tab)
    colnames(aov_tab)[colnames(aov_tab)== "Sum Sq"] <- "Sum_Squares"
    colnames(aov_tab)[colnames(aov_tab)== "num Df"] <- "df"
    aov_tab <- aov_tab[c("Parameter", "Sum_Squares","Error SS", "df", "den Df")]

    id <- "Subject"
    within <- names(model$idata)
    within <- lapply(within, function(x) c(NA, x))
    within <- do.call(expand.grid, within)
    within <- apply(within, 1, na.omit)
    ns <- sapply(within, length)
    within <- sapply(within, paste, collapse = ":")
    within <- within[order(ns)]
    within <- Filter(function(x) nchar(x) > 0, within)
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

    DV_names <- c(id, setdiff(unlist(strsplit(model$terms, ":")), "(Intercept)"))

    out <-
      .es_aov_strata(
        aov_tab,
        DV_names = DV_names,
        type = type,
        partial = partial,
        generalized = generalized,
        ci = ci, alternative = alternative,
        verbose = verbose,
        include_intercept = include_intercept
      )
    out$Group <- NULL

    # Reorder rows
    orig_terms <- model$terms
    if (include_intercept && !"(Intercept)" %in% orig_terms) {
      orig_terms <- c("(Intercept)", orig_terms)
    } else if (!include_intercept && "(Intercept)" %in% orig_terms) {
      orig_terms <- setdiff(orig_terms, "(Intercept)")
    }
    out <- out[match(out$Parameter, orig_terms),]

    attr(out, "anova_type") <- as.numeric(as.roman(model$type))
    attr(out, "approximate") <- FALSE
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
  out <-
    .anova_es.anova(
      model,
      type = type,
      partial = partial,
      generalized = generalized,
      ci = ci,
      alternative = alternative,
      ...
    )
  attr(out, "approximate") <- TRUE
  out
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
  attr(out, "approximate") <- TRUE
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

  attr(out, "anova_type") <- attr(model, "type", exact = TRUE)
  attr(out, "approximate") <- FALSE
  out
}

#' @keywords internal
.anova_es.mixed <- function(model,
                            verbose = TRUE,
                            include_intercept = FALSE,
                            ...) {
  aov_tab <- as.data.frame(model[["anova_table"]])

  if (!"F" %in% colnames(aov_tab)) {
    stop("Cannot estimate approx effect size for `mixed` type model - no F-statistic found.", call. = FALSE)
  }

  if (verbose && include_intercept) {
    warning("Cannot estimate (Intercept) effect size for `mixed` model.", call. = FALSE)
  }

  aov_tab$Parameter <- rownames(aov_tab)
  aov_tab$df <- aov_tab[["num Df"]]
  aov_tab$df_error <- aov_tab[["den Df"]]
  aov_tab <- aov_tab[, c("Parameter", "df", "df_error", "F")]

  out <- .es_aov_table(aov_tab, verbose = verbose, ...)

  attr(out, "anova_type") <- attr(model, "type")
  attr(out, "approximate") <- TRUE
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
  model$Parameter <- i
  colnames(model) <- gsub("d.f.", "df", colnames(model), fixed = TRUE)
  model$df_error <- model$df[rownames(model) == "ERROR"]
  model <- model[rownames(model) != "ERROR", ]

  out <- .es_aov_table(
    model,
    type = type,
    partial = partial,
    generalized = generalized,
    ci = ci,
    alternative = alternative,
    ...
  )
  attr(out, "anova_type") <- 2
  attr(out, "approximate") <- FALSE
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