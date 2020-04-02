#' Effect size for ANOVA
#'
#' Functions to compute effect size measures for ANOVAs, such as Eta, Omega and Epsilon squared,
#' and Cohen's f (or their partialled versions) for \code{aov}, \code{aovlist}, \code{anova},
#' and \code{merMod} models. These indices represent an estimate of how much variance in
#' the response variables is accounted for by the explanatory variable(s).
#' \cr\cr
#' Effect sizes are based on \strong{Type-1} sums of squares for \code{aov} and \code{aovlist}
#' models, and \strong{Type-3} for \code{merMod} models. For \code{anova} tables, effect sizes
#' are computed based on the table's sums of squares (whatever those may be). It is generally
#' recommended to fit models with \emph{\code{contr.sum} factor weights} and
#' \emph{centered covariates}, for sensible results. (\strong{\emph{Yeah... ANOVAs are hard...}})
#'  See examples.
#'
#' @param model An model or ANOVA object.
#' @param partial If \code{TRUE}, return partial indices.
#' @inheritParams chisq_to_phi
#' @param ... Arguments passed to or from other methods (ignored).
#'
#' @return A data frame with the effect size(s) and confidence interval(s).
#'
#' @details
#'
#' For \code{aov} and \code{aovlist} models, the effect sizes are computed directly with
#' Sums-of-Squares. For all other cases, effect sizes are approximated via test statistic
#' conversion (see \code{\link{F_to_eta2} for more details.})
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' \subsection{Omega Squared}{
#' Omega squared is considered as a lesser biased alternative to eta-squared, especially
#' when sample sizes are small (Albers \& Lakens, 2018). Field (2013) suggests the following
#' interpretation heuristics:
#' \itemize{
#'   \item Omega Squared = 0 - 0.01: Very small
#'   \item Omega Squared = 0.01 - 0.06: Small
#'   \item Omega Squared = 0.06 - 0.14: Medium
#'   \item Omega Squared > 0.14: Large
#' }
#'
#' } \subsection{Epsilon Squared}{
#' It is one of the least common measures of effect sizes: omega squared and eta squared are
#' used more frequently. Although having a different name and a formula in appearance
#' different, this index is equivalent to the adjusted R2 (Allen, 2017, p. 382).
#'
#' } \subsection{Cohen's f}{
#' Cohen's f statistic is one appropriate effect size index to use for a oneway analysis
#'  of variance (ANOVA). Cohen's f can take on values between zero, when the population
#'  means are all equal, and an indefinitely large number as standard deviation of means
#'  increases relative to the average standard deviation within each group. Cohen has
#'  suggested that the values of 0.10, 0.25, and 0.40 represent small, medium, and large
#'  effect sizes, respectively.
#' }
#'
#' @seealso \code{\link{F_to_eta2}}
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
#' eta_squared(model, partial = FALSE)
#' omega_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
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
#'
#'   model_anova <- car::Anova(model, type = 3)
#'
#'   eta_squared(model_anova)
#' }
#'
#'
#' if (require(lmerTest, quietly = TRUE)) {
#'   model <- lmer(mpg ~ am_f * cyl_f + (1|vs), data = mtcars)
#'
#'   omega_squared(model)
#' }
#' }
#'
#' @return A data frame containing the effect size values and their confidence intervals.
#'
#'
#' @references \itemize{
#'  \item Albers, C., \& Lakens, D. (2018). When power analyses based on pilot data are biased: Inaccurate effect size estimators and follow-up bias. Journal of experimental social psychology, 74, 187-195.
#'  \item Allen, R. (2017). Statistics and Experimental Design for Psychologists: A Model Comparison Approach. World Scientific Publishing Company.
#'  \item Field, A. (2013). Discovering statistics using IBM SPSS statistics. sage.
#'  \item Kelley, K. (2007). Methods for the behavioral, educational, and social sciences: An R package. Behavior Research Methods, 39(4), 979-984.
#'  \item Kelley, T. (1935) An unbiased correlation ratio measure. Proceedings of the National Academy of Sciences. 21(9). 554-559.
#' }
#'
#' The computation of CIs is based on the implementation done by Stanley (2018) in the \code{ApaTables} package and Kelley (2007) in the \code{MBESS} package. All credits go to them.
#'
#' @export
eta_squared <- function(model,
                        partial = TRUE,
                        ci = 0.9,
                        ...) {
  .anova_es(model, type = "eta", partial = partial, ci = ci)
}

#' @rdname eta_squared
#' @export
omega_squared <- function(model,
                          partial = TRUE,
                          ci = 0.9,
                          ...) {
  .anova_es(model, type = "omega", partial = partial, ci = ci)
}

#' @rdname eta_squared
#' @export
epsilon_squared <- function(model,
                            partial = TRUE,
                            ci = 0.9,
                            ...) {
  .anova_es(model, type = "epsilon", partial = partial, ci = ci)
}

#' @rdname eta_squared
#' @export
cohens_f <- function(model, partial = TRUE, ci = 0.9, ...) {
  res <- eta_squared(model,
                     partial = partial,
                     ci = ci)

  if ("Eta_Sq_partial" %in% colnames(res)) {
    res$Eta_Sq_partial <- sqrt(res$Eta_Sq_partial / (1 - res$Eta_Sq_partial))
    colnames(res)[colnames(res) == "Eta_Sq_partial"] <- "Cohens_f_partial"
  } else {
    res$Eta_Sq <- sqrt(res$Eta_Sq / (1 - res$Eta_Sq))
    colnames(res)[colnames(res) == "Eta_Sq"] <- "Cohens_f"
  }

  if (is.numeric(ci)) {
    res$CI_low <- sqrt(res$CI_low  / (1 - res$CI_low))
    res$CI_high <- sqrt(res$CI_high  / (1 - res$CI_high))
  }

  res
}


# Get ES ------------------------------------------------------------------

#' @keywords internal
.anova_es <-
  function(model,
           type = c("eta", "omega", "epsilon"),
           partial = TRUE,
           ci = 0.9,
           ...) {
    UseMethod(".anova_es")
  }

#' @keywords internal
#' @importFrom parameters model_parameters
#' @importFrom stats anova
.anova_es.aov <- function(model,
                          type = c("eta", "omega", "epsilon"),
                          partial = TRUE,
                          ci = 0.9,
                          ...) {
  type <- match.arg(type)
  es_fun <- switch (type,
                    eta = F_to_eta2,
                    omega = F_to_omega2,
                    epsilon = F_to_epsilon2)

  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) {
    # Pass to ANOVA table method
    res <- .anova_es.anova(
      stats::anova(model),
      type = type,
      partial = partial,
      ci = ci
    )
    return(res)
  }

  params <- as.data.frame(parameters::model_parameters(model))
  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found - ",
         type,
         " squared can only be computed for simple `aov` models.")
  }

  values <- .values_aov(params)
  if (type == "eta") {
    if (isFALSE(partial)) {
      params$Eta_Sq <- params$Sum_Squares / values$Sum_Squares_total
      params[params$Parameter == "Residuals", "Eta_Sq"] <- NA
    } else {
      params$Eta_Sq_partial <-
        params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
      params[params$Parameter == "Residuals", "Eta_Sq_partial"] <-
        NA
    }
  } else if (type == "omega") {
    if (isFALSE(partial)) {
      params$Omega_Sq <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) / (values$Sum_Squares_total + values$Mean_Square_residuals)
      params[params$Parameter == "Residuals", "Omega_Sq"] <- NA
    } else {
      params$Omega_Sq_partial <-
        (params$df * (params$Mean_Square - values$Mean_Square_residuals)) / (
          params$df * params$Mean_Square + (values$n - params$df) * values$Mean_Square_residuals
        )
      params[params$Parameter == "Residuals", "Omega_Sq_partial"] <-
        NA
    }
  } else if (type == "epsilon") {
    if (isFALSE(partial)) {
      params$Epsilon_Sq <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
        values$Sum_Squares_total
      params[params$Parameter == "Residuals", "Epsilon_sq"] <- NA
    } else {
      params$Epsilon_Sq_partial <-
        (params$Sum_Squares - params$df * values$Mean_Square_residuals) /
        (params$Sum_Squares + values$Sum_Squares_residuals)
      params[params$Parameter == "Residuals", "Epsilon_sq_partial"] <-
        NA
    }
  }


  out <- params[params$Parameter != "Residuals",
                colnames(params) %in% c("Group", "Parameter",
                                        "Eta_Sq", "Eta_Sq_partial",
                                        "Omega_Sq", "Omega_Sq_partial",
                                        "Epsilon_Sq", "Epsilon_Sq_partial"),
                drop = FALSE]

  if (is.numeric(ci)) {
    df_error <- params$df[params$Parameter == "Residuals"]
    params <- params[params$Parameter != "Residuals", , drop = FALSE]

    if (isTRUE(partial)) {
      # use NCP
      eta_ci <- es_fun(
        f = params$`F`,
        df = params$df,
        df_error = df_error,
        ci = ci
      )
    } else {
      # Make an F value that is just the effect compared to everything else
      SSE <- values$Sum_Squares_total - params$Sum_Squares
      dfE <- values$n - params$df - 1
      MSE <- SSE / dfE

      eta_ci <- es_fun(
        f = params$Mean_Square / MSE,
        df = params$df,
        df_error = dfE,
        ci = ci
      )
    }
    eta_ci[[1]] <- NULL
    out <- cbind(out, eta_ci)
  }

  class(out) <- unique(c("effectsize_table", class(out)))
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
                            ci = 0.9,
                            ...) {
  type <- match.arg(type)
  es_fun <- switch (type,
                    eta = F_to_eta2,
                    omega = F_to_omega2,
                    epsilon = F_to_epsilon2)
  model <- model[rownames(model) != "(Intercept)", ]

  if (!"DenDF" %in% colnames(model)) {
    # Pass to AOV method
    res <- .anova_es.aov(model,
                         partial = partial,
                         type = type,
                         ci = ci)
    return(res)
  }

  if (isFALSE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for repeated-measures ANOVAs.",
      call. = FALSE
    )
  }

  par_table <- as.data.frame(model)

  out <- cbind(
    Parameter = rownames(par_table),
    es_fun(par_table$`F value`,
           par_table$NumDF,
           par_table$DenDF,
           ci = ci)
  )

  class(out) <- unique(c("effectsize_table", class(out)))
  out
}

#' @keywords internal
#' @importFrom parameters model_parameters
.anova_es.aovlist <- function(model,
                              type = c("eta", "omega", "epsilon"),
                              partial = TRUE,
                              ci = 0.9,
                              ...) {
  type <- match.arg(type)
  es_fun <- switch (type,
                    eta = F_to_eta2,
                    omega = F_to_omega2,
                    epsilon = F_to_epsilon2)


  if (isFALSE(partial)) {
    warning(
      "Currently only supports partial ",
      type,
      " squared for repeated-measures ANOVAs.",
      call. = FALSE
    )
  }

  par_table <- as.data.frame(parameters::model_parameters(model))
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    if (any(.data$Parameter == "Residuals")) {
      .data$df_error <- .data$df[.data$Parameter == "Residuals"]
    } else {
      .data$df_error <- NA
    }
    .data
  })
  par_table <- do.call(rbind, par_table)

  par_table <-
    par_table[par_table$Parameter != "Residuals" &
                !is.na(par_table$`F`), , drop = FALSE]


  out <- cbind(par_table,
               es_fun(par_table$`F`,
                      par_table$df,
                      par_table$df_error,
                      ci = ci))
  out <- out[, colnames(out) %in% c(
    "Group",
    "Parameter",
    "Eta_Sq_partial",
    "Omega_Sq_partial",
    "Epsilon_Sq_partial",
    "CI",
    "CI_low",
    "CI_high"
  ), drop = FALSE]
  rownames(out) <- NULL

  class(out) <- unique(c("effectsize_table", class(out)))
  out
}

#' @keywords internal
#' @importFrom stats anova
.anova_es.merMod <- function(model,
                             type = c("eta", "omega", "epsilon"),
                             partial = TRUE,
                             ci = 0.9,
                             ...) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. ",
         "Please install it by running `install.packages('lmerTest')`.")
  }

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  .anova_es.anova(model, type = type, partial = partial, ci = ci, ...)
}


### Requires more work to get the type 3 working...
### Last worked on Mar 29, 2020 by MSB
#' #' @keywords internal
#' .anova_es.afex_aov <- function(model,
#'                                type = c("eta", "omega", "epsilon"),
#'                                partial = TRUE,
#'                                ci = 0.9,
#'                                ...) {
#'   if (!is.null(model$aov)) {
#'     out <- .anova_es(model$aov, type = type, partial = partial, ci = ci)
#'     return(out)
#'   } else if (length(attr(model, "within")) == 0) {
#'     out <- .anova_es(model$lm, type = type, partial = partial, ci = ci)
#'     return(out)
#'   }
#'
#'   type <- match.arg(type)
#'   es_fun <- switch (type,
#'                     eta = F_to_eta2,
#'                     omega = F_to_omega2,
#'                     epsilon = F_to_epsilon2)
#'
#'   if (isFALSE(partial)) {
#'     # not really true
#'     warning(
#'       "Currently only supports partial ",
#'       type,
#'       " squared for repeated-measures ANOVAs.",
#'       call. = FALSE
#'     )
#'   }
#'
#'   anova_table <- fit$Anova
#'   anova_table <- suppressWarnings(summary(anova_table)$univariate.tests)
#'   anova_table <- as.data.frame(unclass(anova_table))
#'
#'   out <- cbind(
#'     Parameter = rownames(anova_table),
#'     es_fun(
#'       anova_table$`F value`,
#'       anova_table$`num Df`,
#'       anova_table$`den Df`,
#'       ci = ci
#'     )
#'   )
#'
#'   out <- out[out$Parameter != "(Intercept)", , drop = FALSE]
#'   class(out) <- unique(c("effectsize_table", class(out)))
#'   return(out)
#' }