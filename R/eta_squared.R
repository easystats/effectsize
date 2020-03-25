#' Effect size for ANOVA
#'
#' Functions to compute effect size measures for ANOVAs, such as Eta, Omega and Epsilon squared
#' (or their partialled versions), representing an estimate of how much variance in the response
#' variables are accounted for by the explanatory variables.
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
#' For between-subjects ANOVAs, the effect sizes are computed directly with Sums-of-Squares.
#' For all other cases, effect sizes are approsimated via test statistic conversion
#' (see \code{\link{F_to_eta2} for more details.})
#'
#' \subsection{Confidence Intervals}{
#' Confidence intervals are estimated using the Noncentrality parameter method;
#' These methods searches for a the best \code{ncp} (non-central parameters) for
#' of the noncentral F distribution for the desired tail-probabilities,
#' and then convert these \code{ncp}s to the corresponding effect sizes.
#' }
#'
#' \subsection{Omega Squared}{
#' Omega squared is considered as a lesser biased alternative to eta-squared, especially when sample sizes are small (Albers \& Lakens, 2018). Field (2013) suggests the following interpretation heuristics:
#' \itemize{
#'   \item Omega Squared = 0 - 0.01: Very small
#'   \item Omega Squared = 0.01 - 0.06: Small
#'   \item Omega Squared = 0.06 - 0.14: Medium
#'   \item Omega Squared > 0.14: Large
#' }
#'
#' } \subsection{Epsilon Squared}{
#' It is one of the least common measures of effect sizes: omega squared and eta squared are used more frequently. Although having a different name and a formula in appearance different, this index is equivalent to the adjusted R2 (Allen, 2017, p. 382).
#'
#' } \subsection{Cohen's f}{
#' Cohen's f statistic is one appropriate effect size index to use for a oneway analysis of variance (ANOVA). Cohen's f can take on values between zero, when the population means are all equal, and an indefinitely large number as standard deviation of means increases relative to the average standard deviation within each group. Cohen has suggested that the values of 0.10, 0.25, and 0.40 represent small, medium, and large effect sizes, respectively.
#' }
#'
#' @examples
#' library(effectsize)
#'
#' df <- iris
#' df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")
#'
#' model <- aov(Sepal.Length ~ Sepal.Big, data = df)
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#'
#' model <- anova(lm(Sepal.Length ~ Sepal.Big * Species, data = df))
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#'
#' model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)
#' omega_squared(model)
#' eta_squared(model)
#' epsilon_squared(model)
#' cohens_f(model)
#'
#' @return Data.frame containing the effect size values.
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
#' @importFrom parameters model_parameters
#' @export
eta_squared <- function(model, partial = TRUE, ci = 0.9, ...) {
  UseMethod("eta_squared")
}


#' @importFrom stats anova
#' @export
eta_squared.aov <- function(model, partial = TRUE, ci = 0.9, ...) {

  if (!inherits(model, c("Gam", "aov", "anova", "anova.rms"))) {
    # Pass to ANOVA table method
    res <- eta_squared.anova(
      stats::anova(model),
      partial = partial,
      ci = ci
    )
    return(res)
  }

  params <- as.data.frame(parameters::model_parameters(model))
  if (!"Residuals" %in% params$Parameter) {
    stop("No residuals data found. Eta squared can only be computed for simple `aov` models.")
  }

  values <- .values_aov(params)
  if (isFALSE(partial)) {
    params$Eta_Sq <- params$Sum_Squares / values$Sum_Squares_total
    params[params$Parameter == "Residuals", "Eta_Sq"] <- NA
  } else {
    params$Eta_Sq_partial <-
      params$Sum_Squares / (params$Sum_Squares + values$Sum_Squares_residuals)
    params[params$Parameter == "Residuals", "Eta_Sq_partial"] <- NA
  }

  out <- params[params$Parameter != "Residuals",
                intersect(c("Group", "Parameter", "Eta_Sq", "Eta_Sq_partial"),
                          names(params)),
                drop = FALSE]

  if (is.numeric(ci)) {
    df_error <- params$df[params$Parameter == "Residuals"]
    params <- params[params$Parameter!="Residuals", , drop = FALSE]

    if (isTRUE(partial)) {
      # use NCP
      eta_ci <- F_to_eta2(
        f = params$`F`,
        df = params$df,
        df_error = df_error,
        ci = ci
      )

      eta_ci$Eta_Sq_partial <- NULL

      out <- cbind(out, eta_ci)
    } else {
      # Make an F value that is just the effect compared to everything else
      SSE <- values$Sum_Squares_total - params$Sum_Squares
      dfE <- values$n - params$df - 1
      MSE <- SSE / dfE

      eta_ci <- F_to_eta2(
        f = params$Mean_Square / MSE,
        df = params$df[params$Parameter!="Residuals"],
        df_error = dfE,
        ci = ci
      )

      eta_ci$Eta_Sq_partial <- NULL

      out <- cbind(out, eta_ci)
    }
  }

  class(out) <- c(ifelse(isTRUE(partial), "partial_eta_squared", "eta_squared"),
                  "effectsize_table",
                  class(out))
  out
}


#' @export
eta_squared.lm <- eta_squared.aov

#' @export
eta_squared.glm <- eta_squared.aov


#' @export
eta_squared.anova <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!"DenDF" %in% colnames(model)) {
    # Pass to AOV method
    res <- eta_squared.aov(model,
                           partial = partial,
                           ci = ci)
    return(res)
  }

  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for mixed models.", call. = FALSE)
  }

  par_table <- as.data.frame(model)

  out <- cbind(
    Parameter = rownames(par_table),
    F_to_eta2(par_table$`F value`,
              par_table$NumDF,
              par_table$DenDF,
              ci = ci
    )
  )

  class(out) <- c("partial_eta_squared", "effectsize_table", class(out))
  out
}



#' @export
eta_squared.aovlist <- function(model, partial = TRUE, ci = 0.9, ...) {

  if (isFALSE(partial)) {
    warning("Currently only supports partial eta squared for repeated-measures ANOVAs.", call. = FALSE)
  }

  par_table <- as.data.frame(parameters::model_parameters(model))
  par_table <- split(par_table, par_table$Group)
  par_table <- lapply(par_table, function(.data) {
    .data$df_error <- .data$df[.data$Parameter == "Residuals"]
    .data
  })
  par_table <- do.call(rbind, par_table)

  par_table <- par_table[par_table$Parameter!="Residuals", , drop = FALSE]


  out <- cbind(
    Parameter = par_table$Parameter,
    F_to_eta2(par_table$`F`,
              par_table$df,
              par_table$df_error,
              ci = ci)
  )

  class(out) <- c("partial_eta_squared", "effectsize_table", class(out))
  out
}


#' @export
eta_squared.merMod <- function(model, partial = TRUE, ci = 0.9, ...) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package 'lmerTest' required for this function to work. Please install it by running `install.packages('lmerTest')`.")
  }

  model <- lmerTest::as_lmerModLmerTest(model)
  model <- stats::anova(model)
  eta_squared.anova(model, partial = partial, ci = ci, ...)
}

#' @rdname eta_squared
#' @export
cohens_f <- function(model, partial = TRUE, ci = 0.9, ...) {
  res <- eta_squared(model,
                     partial = partial,
                     ci = ci)

  if (partial) {
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
