#' Effect Sizes
#'
#' This function tries to return the best effect-size measure for the provided
#' input model. See details.
#'
#' @param model An object of class `htest`, or a statistical model. See details.
#' @param type The effect size of interest. See details.
#' @param ... Arguments passed to or from other methods. See details.
#' @inheritParams datawizard::standardize.default
#'
#' @details
#'
#' - For an object of class `htest`, data is extracted via [insight::get_data()], and passed to the relevant function according to:
#'   - A **t-test** depending on `type`: `"cohens_d"` (default), `"hedges_g"`, or one of `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`, `"overlap"`.
#'   - A **Chi-squared tests of independence**, depending on `type`: `"cramers_v"` (default), `"phi"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`, or `"riskratio"`.
#'   - A **Chi-squared tests of goodness-of-fit**, depending on `type`: `"fei"` (default) `"cohens_w"`, `"pearsons_c"`
#'   - A **One-way ANOVA test**, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#'   - A **McNemar test** returns *Cohen's g*.
#'   - A **Wilcoxon test** depending on `type`: returns "`rank_biserial`" correlation (default) or one of `"p_superiority"`, `"vda"`, `"u2"`, `"u3"`, `"overlap"`.
#'   - A **Kruskal-Wallis test** depending on `type`: `"epsilon"` (default) or `"eta"`.
#'   - A **Friedman test** returns *Kendall's W*.
#'   (Where applicable, `ci` and `alternative` are taken from the `htest` if not otherwise provided.)
#' - For an object of class `BFBayesFactor`, using [bayestestR::describe_posterior()],
#'   - A **t-test** depending on `type`: `"cohens_d"` (default) or one of `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`, `"overlap"`.
#'   - A **correlation test** returns *r*.
#'   - A **contingency table test**, depending on `type`: `"cramers_v"` (default), `"phi"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`, or `"riskratio"`.
#'   - A **proportion test** returns *p*.
#' - Objects of class `anova`, `aov`, or `aovlist`, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#' - Other objects are passed to [parameters::standardize_parameters()].
#'
#' **For statistical models it is recommended to directly use the listed
#' functions, for the full range of options they provide.**
#'
#' @return A data frame with the effect size (depending on input) and and its
#'   CIs (`CI_low` and `CI_high`).
#'
#' @family effect size indices
#'
#' @examples
#'
#' ## Hypothesis Testing
#' ## ------------------
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#' Xsq <- chisq.test(contingency_table)
#' effectsize(Xsq)
#' effectsize(Xsq, type = "cohens_w")
#'
#' Tt <- t.test(1:10, y = c(7:20), alternative = "less")
#' effectsize(Tt)
#'
#' Aov <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)
#' effectsize(Aov)
#' effectsize(Aov, type = "omega")
#'
#' Wt <- wilcox.test(1:10, 7:20, mu = -3, alternative = "less")
#' effectsize(Wt)
#' effectsize(Wt, type = "u2")
#'
#' ## Bayesian Hypothesis Testing
#' ## ---------------------------
#' \donttest{
#' if (require(BayesFactor)) {
#'   bf_prop <- proportionBF(3, 7, p = 0.3)
#'   effectsize(bf_prop)
#'
#'   bf_corr <- correlationBF(attitude$rating, attitude$complaints)
#'   effectsize(bf_corr)
#'
#'   data(raceDolls)
#'   bf_xtab <- contingencyTableBF(raceDolls, sampleType = "poisson", fixedMargin = "cols")
#'   effectsize(bf_xtab)
#'   effectsize(bf_xtab, type = "oddsratio")
#'
#'   bf_ttest <- ttestBF(sleep$extra[sleep$group == 1],
#'     sleep$extra[sleep$group == 2],
#'     paired = TRUE, mu = -1
#'   )
#'   effectsize(bf_ttest)
#' }
#' }
#'
#' ## Models and Anova Tables
#' ## -----------------------
#' fit <- lm(mpg ~ factor(cyl) * wt + hp, data = mtcars)
#' effectsize(fit)
#'
#' anova_table <- anova(fit)
#' effectsize(anova_table)
#' effectsize(anova_table, type = "epsilon")
#' @export
effectsize <- function(model, ...) {
  UseMethod("effectsize")
}

#' @export
effectsize.anova <- function(model, type = NULL, ...) {
  if (is.null(type)) type <- "eta"

  f <- switch(tolower(type),
    eta = ,
    eta2 = ,
    eta_squared = eta_squared,
    epsilon = ,
    epsilon2 = ,
    epsilon_squared = epsilon_squared,
    omega = ,
    omega2 = ,
    omega_squared = omega_squared,
    f = ,
    cohens_f = cohens_f,
    f2 = ,
    f_squared = ,
    cohens_f2 = cohens_f_squared
  )

  f(model, ...)
}

#' @export
effectsize.afex_aov <- effectsize.anova

#' @export
#' @rdname effectsize
effectsize.aov <- effectsize.anova

#' @export
effectsize.aovlist <- effectsize.anova


#' @export
effectsize.easycorrelation <- function(model, ...) {
  if (is.null(r_name <- attr(model, "coefficient_name"))) {
    r_name <- "r"
  }

  r_cols <- 1:which(colnames(model) == r_name)
  if (!is.null(attr(model, "ci"))) {
    model$CI <- attr(model, "ci")
    CI_cols <- c("CI", "CI_low", "CI_high")
    CI_cols <- sapply(CI_cols, function(ici) which(colnames(model) == ici))
    r_cols <- c(r_cols, CI_cols)
  }

  out <- model[, r_cols, drop = FALSE]
  class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
  attr(out, "approximate") <- FALSE
  out
}


#' @export
effectsize.default <- function(model, ...) {
  # message("Using standardize_parameters().")
  parameters::standardize_parameters(model, ...)
}
