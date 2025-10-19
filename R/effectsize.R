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
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @details
#'
#' - For an object of class `htest`, data is extracted via [insight::get_data()], and passed to the relevant function according to:
#'   - A **t-test** depending on `type`: `"cohens_d"` (default), `"hedges_g"`, or one of `"p_superiority"`, `"u1"`, `"u2"`, `"u3"`, `"overlap"`.
#'     - For a **Paired t-test**: depending on `type`: `"rm_rm"`, `"rm_av"`, `"rm_b"`, `"rm_d"`, `"rm_z"`.
#'   - A **Chi-squared tests of independence** or **Fisher's Exact Test**, depending on `type`: `"cramers_v"` (default), `"tschuprows_t"`, `"phi"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`, `"riskratio"`, `"arr"`, or `"nnt"`.
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
#'   - A **contingency table test**, depending on `type`: `"cramers_v"` (default), `"phi"`, `"tschuprows_t"`, `"cohens_w"`, `"pearsons_c"`, `"cohens_h"`, `"oddsratio"`, or `"riskratio"`, `"arr"`, or `"nnt"`.
#'   - A **proportion test** returns *p*.
#' - Objects of class `anova`, `aov`, `aovlist` or `afex_aov`, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#' - Objects of class `datawizard_crosstab(s)` / `datawizard_table(s)` built with [datawizard::data_tabulate()] - same as Chi-squared tests of independence / goodness-of-fit, respectively.
#' - Other objects are passed to [parameters::standardize_parameters()].
#'
#' **For statistical models it is recommended to directly use the listed
#' functions, for the full range of options they provide.**
#'
#' @return A data frame with the effect size (depending on input) and and its
#'   CIs (`CI_low` and `CI_high`).
#'
#' @seealso `vignette(package = "effectsize")`
#'
#' @examples
#'
#' ## Hypothesis Testing
#' ## ------------------
#' data("Music_preferences")
#' Xsq <- chisq.test(Music_preferences)
#' effectsize(Xsq)
#' effectsize(Xsq, type = "cohens_w")
#'
#' # Or:
#' data("mtcars")
#' xtab <- datawizard::data_tabulate(mtcars, select = "cyl", by = "am")
#' effectsize(xtab)
#'
#' Tt <- t.test(1:10, y = c(7:20), alternative = "less")
#' effectsize(Tt)
#'
#' Tt <- t.test(
#'   x = c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30),
#'   y = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29),
#'   paired = TRUE
#' )
#' effectsize(Tt, type = "rm_b")
#'
#' Aov <- oneway.test(extra ~ group, data = sleep, var.equal = TRUE)
#' effectsize(Aov)
#' effectsize(Aov, type = "omega")
#'
#' Wt <- wilcox.test(1:10, 7:20, mu = -3, alternative = "less", exact = FALSE)
#' effectsize(Wt)
#' effectsize(Wt, type = "u2")
#'
#' ## Models and Anova Tables
#' ## -----------------------
#' fit <- lm(mpg ~ factor(cyl) * wt + hp, data = mtcars)
#' effectsize(fit, method = "basic")
#'
#' anova_table <- anova(fit)
#' effectsize(anova_table)
#' effectsize(anova_table, type = "epsilon")
#'
#' @examplesIf requireNamespace("BayesFactor", quietly = TRUE) && interactive()
#' ## Bayesian Hypothesis Testing
#' ## ---------------------------
#' bf_prop <- BayesFactor::proportionBF(3, 7, p = 0.3)
#' effectsize(bf_prop)
#'
#' bf_corr <- BayesFactor::correlationBF(attitude$rating, attitude$complaints)
#' effectsize(bf_corr)
#'
#' data(RCT_table)
#' bf_xtab <- BayesFactor::contingencyTableBF(RCT_table, sampleType = "poisson", fixedMargin = "cols")
#' effectsize(bf_xtab)
#' effectsize(bf_xtab, type = "oddsratio")
#' effectsize(bf_xtab, type = "arr")
#'
#' bf_ttest <- BayesFactor::ttestBF(sleep$extra[sleep$group == 1],
#'   sleep$extra[sleep$group == 2],
#'   paired = TRUE, mu = -1
#' )
#' effectsize(bf_ttest)
#'
#' @export
effectsize <- function(model, ...) {
  UseMethod("effectsize")
}

#' @export
effectsize.anova <- function(model, type = NULL, ...) {
  if (is.null(type)) {
    type <- "eta"
  }

  f <- switch(
    tolower(type),
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
  r_name <- attr(model, "coefficient_name")
  if (is.null(r_name)) {
    r_name <- "r"
  }

  r_cols <- 1:which(colnames(model) == r_name)
  if (!is.null(attr(model, "ci"))) {
    model$CI <- attr(model, "ci")
    CI_cols <- match(c("CI", "CI_low", "CI_high"), colnames(model))
    r_cols <- c(r_cols, CI_cols)
  }

  out <- model[, r_cols, drop = FALSE]
  class(out) <- c("effectsize_table", "see_effectsize_table", "data.frame")
  attr(out, "approximate") <- FALSE
  out
}


#' @export
effectsize.default <- function(model, ...) {
  # message(insight::format_message("Using standardize_parameters()."))
  parameters::standardize_parameters(model, ...)
}
