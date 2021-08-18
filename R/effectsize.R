#' Effect Size
#'
#' This function tries to return the best effect-size measure for the provided
#' input model. See details.
#'
#' @param model An object of class `htest`, or a statistical model. See details.
#' @param type The effect size of interest. See details.
#' @param ... Arguments passed to or from other methods. See details.
#' @inheritParams standardize.default
#'
#' @details
#'
#' - For an object of class `htest`, data is extracted via [insight::get_data()], and passed to the relevant function according to:
#'   - A **t-test** depending on `type`: `"cohens_d"` (default), `"hedges_g"`.
#'   - A **correlation test** returns *r*.
#'   - A **Chi-squared tests of independence or goodness-of-fit**, depending on `type`: `"cramers_v"` (default), `"phi"` or `"cohens_w"`, `"cohens_h"`, `"oddsratio"`, or `"riskratio"`.
#'   - A **One-way ANOVA test**, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#'   - A **McNemar test** returns *Cohen's g*.
#'   - A **Fisher's Exact test** (in the 2x2 case) returns *Odds ratio*.
#'   - A **Wilcoxon test** returns *rank biserial correlation*.
#'   - A **Kruskal-Wallis test** returns *rank Epsilon squared*.
#'   - A **Friedman test** returns *Kendall's W*.
#'   (Where applicable, `ci` and `alternative` are taken from the `htest` if not otherwise provided.)
#' - For an object of class `BFBayesFactor`, using [bayestestR::describe_posterior()],
#'   - A **t-test** returns *Cohen's d*.
#'   - A **correlation test** returns *r*.
#'   - A **contingency table test**, depending on `type`: `"cramers_v"` (default), `"phi"` or `"cohens_w"`, `"cohens_h"`, `"oddsratio"`, or `"riskratio"`.
#' - Objects of class `anova`, `aov`, or `aovlist`, depending on `type`: `"eta"` (default), `"omega"` or `"epsilon"` -squared, `"f"`, or `"f2"`.
#' - Other objects are passed to [standardize_parameters()].
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
#' effectsize(Xsq, type = "phi")
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
#'
#' ## Bayesian Hypothesis Testing
#' ## ---------------------------
#' \donttest{
#' if (require(BayesFactor)) {
#'   bf1 <- ttestBF(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0])
#'   effectsize(bf1, test = NULL)
#'
#'   bf2 <- correlationBF(attitude$rating, attitude$complaints)
#'   effectsize(bf2, test = NULL)
#'
#'   data(raceDolls)
#'   bf3 <- contingencyTableBF(raceDolls, sampleType = "poisson", fixedMargin = "cols")
#'   effectsize(bf3, test = NULL)
#'   effectsize(bf3, type = "oddsratio", test = NULL)
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
#' @rdname effectsize
#' @importFrom insight get_data get_parameters check_if_installed
#' @importFrom bayestestR describe_posterior
effectsize.BFBayesFactor <- function(model, type = NULL, verbose = TRUE, ...) {
  insight::check_if_installed("BayesFactor")

  if (length(model) > 1) {
    if (verbose) {
      warning("Multiple models detected. Using first only.", call. = FALSE)
    }
    model <- model[1]
  }

  if (inherits(model@numerator[[1]], "BFcontingencyTable")) {
    if (is.null(type)) type <- "cramers_v"

    f <- switch(tolower(type),
      v = ,
      cramers_v = cramers_v,
      w = ,
      cohens_w = ,
      phi = phi,
      h = ,
      cohens_h = cohens_h,
      or = ,
      oddsratio = oddsratio,
      rr = ,
      riskratio = riskratio
    )
    data <- insight::get_data(model)
    posts <- insight::get_parameters(model)

    ES <- apply(posts, 1, function(a) {
      f(matrix(a, nrow = nrow(data)), ci = NULL)[[1]]
    })

    res <- data.frame(ES)
    colnames(res) <- colnames(f(data, ci = NULL))
  } else if (inherits(model@numerator[[1]], c("BFoneSample", "BFindepSample"))) {
    D <- as.matrix(BayesFactor::posterior(model, iterations = 4000, progress = FALSE))[, "delta"]
    res <- data.frame(Cohens_d = D)
  } else if (inherits(model@numerator[[1]], "BFcorrelation")) {
    rho <- insight::get_parameters(model)[["rho"]]
    res <- data.frame(rho = rho)
  } else if (inherits(model@numerator[[1]], "BFproportion")) {
    res <- insight::get_parameters(model)
  } else {
    stop("No effect size for this type of BayesFactor object.")
  }

  out <- bayestestR::describe_posterior(res, ...)
  attr(out, "approximate") <- FALSE
  out
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
  standardize_parameters(model, ...)
}
