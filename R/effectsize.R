#' Effect Size
#'
#' This function tries to return the best effect-size measure for the provided
#' input model. See details.
#'
#' @param model An object of class `htest`, or a statistical model. See details.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'
#' - For an object of class `htest`:
#'   - A **t-test** returns *Cohen's d* via [t_to_d()].
#'   - A **correlation test** returns *r*. See [t_to_r()].
#'   - A **Chi-squared test** returns *Cramer's V* via [cramers_v()].
#'   - A **One-way ANOVA test** returns *Eta squared* via [F_to_eta2()], but can be changes via an `es` argument.
#' - Objects of class `anova`, `aov`, or `aovlist` are passed to [eta_squared()].
#' - Other objects are passed to [standardize_parameters()].
#'
#' **For statistical models it is recommended to directly use the listed
#' functions, for the full range of options they provide.**
#'
#' @examples
#' contingency_table <- as.table(rbind(c(762, 327, 468), c(484, 239, 477), c(484, 239, 477)))
#' Xsq <- chisq.test(contingency_table)
#' effectsize(Xsq)
#'
#' Ts <- t.test(1:10, y = c(7:20))
#' effectsize(Ts)
#'
#' Aov <- oneway.test(extra ~ group, data = sleep)
#' effectsize(Aov)
#'
#' fit <- lm(mpg ~ factor(cyl) * wt + hp, data = mtcars)
#' effectsize(fit)
#'
#' anova_table <- anova(fit)
#' effectsize(anova_table)
#'
#' @export
effectsize <- function(model, ...) {
  UseMethod("effectsize")
}

#' @export
effectsize.htest <- function(model, ...) {
  if (grepl("t-test", model$method)) {
    # message("Using t_to_d().")
    out <- t_to_d(
      unname(model$statistic),
      unname(model$parameter),
      paired = !grepl("Two", model$method),
      ...
    )
    return(out)
  } else if (grepl("correlation", model$method)) {
    out <- t_to_r(1, 1, ci = NULL)
    out$r <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    return(out)
  } else if (grepl("Pearson's Chi-squared", model$method) ||
             grepl("Chi-squared test for given probabilities", model$method)) {
    # message("Using chisq_to_cramers_v().")
    Obs <- model$observed
    Exp <- model$expected

    if (!is.null(dim(Exp))) {
      nr <- nrow(Obs)
      nc <- ncol(Obs)
    } else {
      nr <- length(Obs)
      nc <- 1
    }

    out <- chisq_to_cramers_v(
      chisq = .chisq(Obs, Exp),
      n = sum(Obs),
      nrow = nr,
      ncol = nc,
      ...
    )
    return(out)
  } else if (grepl("One-way", model$method)) {
    # message("Using F_to_eta2/epsilon2/omega2().")
    out <- .F_to_pve(
      model$statistic,
      model$parameter[1],
      model$parameter[2],
      ...
    )
    return(out)
  } else if (grepl("McNemar", model$method)) {
    stop("Cannot extract Cohen's g from an 'htest' object.",
         "\nTry using 'cohens_g()' directly.", call. = FALSE)
  } else if (grepl("Fisher's Exact", model$method)) {
    stop("Cannot extract effect size from an 'htest' of Fisher's exact test.",
         "\nTry using 'cramers_v()' or 'phi()' directly.", call. = FALSE)
  } else if (grepl("Wilcoxon", model$method)) {
    stop("Cannot extract effect size from an 'htest' of Wilcoxon's test.",
         "\nTry using 'ranktransform()' and 'cohens_d()' directly.", call. = FALSE)
  } else {
    stop("This 'htest' method is not (yet?) supported.", call. = FALSE)
  }
}

#' @export
effectsize.anova <- function(model, ...) {
  # message("Using eta_squared().")
  eta_squared(model, ...)
}

#' @export
effectsize.aov <- effectsize.anova

#' @export
effectsize.aovlist <- effectsize.anova


#' @export
effectsize.easycorrelation <- function(model, ...){
  if (is.null(r_name <- attr(model, "coefficient_name")))
    r_name <- "r"

  r_cols <- 1:which(colnames(model) == r_name)
  if (!is.null(attr(model, "ci"))) {
    model$CI <- attr(model, "ci")
    CI_cols <- c("CI", "CI_low", "CI_high")
    CI_cols <- sapply(CI_cols, function(ici) which(colnames(model) == ici))
    r_cols <- c(r_cols, CI_cols)
  }

  out <- model[,r_cols, drop = FALSE]
  class(out) <- c("effectsize_table", "data.frame")
  out
}


#' @export
effectsize.default <- function(model, ...) {
  # message("Using standardize_parameters().")
  standardize_parameters(model, ...)
}
