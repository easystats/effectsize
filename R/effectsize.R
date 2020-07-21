#' Effect Size
#'
#' See the documentation for [eta_squared()], [standardize_parameters()], [cramers_v()].
#'
#' @param model Statistical model or object of class `htest`.
#' @param ... Arguments passed to or from other methods.
#' See `eta_squared`, `standardize_parameters`, `cramers_v`, `t_to_d`,
#' `d_to_r` or `F_to_eta2`.
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
    out <- t_to_d(
      unname(model$statistic),
      unname(model$parameter),
      paired = grepl("Paired", model$method) | grepl("One Sample", model$method),
      ...
    )
    return(out)
  } else if (grepl("correlation", model$method)) {
    out <- t_to_r(unname(model$statistic), unname(model$parameter), ...)
    out$r <- unname(model$estimate)
    out$CI <- attr(model$conf.int, "conf.level")
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    return(out)
  } else if (grepl("Chi-squared", model$method)) {
    Obs <- model$observed
    Exp <- model$expected

    out <- chisq_to_cramers_v(
      chisq = .chisq(Obs, Exp),
      n = sum(model$observed),
      nrow = nrow(model$observed),
      ncol = ncol(model$observed),
      ...
    )
    return(out)
  } else if (grepl("One-way", model$method)) {
    out <- F_to_eta2(
      model$statistic,
      model$parameter[1],
      model$parameter[2],
      ...
    )
    return(out)
  } else {
    stop("This 'htest' method is not supported.", call. = FALSE)
  }
}

#' @export
effectsize.anova <- function(model, ...) {
  eta_squared(model, ...)
}

#' @export
effectsize.default <- function(model, ...) {
  standardize_parameters(model, ...)
}
