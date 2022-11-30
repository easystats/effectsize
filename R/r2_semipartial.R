#' Semi-Partial Squared Correlation (\eqn{\Delta R^2})
#'
#' Compute the semi-partial squared correlation (also known as \eqn{\Delta
#' R^2}). Currently, only `lm()` models are supported.
#'
#' @param model An `lm` model.
#' @param type Type, either `"terms"`, or `"parameters"`.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams eta_squared
#'
#' @return A data frame with the effect size.
#'
#' @details
#' This is similar to the last column of the "Conditional Dominance Statistics"
#' section of the [parameters::dominance_analysis()] output. For each term, the
#' model is refit *without* the columns on the [model
#' matrix][stats::model.matrix] that corresponds to that term. The \eqn{R^2} of
#' this *sub*-model is then subtracted from the \eqn{R^2} of the *full* model to
#' yield the \eqn{\Delta R^2}. (For `type = "parameters"`, this is done for each
#' column in the model matrix.)
#'
#' **Note** that this is unlike [parameters::dominance_analysis()], where term
#' deletion is done via the formula interface, and therefore may lead to
#' different results.
#'
#' For other, non-`lm()` models, as well as more verbose information and
#' options, please see the documentation for [parameters::dominance_analysis()].
#'
#' # Confidence (Compatibility) Intervals (CIs)
#' Confidence intervals are based on the normal approximation as provided by
#' Alf and Graf (1999).
#'
#' @inheritSection effectsize_CIs CIs and Significance Tests
#'
#' @seealso [eta_squared()], [parameters::dominance_analysis()] and
#'   [parameters::standardise_parameters()].
#'
#' @references
#' Alf Jr, E. F., & Graf, R. G. (1999). Asymptotic confidence limits for the
#' difference between two squared multiple correlations: A simplified approach.
#' Psychological Methods, 4(1), 70. \doi{10.1037/1082-989X.4.1.70}
#'
#' @examples
#' data("hardlyworking")
#'
#' m <- lm(salary ~ factor(n_comps) + xtra_hours * seniority, data = hardlyworking)
#'
#' r2_semipartial(m)
#'
#' r2_semipartial(m, type = "parameters")
#'
#' @export
r2_semipartial <- function(model, type = c("terms", "parameters"),
                           ci = 0.95, alternative = "greater",
                           ...) {
  UseMethod("r2_semipartial")
}

#' @export
r2_semipartial.lm <- function(model, type = c("terms", "parameters"),
                              ci = 0.95, alternative = "greater",
                              ...) {
  type <- match.arg(type)
  alternative <- .match.alt(alternative)

  mf <- stats::model.frame(model)
  mm <- stats::model.matrix(model)
  mterms <- stats::terms(model)

  y <- mf[[1]]

  if (type == "terms") {
    out <- data.frame(Term = attr(mterms,"term.labels"))
    idx <- attr(mm, "assign")
    idx_sub <- idx[idx > 0]
  } else {
    Parameter <- colnames(mm)
    out <- data.frame(Parameter = Parameter)
    out <- subset(out, Parameter != "(Intercept)")
    idx <- seq.int(ncol(mm))
    idx_sub <- idx[Parameter!="(Intercept)"]
  }

  tot_mod <- if (attr(mterms,"intercept") == 1) {
    stats::lm(y ~ 1 + mm)
  } else {
    stats::lm(y ~ 0 + mm)
  }

  sub_mods <- lapply(unique(idx_sub), function(.i) {
    if (attr(mterms,"intercept") == 1) {
      stats::lm(y ~ 1 + mm[,.i!=idx])
    } else {
      stats::lm(y ~ 0 + mm[,.i!=idx])
    }
  })

  tot_r2 <- performance::r2(model)[[1]]
  sub_r2 <- lapply(sub_mods, performance::r2)
  sub_r2 <- sapply(sub_r2, "[[", 1)

  out$r2_semipartial <- unname(tot_r2 - sub_r2)

  if (.test_ci(ci)) {
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)
    alpha <- 1 - ci.level
    zc <- stats::qnorm(alpha / 2, lower.tail = FALSE)

    N <- insight::n_obs(model)
    SE <- unname(.delta_r2_SE(sub_r2, tot_r2, N))


    out$CI_low <- pmax(out$r2_semipartial - zc * SE, 0)
    out$CI_high <- pmin(out$r2_semipartial + zc * SE, 1)

    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, 0, 1)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_table", class(out))
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
  out
}


# Utils -------------------------------------------------------------------

#' @keywords internal
.delta_r2_V <- function(R2, N) {
  (4 * R2 * ((1 - R2) ^ 2)) / N
}

#' @keywords internal
.delta_r2_COV <- function(R2r, R2f, N) {
  rhor <- sqrt(R2r)
  rhof <- sqrt(R2f)

  (4 * rhof * rhor * (0.5 * (2 * rhor / rhof - rhor * rhof) * (1 - R2f - R2r - R2r / R2f) + (rhor / rhof)^3)) / N
}

#' @keywords internal
.delta_r2_SE <- function(R2r, R2f, N) {
  Vf <- .delta_r2_V(R2f, N)
  Vr <- .delta_r2_V(R2r, N)
  COVfr <- .delta_r2_COV(R2r, R2f, N)

  sqrt(Vf + Vr - 2 * COVfr)
}