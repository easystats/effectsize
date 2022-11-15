#' Semi-Partial Correlation Squared (aka Delta R2)
#'
#' Compute the semi-partial correlation squared (also known as the delta
#' R2), for an `lm` model.
#'
#' @details This is similar to the last column of the "Conditional Dominance
#'   Statistics" section of the [parameters::dominance_analysis()] output.
#'   For other, non-lm models, as well as more verbose info / options,
#'   please see the documentation for [parameters::dominance_analysis()].
#'
#' @param model An `lm` model.
#' @param type Type, either "term", or "parameters".
#' @param ci Confidence interval, defaults to 95%.
#' @param alternative Alternative for the confidence interval.
#'   One of “two.sided”, “less”, or “greater” (default).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data frame with the effect size.
#'
#' @family effect size correlation
#'
#' @examples
#' m <- lm(mpg ~ factor(cyl) + disp + hp * drat, data = mtcars)
#' r2_semipartial(m)
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

  mf <- stats::model.frame(model)
  mm <- stats::model.matrix(model)
  mterms <- stats::terms(model)

  y <- mf[[1]]

  if (type == "terms") {
    out <- data.frame(Term = attr(mterms,"term.labels"))
    idx <- attr(mm, "assign")
    idx_sub <- idx[idx > 0]
  } else {
    out <- data.frame(Parameter = colnames(mm))
    out <- subset(out, Parameter != "(Intercept)")
    idx <- seq.int(ncol(mm))
    idx_sub <- idx[colnames(mm)!="(Intercept)"]
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

  if (!is.null(ci)) {
    tot_df_model <- insight::get_df(model, type = "model")
    sub_df_model <- tot_df_model - sapply(sub_mods, insight::get_df, type = "model")
    df_res <- stats::df.residual(model)

    F_stat <- (out$r2_semipartial / (1 - out$r2_semipartial)) * (df_res / sub_df_model)

    out <- cbind(out, effectsize::F_to_eta2(F_stat, sub_df_model, df_res, ci = ci, alternative = alternative)[,2:4])
  }

  class(out) <- c("semipartial_r2", "effectsize_table", class(out))

  out
}

#' @export
print.semipartial_r2 <- function(x, ...) {
  names(x)[2] <- "R2 (semi-partial)"
  print(insight::format_table(x))
}