#' @rdname t_to_r
#' @export
t_to_d <- function(
  t,
  df_error,
  paired = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  ...
) {
  alternative <- .match.alt(alternative)

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * t / sqrt(df_error))

  if (.test_ci(ci)) {
    res$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    ts <- t(mapply(
      .get_ncp_t,
      t,
      df_error,
      ci.level
    ))

    res$CI_low <- paired * ts[, 1] / sqrt(df_error)
    res$CI_high <- paired * ts[, 2] / sqrt(df_error)

    ci_method <- list(method = "ncp", distribution = "t")
    res <- .limit_ci(res, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}


# z -----------------------------------------------------------------------

#' @rdname t_to_r
#' @export
z_to_d <- function(
  z,
  n,
  paired = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  ...
) {
  alternative <- .match.alt(alternative)

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * z / sqrt(n))

  if (.test_ci(ci)) {
    res$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level
    probs <- c(alpha / 2, 1 - alpha / 2)

    qs <- stats::qnorm(probs)
    zs <- cbind(qs[1] + z, qs[2] + z)

    res$CI_low <- paired * zs[, 1] / sqrt(n)
    res$CI_high <- paired * zs[, 2] / sqrt(n)

    ci_method <- list(method = "normal")
    res <- .limit_ci(res, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}


# F -----------------------------------------------------------------------

#' @rdname t_to_r
#' @export
F_to_d <- function(
  f,
  df,
  df_error,
  paired = FALSE,
  ci = 0.95,
  alternative = "two.sided",
  ...
) {
  if (df > 1) {
    insight::format_error(
      "Cannot convert F with more than 1 df to (partial) r."
    )
  }
  t_to_d(
    sqrt(f),
    df_error,
    paired = paired,
    ci = ci,
    alternative = alternative,
    ...
  )
}
