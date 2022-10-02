#' @rdname t_to_r
#' @export
t_to_d <- function(t, df_error,
                   paired = FALSE,
                   ci = 0.95, alternative = "two.sided", ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * t / sqrt(df_error))

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    ts <- t(mapply(
      .get_ncp_t,
      t, df_error, ci.level
    ))

    res$CI_low <- paired * ts[, 1] / sqrt(df_error)
    res$CI_high <- paired * ts[, 2] / sqrt(df_error)

    ci_method <- list(method = "ncp", distribution = "t")
    if (alternative == "less") {
      res$CI_low <- -Inf
    } else if (alternative == "greater") {
      res$CI_high <- Inf
    }
  } else {
    alternative <- NULL
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  attr(res, "ci") <- ci
  attr(res, "ci_method") <- ci_method
  attr(res, "alternative") <- alternative
  return(res)
}




# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @importFrom stats qnorm
#' @export
z_to_d <- function(z, n,
                   paired = FALSE,
                   ci = 0.95, alternative = "two.sided",
                   ...) {
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * z / sqrt(n))

  ci_method <- NULL
  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    alpha <- 1 - ci.level
    probs <- c(alpha / 2, 1 - alpha / 2)

    qs <- stats::qnorm(probs)
    zs <- cbind(qs[1] + z, qs[2] + z)

    res$CI_low <- paired * zs[, 1] / sqrt(n)
    res$CI_high <- paired * zs[, 2] / sqrt(n)

    ci_method <- list(method = "normal")
    if (alternative == "less") {
      res$CI_low <- -Inf
    } else if (alternative == "greater") {
      res$CI_high <- Inf
    }
  } else {
    alternative <- NULL
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
F_to_d <- function(f, df, df_error,
                   paired = FALSE,
                   ci = 0.95, alternative = "two.sided",
                   ...) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to (partial) r.", call. = FALSE)
  }
  t_to_d(sqrt(f), df_error,
    paired = paired,
    ci = ci, alternative = alternative,
    ...
  )
}
