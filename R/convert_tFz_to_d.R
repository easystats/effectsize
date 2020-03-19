#' @rdname t_to_r
#' @export
t_to_d <- function(t, df_error, pooled = FALSE, CI = 0.95, ...) {
  # Will be 1 if TRUE, and 2 if FALSE
  pooled <- 2 - pooled

  res <- data.frame(d = pooled * t / sqrt(df_error))

  if (is.numeric(CI)) {
    stopifnot(length(CI) == 1, CI < 1, CI > 0)
    res$CI <- CI

    ts <- t(mapply(.get_ncp_t,
                   t, df_error, CI))

    res$CI_low <- pooled * ts[,1] / sqrt(df_error)
    res$CI_high <- pooled * ts[,2] / sqrt(df_error)
  }

  return(res)
}

#' @rdname t_to_r
#' @export
convert_t_to_d <- t_to_d




# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
z_to_d <- function(z, n, pooled = FALSE, CI = 0.95, ...) {
  # Will be 1 if TRUE, and 2 if FALSE
  pooled <- 2 - pooled

  res <- data.frame(d = pooled * z / sqrt(n))

  if (is.numeric(CI)) {
    stopifnot(length(CI) == 1, CI < 1, CI > 0)
    res$CI <- CI

    alpha <- 1 - CI
    probs <- c(alpha / 2, 1 - alpha / 2)

    qs <- qnorm(probs)
    zs <- cbind(qs[1] + z, qs[2] + z)

    res$CI_low <- pooled * zs[,1] / sqrt(n)
    res$CI_high <- pooled * zs[,2] / sqrt(n)
  }

  return(res)
}

#' @rdname t_to_r
#' @export
convert_z_to_d <- z_to_d



# F -----------------------------------------------------------------------




#' @rdname t_to_r
#' @export
F_to_d <- function(f, df, df_error, pooled = FALSE, CI = 0.95, ...) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to (partial) r.")
  }
  t_to_d(sqrt(f), df_error, pooled, CI)
}

#' @rdname t_to_r
#' @export
convert_F_to_d <- F_to_d
