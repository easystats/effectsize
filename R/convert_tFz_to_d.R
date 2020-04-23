#' @rdname t_to_r
#' @export
t_to_d <- function(t, df_error, paired = FALSE, ci = 0.95, pooled, ...) {
  if (!missing(pooled)) {
    paired <- pooled
    warning(
      "Argument 'pooled' is deprecated, use 'paired' instead. Setting paired <- pooled.",
      call. = FALSE
    )
  }

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * t / sqrt(df_error))

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    ts <- t(mapply(.get_ncp_t,
                   t, df_error, ci))

    res$CI_low <- paired * ts[,1] / sqrt(df_error)
    res$CI_high <- paired * ts[,2] / sqrt(df_error)
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}

#' @rdname t_to_r
#' @export
convert_t_to_d <- t_to_d




# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
z_to_d <- function(z, n, paired = FALSE, ci = 0.95, pooled, ...) {
  if (!missing(pooled)) {
    paired <- pooled
    warning(
      "Argument 'pooled' is deprecated, use 'paired' instead. Setting paired <- pooled.",
      call. = FALSE
    )
  }

  # Will be 1 if TRUE, and 2 if FALSE
  paired <- 2 - paired

  res <- data.frame(d = paired * z / sqrt(n))

  if (is.numeric(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)
    res$CI <- ci

    alpha <- 1 - ci
    probs <- c(alpha / 2, 1 - alpha / 2)

    qs <- qnorm(probs)
    zs <- cbind(qs[1] + z, qs[2] + z)

    res$CI_low <- paired * zs[,1] / sqrt(n)
    res$CI_high <- paired * zs[,2] / sqrt(n)
  }

  class(res) <- c("effectsize_table", "see_effectsize_table", class(res))
  return(res)
}

#' @rdname t_to_r
#' @export
convert_z_to_d <- z_to_d



# F -----------------------------------------------------------------------




#' @rdname t_to_r
#' @export
F_to_d <- function(f, df, df_error, paired = FALSE, ci = 0.95, pooled, ...) {
  if (!missing(pooled)) {
    paired <- pooled
    warning(
      "Argument 'pooled' is deprecated, use 'paired' instead. Setting paired <- pooled.",
      call. = FALSE
    )
  }

  if (df > 1) {
    stop("Cannot convert F with more than 1 df to (partial) r.")
  }
  t_to_d(sqrt(f), df_error, paired, ci)
}

#' @rdname t_to_r
#' @export
convert_F_to_d <- F_to_d
