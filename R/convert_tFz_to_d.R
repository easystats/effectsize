#' @rdname t_to_r
#' @export
t_to_d <- function(t, df_error, pooled = FALSE, ...) {
  if (isTRUE(pooled)) {
    t / sqrt(df_error)
  } else {
    2 * t / sqrt(df_error)
  }
}

#' @rdname t_to_r
#' @export
convert_t_to_d <- t_to_d




# z -----------------------------------------------------------------------



#' @rdname t_to_r
#' @export
z_to_d <- function(z, n, ...) {
  2 * z / sqrt(n)
}

#' @rdname t_to_r
#' @export
convert_z_to_d <- z_to_d



# F -----------------------------------------------------------------------




#' @rdname t_to_r
#' @export
F_to_d <- function(f, df, df_error, pooled = FALSE, ...) {
  if (df > 1) {
    stop("Cannot convert F with more than 1 df to (partial) r.")
  }
  t_to_d(sqrt(f), df_error, pooled)
}

#' @rdname t_to_r
#' @export
convert_F_to_d <- F_to_d
