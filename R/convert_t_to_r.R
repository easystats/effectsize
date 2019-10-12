#' @rdname d_to_r
#' @export
convert_t_to_r <- function(t, df_error){
  sqrt(t^2 / (t^2 + df_error))
}

#' @rdname d_to_r
#' @export
convert_t_to_d <- function(t, df_error){
  2 * t / sqrt(df_error)
}


#' @rdname d_to_r
#' @export
convert_z_to_r <- function(z, df_error){
  print("TODO")
}

#' @rdname d_to_r
#' @export
convert_F_to_r <- function(f, df_error){
  print("TODO")
}