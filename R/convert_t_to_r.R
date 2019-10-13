#' @rdname d_to_r
#' @export
convert_t_to_r <- function(t, df_error){
  # https://www.soph.uab.edu/sites/edu.ssg/files/People/MBeasley/Courses/EffectSizeConversion.pdf
  sign(t) * sqrt(t^2 / (t^2 + df_error))
}

#' @rdname d_to_r
#' @export
convert_t_to_d <- function(t, df_error){
  sign(t) * 2 * t / sqrt(df_error)
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