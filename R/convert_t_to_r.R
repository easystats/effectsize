
# t -----------------------------------------------------------------------



#' @rdname d_to_r
#' @export
convert_t_to_r <- function(t, df_error) {
  # https://www.soph.uab.edu/sites/edu.ssg/files/People/MBeasley/Courses/EffectSizeConversion.pdf
  sign(t) * sqrt(t^2 / (t^2 + df_error))
}

#' @rdname d_to_r
#' @export
t_to_r <- convert_t_to_r


#' @rdname d_to_r
#' @export
convert_t_to_d <- function(t, df_error) {
  2 * t / sqrt(df_error)
}

#' @rdname d_to_r
#' @export
t_to_d <- convert_t_to_d




# z -----------------------------------------------------------------------



#' @rdname d_to_r
#' @export
convert_z_to_r <- function(z, n) {
  sign(z) * sqrt(z^2 / (z^2 + n))
}


#' @rdname d_to_r
#' @export
z_to_r <- convert_z_to_r


#' @rdname d_to_r
#' @export
convert_z_to_d <- function(z, n) {
  2 * z / sqrt(n)
}

#' @rdname d_to_r
#' @export
z_to_d <- convert_z_to_d



# F -----------------------------------------------------------------------



#' @rdname d_to_r
#' @export
convert_F_to_r <- function(f, df, df_error) {
  print("TODO")
}
