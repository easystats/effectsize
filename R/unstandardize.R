#' @param center,scale,reference Used by `unstandardize()`; `center` and `scale` correspond to the center (the mean) and the scale (SD) of the original non-standardized data. However, one can also directly provide the original data through `reference`, from which the center and the scale will be computed. Also, if the input contains the attributes `center` and `scale` (as does the output of `standardize()`), it will take it from there if the rest of the arguments are absent.
#' @rdname standardize
#' @export
unstandardize <- function(x, center = NULL, scale = NULL, reference = NULL) {
  UseMethod("unstandardize")
}


#' @export
unstandardize.numeric <- function(x, center = NULL, scale = NULL, reference = NULL) {

  if(!is.null(reference)) {
    center <- mean(reference, na.rm = TRUE)
    scale <- stats::sd(reference, na.rm = TRUE)
  } else if (is.null(center) && is.null(scale)) {
    if ( all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attributes(x)$center
      scale <- attributes(x)$scale
    } else{
      stop("You must provide the arguments `center`, `scale` or `reference`.")
    }
  }

  x * scale + center
}

#' @export
unstandardize.data.frame <- function(x, center = NULL, scale = NULL, reference = NULL) {

  nums <- sapply(x, is.numeric)

  if(!is.null(reference)) {
    reference <- reference[nums]
    center <- sapply(reference, mean, na.rm = TRUE)
    scale <- sapply(reference, stats::sd, na.rm = TRUE)
  } else if (is.null(center) && is.null(scale)) {
    if ( all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attributes(x)$center
      scale <- attributes(x)$scale
    } else{
      stop("You must provide the arguments `center`, `scale` or `reference`.")
    }
  }

  for(i in 1:sum(nums)) {
    x[nums][[i]] <- unstandardize(x[nums][[i]], center[i], scale[i])
  }
  x
}
