#' @param center,scale,reference Used by `unstandardize()`; `center` and `scale`
#'   correspond to the center (the mean / median) and the scale (SD / MAD) of
#'   the original non-standardized data (for data frames, should be named, or
#'   have column order correspond to the numeric column). However, one can also
#'   directly provide the original data through `reference`, from which the
#'   center and the scale will be computed (according to `robust` and `two_sd`.
#'   Alternatively, if the input contains the attributes `center` and `scale`
#'   (as does the output of `standardize()`), it will take it from there if the
#'   rest of the arguments are absent.
#' @rdname standardize
#' @export
unstandardize <- function(x,
                          center = NULL,
                          scale = NULL,
                          reference = NULL,
                          robust = FALSE,
                          two_sd = FALSE,
                          ...) {
  UseMethod("unstandardize")
}


#' @export
unstandardize.numeric <- function(x,
                                  center = NULL,
                                  scale = NULL,
                                  reference = NULL,
                                  robust = FALSE,
                                  two_sd = FALSE,
                                  ...) {
  if (!is.null(reference)) {
    if (robust) {
      center <- stats::median(reference, na.rm = TRUE)
      scale <- stats::mad(reference, na.rm = TRUE)
    } else {
      center <- mean(reference, na.rm = TRUE)
      scale <- stats::sd(reference, na.rm = TRUE)
    }
  } else if (is.null(center) || is.null(scale)) {
    if (all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attr(x, "center", exact = TRUE)
      scale <- attr(x, "scale", exact = TRUE)
      attr(x, "scale") <- attr(x, "center") <- NULL
    } else if (all(c("scaled:center", "scaled:scale") %in% names(attributes(x)))) {
      center <- attr(x, "scaled:center", exact = TRUE)
      scale <- attr(x, "scaled:scale", exact = TRUE)
      attr(x, "scaled:scale") <- attr(x, "scaled:center") <- NULL
    } else {
      stop("You must provide the arguments `center`, `scale` or `reference`.")
    }
  }

  if (two_sd) {
    scale <- 2 * scale
  }

  x * scale + center
}

#' @export
unstandardize.data.frame <- function(x,
                                     center = NULL,
                                     scale = NULL,
                                     reference = NULL,
                                     robust = FALSE,
                                     two_sd = FALSE,
                                     ...) {
  if (!is.null(reference)) {
    i <- sapply(x, is.numeric)
    i <- i[i]
    reference <- reference[names(i)]
    if (robust) {
      center <- sapply(reference, stats::median, na.rm = TRUE)
      scale <- sapply(reference, stats::mad, na.rm = TRUE)
    } else {
      center <- sapply(reference, mean, na.rm = TRUE)
      scale <- sapply(reference, stats::sd, na.rm = TRUE)
    }
  } else if (is.null(center) || is.null(scale)) {
    i <- sapply(x, function(k) {
      is.numeric(k) && !is.null(a <- attributes(k)) && all(c("scale", "center") %in% names(a))
    })

    if (any(i)) {
      i <- i[i]
      center <- sapply(x[names(i)], attr, "center", exact = TRUE)
      scale <- sapply(x[names(i)], attr, "scale", exact = TRUE)
    } else if (all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attr(x, "center", exact = TRUE)
      scale <- attr(x, "scale", exact = TRUE)
      attr(x, "center") <- attr(x, "scale") <- NULL
      i <- names(x) %in% names(scale)
      i <- i[i]
    } else {
      stop("You must provide the arguments `center`, `scale` or `reference`.")
    }
  } else {
    if (is.null(names(center))) {
      i <- sapply(x, is.numeric)
      names(center) <- names(scale) <- names(x[i])
    }

    i <- names(x) %in% names(center)
    names(i) <- names(x)
    i <- i[i]
  }

  if (two_sd) {
    scale <- 2 * scale
  }

  x[names(i)] <- mapply(unstandardize, x[names(i)],
    center = center[names(i)],
    scale = scale[names(i)]
  )
  x
}

#' @export
unstandardize.grouped_df <- function(x,
                                     center = NULL,
                                     scale = NULL,
                                     reference = NULL,
                                     robust = FALSE,
                                     two_sd = FALSE,
                                     ...) {
  stop("Cannot (yet) unstandardize a grouped_df.")
}

#' @export
unstandardize.matrix <- function(x,
                                 center = NULL,
                                 scale = NULL,
                                 reference = NULL,
                                 robust = FALSE,
                                 two_sd = FALSE,
                                 ...) {
  if (all(c("scaled:center", "scaled:scale") %in% names(attributes(x)))) {
    center <- attr(x, "scaled:center", exact = TRUE)
    scale <- attr(x, "scaled:scale", exact = TRUE)
    attr(x, "scaled:center") <- attr(x, "scaled:scale") <- NULL

    for (col in seq_len(ncol(x))) {
      x[, col] <- unstandardize.numeric(x[, col], center = center[col], scale = scale[col])
    }
  } else {
    x <- unstandardize.numeric(
      x,
      center = center,
      scale = scale,
      reference = reference,
      robust = robust,
      two_sd = two_sd
    )
  }
  x
}

#' @export
unstandardize.array <- unstandardize.matrix
