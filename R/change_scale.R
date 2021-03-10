#' Rescale a numeric variable
#'
#' Rescale a numeric variable to a new range.
#'
#' @inheritParams standardize.data.frame
#'
#' @param x Object.
#' @param to New range of values of the data after rescaling.
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   data.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' change_scale(c(0, 1, 5, -5, -2))
#' change_scale(c(0, 1, 5, -5, -2), to = c(-5, 5))
#'
#' head(change_scale(trees))
#' @seealso [normalize()] [standardize()] [ranktransform()]
#'
#' @return A rescaled object.
#'
#' @family transform utilities
#'
#' @export
change_scale <- function(x, ...) {
  UseMethod("change_scale")
}






#' @rdname change_scale
#' @export
change_scale.numeric <- function(x,
                                 to = c(0, 100),
                                 range = NULL,
                                 verbose = TRUE,
                                 ...) {
  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  # Warning if only one value
  if (length(unique(x)) == 1 && is.null(range)) {
    if (verbose) {
      warning(paste0("A `range` must be provided for data with only one observation."))
    }
    return(x)
  }

  if (is.null(range)) {
    range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }

  min <- ifelse(is.na(range[1]), min(x, na.rm = TRUE), range[1])
  max <- ifelse(is.na(range[2]), max(x, na.rm = TRUE), range[2])
  new_min <- ifelse(is.na(to[1]), min, to[1])
  new_max <- ifelse(is.na(to[2]), max, to[2])

  out <- as.vector((new_max - new_min) / (max - min) * (x - min) + new_min)
  out
}




#' @export
change_scale.factor <- function(x, ...) {
  x
}




#' @rdname change_scale
#' @export
change_scale.grouped_df <- function(x,
                                    select = NULL,
                                    exclude = NULL,
                                    to = c(0, 100),
                                    range = NULL,
                                    ...) {
  info <- attributes(x)

  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- change_scale(
      x[rows, ],
      select = select,
      exclude = exclude,
      to = to,
      range = range,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname change_scale
#' @export
change_scale.data.frame <- function(x,
                                    select = NULL,
                                    exclude = NULL,
                                    to = c(0, 100),
                                    range = NULL,
                                    ...) {

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  x[select] <- lapply(x[select], change_scale, to = to, range = range)
  x
}
