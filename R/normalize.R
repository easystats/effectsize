#' Normalize numeric variable to \[0-1\] range
#'
#' Performs a normalization of data, i.e., it scales variables in the range 0 -
#' 1. This is a special case of [change_scale()].
#'
#' @inheritParams standardize.data.frame
#'
#' @param x A numeric vector, data frame, or matrix. See details.
#' @param include_bounds Logical, if `TRUE`, return value may include 0 and 1.
#'   If `FALSE`, the return value is compressed, using Smithson and Verkuilen's
#'   (2006) formula `(x * (n - 1) + 0.5) / n`, to avoid zeros and ones in the
#'   normalized variables. This can be useful in case of beta-regression, where
#'   the response variable is not allowed to include zeros and ones.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#' - If `x` is a matrix, normalization is performed across all values (not
#'   column- or row-wise). For column-wise normalization, convert the matrix to a
#'   data.frame.
#' - If `x` is a grouped data frame (`grouped_df`), normalization is performed
#'   separately for each group.
#'
#' @examples
#' normalize(c(0, 1, 5, -5, -2))
#' normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE)
#'
#' head(normalize(trees))
#' @references
#' - Smithson M, Verkuilen J (2006). A Better Lemon Squeezer? Maximum-Likelihood Regression with Beta-Distributed Dependent Variables. Psychological Methods, 11(1), 54–71.
#'
#' @family transform utilities
#'
#' @return A normalized object.
#'
#' @export
normalize <- function(x, ...) {
  UseMethod("normalize")
}






#' @rdname normalize
#' @export
normalize.numeric <- function(x, include_bounds = TRUE, verbose = TRUE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }


  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only one unique value and will not be normalized."))
    }
    return(x)
  }


  # Warning if logical vector
  if (length(unique(x)) == 2) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    if (verbose) {
      warning(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
    }
  }


  out <- as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE))

  if (!include_bounds && (any(out == 0) | any(out == 1))) {
    out <- (out * (length(out) - 1) + 0.5) / length(out)
  }

  out
}








#' @export
normalize.factor <- function(x, ...) {
  x
}




#' @rdname normalize
#' @export
normalize.grouped_df <- function(x, select = NULL, exclude = NULL, include_bounds = TRUE, verbose = TRUE, ...) {
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
    x[rows, ] <- normalize(
      x[rows, ],
      select = select,
      exclude = exclude,
      include_bounds = include_bounds,
      verbose = verbose,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname normalize
#' @export
normalize.data.frame <- function(x, select = NULL, exclude = NULL, include_bounds = TRUE, verbose = TRUE, ...) {
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

  x[select] <- lapply(x[select], normalize, include_bounds = include_bounds, verbose = verbose)
  x
}


#' @export
normalize.matrix <- function(x, ...) {
  matrix(normalize(as.numeric(x), ...), nrow = nrow(x))
}
