#' @rdname standardize
#' @export
standardize.numeric <- function(x,
                                robust = FALSE,
                                two_sd = FALSE,
                                weights = NULL,
                                verbose = TRUE,
                                ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  if (.are_weights(weights)) {
    valid_x <- !is.na(x) & !is.na(weights)
    x <- x[valid_x]
    weights <- weights[valid_x]
  } else {
    valid_x <- !is.na(x)
    x <- x[valid_x]
  }
  scaled_x <- rep(NA, length(x))


  # Sanity checks
  check <- .check_standardize_numeric(x, name = NULL, verbose = verbose)
  if (is.null(check)) {
    return(x)
  }

  if (is.factor(x) || is.character(x)) {
    x <- .factor_to_numeric(x)
  }

  if (robust) {
    center <- .median(x, weights)
    scale <- .mad(x, weights)
  } else {
    center <- .mean(x, weights)
    scale <- .sd(x, weights)
  }

  if (two_sd) {
    x <- as.vector((x - center) / (2 * scale))
  } else {
    x <- as.vector((x - center) / scale)
  }

  scaled_x[valid_x] <- x
  attr(scaled_x, "center") <- center
  attr(scaled_x, "scale") <- scale
  scaled_x
}

#' @export
standardize.double <- standardize.numeric

#' @export
standardize.integer <- standardize.numeric



#' @export
standardize.factor <- function(x,
                               robust = FALSE,
                               two_sd = FALSE,
                               weights = NULL,
                               verbose = TRUE,
                               force = FALSE,
                               ...) {
  if (!force) {
    return(x)
  }

  standardize(as.numeric(x),
    robust = robust, two_sd = two_sd, weights = weights, verbose = verbose, ...
  )
}


#' @export
standardize.character <- standardize.factor

#' @export
standardize.logical <- standardize.factor

#' @export
standardize.AsIs <- standardize.numeric


# Data frames -------------------------------------------------------------


#' @rdname standardize
#' @param select Character vector of column names. If `NULL` (the default), all
#'   variables will be selected.
#' @param exclude Character vector of column names to be excluded from selection.
#' @param remove_na How should missing values (`NA`) be treated: if `"none"`
#'   (default): each column's standardization is done separately, ignoring
#'   `NA`s. Else, rows with `NA` in the columns selected with `select` /
#'   `exclude` (`"selected"`) or in all columns (`"all"`) are dropped before
#'   standardization, and the resulting data frame does not include these cases.
#' @param force Logical, if `TRUE`, forces standardization of factors as
#'   well. Factors are converted to numerical values, with the lowest level
#'   being the value `1` (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param append Logical, if `TRUE` and `x` is a data frame, standardized
#'   variables will be added as additional columns; if `FALSE`,
#'   existing variables are overwritten.
#' @param suffix Character value, will be appended to variable (column) names of
#'   `x`, if `x` is a data frame and `append = TRUE`.
#'
#' @section Model Standardization:
#' If `x` is a model object, standardization is done by completely refitting the
#' model on the standardized data. Hence, this approach is equal to
#' standardizing the variables *before* fitting the model and will return a new
#' model object. However, this method is particularly recommended for complex
#' models that include interactions or transformations (e.g., polynomial or
#' spline terms). The `robust` (default to `FALSE`) argument enables a robust
#' standardization of data, i.e., based on the `median` and `MAD` instead of the
#' `mean` and `SD`. See [standardize_parameters()] for other methods of
#' standardizing model coefficients.
#'
#' ## Transformed Variables
#' When the model's formula contains transformations (e.g. `y ~ exp(X)`) the
#' transformation effectively takes place after standardization (e.g.,
#' `exp(scale(X))`). Some transformations are undefined for negative values,
#' such as `log()` and `sqrt()`. To avoid dropping these values, the
#' standardized data is shifted by `Z - min(Z) + 1` or `Z - min(Z)`
#' (respectively).
#'
#' @export
standardize.data.frame <- function(x,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   weights = NULL,
                                   verbose = TRUE,
                                   select = NULL,
                                   exclude = NULL,
                                   remove_na = c("none", "selected", "all"),
                                   force = FALSE,
                                   append = FALSE,
                                   suffix = "_z",
                                   ...) {

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  if (!is.null(weights) && is.character(weights)) {
    if (weights %in% colnames(x)) {
      exclude <- c(exclude, weights)
    } else {
      warning("Could not find weighting column '", weights, "'. Weighting not carried out.")
      weights <- NULL
    }
  }

  select <- .select_z_variables(x, select, exclude, force)

  # drop NAs
  remove_na <- match.arg(remove_na)

  omit <- switch(remove_na,
    none = logical(nrow(x)),
    selected = rowSums(sapply(x[select], is.na)) > 0,
    all = rowSums(sapply(x, is.na)) > 0
  )
  x <- x[!omit, , drop = FALSE]

  if (!is.null(weights) && is.character(weights)) weights <- x[[weights]]

  if (append) {
    new_variables <- x[select]
    if (!is.null(suffix)) {
      colnames(new_variables) <- paste0(colnames(new_variables), suffix)
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
  }

  x[select] <- lapply(x[select], standardize,
    robust = robust,
    two_sd = two_sd,
    weights = weights,
    verbose = FALSE,
    force = force
  )

  attr(x, "center") <- sapply(x[select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[select], function(z) attributes(z)$scale)
  x
}



#' @export
standardize.grouped_df <- function(x,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   weights = NULL,
                                   verbose = TRUE,
                                   select = NULL,
                                   exclude = NULL,
                                   remove_na = c("none", "selected", "all"),
                                   force = FALSE,
                                   append = FALSE,
                                   suffix = "_z",
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

  if (is.numeric(weights)) {
    warning(
      "For grouped data frames, 'weights' must be a character, not a numeric vector.\n",
      "Ignoring weightings."
    )
    weights <- NULL
  }


  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  select <- .select_z_variables(x, select, exclude, force)

  if (append) {
    new_variables <- x[select]
    if (!is.null(suffix)) {
      colnames(new_variables) <- paste0(colnames(new_variables), suffix)
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
    info$names <- c(info$names, select)
  }

  for (rows in grps) {
    x[rows, ] <- standardize(
      x[rows, ],
      select = select,
      exclude = NULL,
      robust = robust,
      two_sd = two_sd,
      weights = weights,
      remove_na = remove_na,
      verbose = verbose,
      force = force,
      append = FALSE,
      suffix = NULL,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}



# helper -----------------------------


.select_z_variables <- function(x, select, exclude, force) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  if (!force) {
    factors <- sapply(x[select], function(i) is.factor(i) | is.character(i))
    select <- select[!factors]
  }

  select
}

#' @keywords internal
.check_standardize_numeric <- function(x, name = NULL, verbose = TRUE) {
  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only one unique value and will not be standardized.")
      } else {
        message(paste0("The variable `", name, "` contains only one unique value and will not be standardized."))
      }
    }
    return(NULL)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2 && !is.factor(x) && !is.character(x)) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only two different values. Consider converting it to a factor.")
      } else {
        message(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
      }
    }
  }
  x
}
