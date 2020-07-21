#' @importFrom stats median mad na.omit
#' @export
standardize.numeric <- function(x, robust = FALSE, two_sd = FALSE, verbose = TRUE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  valid_x <- !is.na(x)
  scaled_x <- rep(NA, length(x))
  x <- stats::na.omit(x)

  # Sanity checks
  check <- .check_standardize_numeric(x, name = NULL, verbose = verbose)
  if (is.null(check)) {
    return(x)
  }

  if (is.factor(x) || is.character(x)) {
    x <- .factor_to_numeric(x)
  }

  if (robust) {
    center <- stats::median(x)
    scale <- stats::mad(x)
  } else {
    center <- mean(x)
    scale <- stats::sd(x)
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





#' @export
standardize.factor <- function(x, force = FALSE, ...) {
  if (force) {
    standardize(as.numeric(x), ...)
  } else {
    x
  }
}


#' @export
standardize.character <- standardize.factor


#' @export
standardize.logical <- standardize.factor

#' @export
standardize.AsIs <- standardize.numeric



#' @export
standardize.grouped_df <- function(x, robust = FALSE, two_sd = FALSE, select = NULL, exclude = NULL, verbose = TRUE, force = FALSE, append = FALSE, suffix = "_z", ...) {
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



#' @rdname standardize
#' @export
standardize.data.frame <- function(x, robust = FALSE, two_sd = FALSE, select = NULL, exclude = NULL, verbose = TRUE, force = FALSE, append = FALSE, suffix = "_z", ...) {
  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  select <- .select_z_variables(x, select, exclude, force)

  if (append) {
    new_variables <- x[select]
    if (!is.null(suffix)) {
      colnames(new_variables) <- paste0(colnames(new_variables), suffix)
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
  }

  x[select] <- lapply(x[select], standardize, robust = robust, two_sd = two_sd, verbose = FALSE, force = force)

  attr(x, "center") <- sapply(x[select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[select], function(z) attributes(z)$scale)
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
