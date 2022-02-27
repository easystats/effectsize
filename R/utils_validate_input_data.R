
#' @keywords internal
.get_data_2_samples <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {


  if (inherits(x, "formula")) {
    # Validate:
    if (length(x) != 3L) {
      stop("Formula must be two sided.", call. = FALSE)
    }

    # Pull columns
    mf <- .resolve_formula(x, data, ...)

    x <- mf[[1]]
    y <- NULL
    if (ncol(mf) == 2L) {
      y <- mf[[2]]
      if (!is.factor(y)) y <- factor(y)
    } else {
      stop("Formula must have only one term on the RHS.", call. = FALSE)
    }
  } else {
    # Test if they are they are column names
    x <- .resolve_char(x, data)
    y <- .resolve_char(y, data)
  }


  # x should be a numeric vector or a Pair:
  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  } else if (inherits(x, "Pair")) {
    x <- x[, 1] - x[, 2]
    y <- NULL
  }


  # y should be NULL, numeric, or a factor:
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (length(unique(y)) != 2L) {
        stop("Grouping variable y must have exactly 2 levels.", call. = FALSE)
      }

      if (length(x) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- Filter(length, split(x, y))
      x <- data[[1]]
      y <- data[[2]]
    }

    if (verbose && length(unique(y)) == 2) {
      warning("'y' is numeric but has only 2 unique values.",
              "\nIf this is a grouping variable, convert it to a factor.",
              call. = FALSE)
    }
  }

  list(x = x, y = y)
}



#' @keywords internal
.get_data_multi_group <- function(x, groups, data = NULL, ...) {

  if (inherits(frm <- x, "formula")) {
    mf <- .resolve_formula(x, data, ...)

    if (length(frm) != 3 | ncol(mf) != 2) {
      stop("Formula must have the form of 'outcome ~ group'.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- mf[[2]]
    if (!is.factor(groups)) groups <- factor(groups)
  } else if (inherits(x, "list")) {
    groups <- rep(letters[seq_along(x)], sapply(x, length))
    x <- unsplit(x, groups)
  } else {
    # If they are column names
    x <- .resolve_char(x, data)
    groups <- .resolve_char(groups, data)
  }

  # x should be a numeric vector or a Pair:
  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  }

  # groups should be not numeric
  if (length(x) != length(groups)) {
    stop("x and groups must be of the same length.", call. = FALSE)
  }

  if (is.numeric(groups)) {
    stop("groups cannot be numeric.", call. = FALSE)
  }

  data.frame(x, groups)
}

#' @keywords internal
#' @importFrom stats reshape
.get_data_nested_groups <- function(x, groups, blocks, data = NULL, ...) {

  if (inherits(x, "formula")) {
    if ((length(x) != 3L) ||
        (length(x[[3L]]) != 3L) ||
        (x[[3L]][[1L]] != as.name("|"))) {
      stop("Formula must have the 'x ~ groups | blocks'.", call. = FALSE)
    }

    x[[3L]][[1L]] <- as.name("+")

    mf <- .resolve_formula(x, data, ...)

    if (ncol(mf) != 3) {
      stop("Formula must have only two terms on the RHS.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- mf[[2]]
    blocks <- mf[[3]]
  } else if (inherits(x, c("table", "matrix", "array", "data.frame"))) {
    data <- data.frame(
      x = c(x),
      groups = rep(factor(seq_len(ncol(x))), each = nrow(x)),
      blocks = rep(factor(seq_len(nrow(x))), ncol(x))
    )

    x <- data[[1]]
    groups <- data[[2]]
    blocks <- data[[3]]
  } else {
    # If they are column names
    x <- .resolve_char(x, data)
    groups <- .resolve_char(groups, data)
    blocks <- .resolve_char(blocks, data)
  }

  if (length(x) != length(groups) || length(x) != length(blocks)) {
    stop("x, groups and blocks must be of the same length.", call. = FALSE)
  }

  if (!is.factor(groups)) groups <- factor(groups)
  if (!is.factor(blocks)) blocks <- factor(blocks)

  data <- data.frame(x, groups, blocks, stringsAsFactors = FALSE)

  data <- stats::reshape(
    data,
    direction = "wide",
    v.names = "x",
    timevar = "groups",
    idvar = "blocks"
  )

  as.matrix(data[, -1])
}



# Helpers -----------------------------------------------------------------


#' @keywords internal
#' @importFrom stats model.frame
.resolve_formula <- function(formula, data, subset, na.action, ...) {
  cl <- match.call(expand.dots = FALSE)
  cl[[1L]] <- quote(stats::model.frame)
  cl$... <- NULL
  eval(cl, envir = parent.frame())
}

#' @keywords internal
.resolve_char <- function(nm, data) {
  if (is.character(nm)  && length(nm) == 1L) {
    if (is.null(data)) {
      stop("Please provide data argument.", call. = FALSE)
    } else if (!nm %in% names(data)) {
      stop("Column ", x, " missing from data.", call. = FALSE)
    }

    return(data[[nm]])
  }
  nm
}