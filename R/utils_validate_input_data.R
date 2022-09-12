
#' @keywords internal
.get_data_2_samples <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    # Validate:
    if (length(x) != 3L) {
      stop("Formula must have one of the following forms:",
        "\n\ty ~ group,\n\ty ~ 1,\n\tPair(x,y) ~ 1",
        call. = FALSE
      )
    }

    # Pull columns
    mf <- .resolve_formula(x, data, ...)

    if (ncol(mf) > 2L) {
      stop("Formula must have only one term on the RHS.", call. = FALSE)
    }

    x <- mf[[1]]
    y <- NULL
    if (ncol(mf) == 2L) {
      y <- mf[[2]]
      if (!is.factor(y)) y <- factor(y)
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
      if (insight::n_unique(y) != 2) {
        stop("Grouping variable y must have exactly 2 levels.", call. = FALSE)
      }

      if (length(x) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- Filter(length, split(x, y))
      x <- data[[1]]
      y <- data[[2]]
    }

    if (verbose && insight::n_unique(y) == 2) {
      warning("'y' is numeric but has only 2 unique values.",
        "\nIf this is a grouping variable, convert it to a factor.",
        call. = FALSE
      )
    }
  }

  list(x = x, y = y)
}



#' @keywords internal
.get_data_multi_group <- function(x, groups, data = NULL, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3) {
      stop("Formula must have the form of 'outcome ~ group'.", call. = FALSE)
    }

    mf <- .resolve_formula(x, data, ...)

    if (ncol(mf) != 2L) {
      stop("Formula must have only one term on the RHS.", call. = FALSE)
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
.get_data_nested_groups <- function(x, groups = NULL, blocks = NULL, data = NULL, wide = TRUE, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L ||
      x[[3L]][[1L]] != as.name("|")) {
      stop("Formula must have the 'x ~ groups | blocks'.", call. = FALSE)
    }

    x[[3L]][[1L]] <- as.name("+")

    x <- .resolve_formula(x, data, ...)

    if (ncol(x) != 3L) {
      stop("Formula must have only two term on the RHS.", call. = FALSE)
    }
  } else if (inherits(x, "data.frame")) {
    x <- as.matrix(x)
  } else if (!inherits(x, c("table", "matrix", "array"))) {
    x <- .resolve_char(x, data)
    groups <- .resolve_char(groups, data)
    blocks <- .resolve_char(blocks, data)

    if (length(x) != length(groups) || length(x) != length(blocks)) {
      stop("x, groups and blocks must be of the same length.", call. = FALSE)
    }

    x <- data.frame(x, groups, blocks)
  }


  if (inherits(x, c("matrix", "array"))) {
    x <- as.table(x)
  }

  if (inherits(x, c("table"))) {
    x <- as.data.frame(x)[, c(3, 2, 1)]
  }

  colnames(x) <- c("x", "groups", "blocks")

  if (!is.numeric(x$x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  }
  if (!is.factor(x$groups)) x$groups <- factor(x$groups)
  if (!is.factor(x$blocks)) x$blocks <- factor(x$blocks)

  # By this point, the data is in long format
  if (wide) {
    x <- datawizard::data_to_wide(x,
      values_from = "x",
      id_cols = "blocks",
      names_from = "groups"
    )
    x <- x[, -1]
  }
  x
}


.get_data_multivariate <- function(x, y, data = data, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L || length(x[[3]]) != 1L) {
      stop("Formula must have the form of 'DV1 + ... + DVk ~ group', with exactly one term on the RHS.", call. = FALSE)
    }

    data <- model.frame(formula = reformulate(as.character(x)[3:2]),
                        data = data)

    data <- split(data[,-1, drop = FALSE], f = data[[1]])
    if (length(data) != 2) {
      stop("~ group must have 2 levels exactly.", call. = FALSE)
    }
    x <- data[[1]]
    y <- data[[2]]
  }

  if (!(is.data.frame(x) && is.data.frame(y))) {
    stop("x,y or data must be data.frames.", call. = FALSE)
  }

  if (!all(colnames(x) == colnames(y))) {
    stop("x,y must have the same variables (in the same order)", call. = FALSE)
  }

  if (!all(c(sapply(x, is.numeric), sapply(y, is.numeric)))) {
    stop("All DVs must be numeric.", call. = FALSE)
  }

  .nlist(x, y)
}


# Helpers -----------------------------------------------------------------


#' @keywords internal
#' @importFrom stats model.frame
.resolve_formula <- function(formula, data, subset, na.action, ...) {
  cl <- match.call(expand.dots = FALSE)
  cl[[1]] <- quote(stats::model.frame)
  if ("subset" %in% names(cl)) {
    cl$subset <- substitute(subset)
  }
  cl$... <- NULL
  eval(cl, envir = parent.frame())
}

#' @keywords internal
.resolve_char <- function(nm, data) {
  if (is.character(nm) && length(nm) == 1L) {
    if (is.null(data)) {
      stop("Please provide data argument.", call. = FALSE)
    } else if (!nm %in% names(data)) {
      stop("Column ", nm, " missing from data.", call. = FALSE)
    }

    return(data[[nm]])
  }
  nm
}
