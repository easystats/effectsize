#' @keywords internal
#' @importFrom stats na.omit complete.cases
.get_data_2_samples <- function(x, y = NULL, data = NULL,
                                paired = FALSE, allow_ordered = FALSE,
                                verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    # Validate:
    if (length(x) != 3L) {
      insight::format_error(
        "Formula must have one of the following forms:",
        "\n\ty ~ group,\n\ty ~ 1,\n\tPair(x,y) ~ 1"
      )
    }

    # Pull columns
    mf <- .resolve_formula(x, data, ...)

    if (ncol(mf) > 2L) {
      insight::format_error("Formula must have only one term on the RHS.")
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


  # If x is ordered and allowed to be...
  if (allow_ordered && is.ordered(x)) {
    if (is.ordered(y)) {
      if (!isTRUE(all.equal(levels(y), levels(x)))) {
        insight::format_error("x and y are ordered, but do not have the same levels.")
      }
      y <- as.numeric(y)
    }

    x <- as.numeric(x)
  }

  # x should be a numeric vector or a Pair:
  if (!is.numeric(x)) {
    insight::format_error("Cannot compute effect size for a non-numeric vector.")
  } else if (inherits(x, "Pair")) {
    x <- x[, 1] - x[, 2]
    y <- NULL
  }


  # y should be NULL, numeric, or a factor:
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (insight::n_unique(y) != 2) {
        insight::format_error("Grouping variable y must have exactly 2 levels.")
      }

      if (length(x) != length(y)) {
        insight::format_error("Grouping variable must be the same length.")
      }

      data <- Filter(length, split(x, y))
      x <- data[[1]]
      y <- data[[2]]
    }

    if (verbose && insight::n_unique(y) == 2) {
      insight::format_warning(
        "'y' is numeric but has only 2 unique values.",
        "If this is a grouping variable, convert it to a factor."
      )
    }
  }

  if (verbose && (anyNA(x) || anyNA(y))) {
    insight::format_warning("Missing values detected. NAs dropped.")
  }

  if (paired && !is.null(y)) {
    o <- stats::complete.cases(x, y)
    x <- x[o]
    y <- y[o]
  } else {
    x <- stats::na.omit(x)
    y <- stats::na.omit(y)
  }


  list(x = x, y = y)
}


#' @keywords internal
.get_data_xtabs <- function(x, y = NULL, p = NULL) {
  # TODO dont rely on chisq.test
  res <- suppressWarnings(stats::chisq.test(x,
    y = y,
    p = p,
    correct = FALSE,
    rescale.p = TRUE,
    simulate.p.value = FALSE
  ))

  res[c("observed", "expected")]
}

#' @keywords internal
.get_data_multi_group <- function(x, groups, data = NULL,
                                  allow_ordered = FALSE,
                                  verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3) {
      insight::format_error("Formula must have the form of 'outcome ~ group'.")
    }

    mf <- .resolve_formula(x, data, ...)

    if (ncol(mf) != 2L) {
      insight::format_error("Formula must have only one term on the RHS.")
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
  if (allow_ordered && is.ordered(x)) {
    x <- as.numeric(x)
  }
  if (!is.numeric(x)) {
    insight::format_error("Cannot compute effect size for a non-numeric vector.")
  }

  # groups should be not numeric
  if (length(x) != length(groups)) {
    insight::format_error("x and groups must be of the same length.")
  }

  if (is.numeric(groups)) {
    insight::format_error("groups cannot be numeric.")
  }

  out <- data.frame(x, groups)
  if (verbose && anyNA(out)) {
    insight::format_warning("Missing values detected. NAs dropped.")
  }
  stats::na.omit(out)
}

#' @keywords internal
#' @importFrom stats reshape
.get_data_nested_groups <- function(x, groups = NULL, blocks = NULL, data = NULL,
                                    wide = TRUE, allow_ordered = FALSE,
                                    verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L ||
      x[[3L]][[1L]] != as.name("|")) {
      insight::format_error("Formula must have the 'x ~ groups | blocks'.")
    }

    x[[3L]][[1L]] <- as.name("+")

    x <- .resolve_formula(x, data, ...)

    if (ncol(x) != 3L) {
      insight::format_error("Formula must have only two term on the RHS.")
    }
  } else if (inherits(x, "data.frame")) {
    x <- as.matrix(x)
  } else if (!inherits(x, c("table", "matrix", "array"))) {
    x <- .resolve_char(x, data)
    groups <- .resolve_char(groups, data)
    blocks <- .resolve_char(blocks, data)

    if (length(x) != length(groups) || length(x) != length(blocks)) {
      insight::format_error("x, groups and blocks must be of the same length.")
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

  if (allow_ordered && is.ordered(x$x)) {
    x$x <- as.numeric(x$x)
  }
  if (!is.numeric(x$x)) {
    insight::format_error("Cannot compute effect size for a non-numeric vector.")
  }
  if (!is.factor(x$groups)) x$groups <- factor(x$groups)
  if (!is.factor(x$blocks)) x$blocks <- factor(x$blocks)


  if (verbose && anyNA(x)) {
    insight::format_warning("Missing values detected. NAs dropped.")
  }
  x <- stats::na.omit(x)

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

#' @keywords internal
#' @importFrom stats na.pass reformulate
.get_data_multivariate <- function(x, y = NULL, data = NULL,
                                   verbose = TRUE, ...) {
  if (inherits(x, "formula")) {
    if (length(x) != 3L || length(x[[3]]) != 1L) {
      insight::format_error("Formula must have the form of 'DV1 + ... + DVk ~ group', with exactly one term on the RHS.")
    }

    data <- .resolve_formula(stats::reformulate(as.character(x)[3:2]), data, ...)

    if (x[[3]] == 1) {
      # Then it is one sampled
      x <- data
    } else {
      data <- split(data[, -1, drop = FALSE], f = data[[1]])
      if (length(data) != 2) {
        insight::format_error("~ group must have 2 levels exactly.")
      }
      x <- data[[1]]
      y <- data[[2]]
    }

    if (ncol(x) == 1L && is.matrix(x[[1]])) {
      x <- x[[1]]
      y <- y[[1]]
    }
  }

  # x should be a data frame or matrix
  if (is.matrix(x)) {
    x <- as.data.frame(x)
  } else if (!is.data.frame(x)) {
    insight::format_error("x must be a data frame.")
  }

  if (!all(sapply(x, is.numeric))) {
    insight::format_error("All DVs must be numeric.")
  }


  # y should be null, a data frame or matrix
  if (!is.null(y)) {
    if (is.matrix(y)) {
      y <- as.data.frame(y)
    } else if (!is.data.frame(y)) {
      insight::format_error("y must be a data frame.")
    }

    if (!all(sapply(y, is.numeric))) {
      insight::format_error("All DVs must be numeric.")
    }

    if (!all(colnames(x) == colnames(y))) {
      insight::format_error("x,y must have the same variables (in the same order).")
    }
  }

  if (verbose && (anyNA(x) || anyNA(y))) {
    insight::format_warning("Missing values detected. NAs dropped.")
  }
  x <- stats::na.omit(x)
  y <- stats::na.omit(y)

  .nlist(x, y)
}


# Helpers -----------------------------------------------------------------


#' @keywords internal
#' @importFrom stats model.frame na.pass
.resolve_formula <- function(formula, data, subset, na.action = stats::na.pass, ...) {
  cl <- match.call(expand.dots = FALSE)
  cl[[1]] <- quote(stats::model.frame)

  if (!"na.action" %in% names(cl)) {
    cl$na.action <- quote(stats::na.pass)
  }

  if ("subset" %in% names(cl)) {
    cl$subset <- substitute(subset)
  }

  cl$... <- NULL
  eval.parent(cl)
}

#' @keywords internal
.resolve_char <- function(nm, data) {
  if (is.character(nm) && length(nm) == 1L) {
    if (is.null(data)) {
      insight::format_error("Please provide data argument.")
    }

    if (!nm %in% names(data)) {
      insight::format_error(sprintf("Column %s missing from data.", nm))
    }

    return(data[[nm]])
  }
  nm
}
