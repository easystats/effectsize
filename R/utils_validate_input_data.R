
#' @keywords internal
#' @importFrom stats terms
#' @importFrom stats delete.response
.get_data_2_samples <- function(x, y = NULL, data = NULL, verbose = TRUE, ...) {

  # Sanity checks
  if (((is.character(x) && length(x) == 1) ||
       (is.character(y) && length(y) == 1)) &&
      is.null(data)) {
    stop("Please provide data argument.")
  }

  ## Pull columns ----

  ### Formula ----
  if (inherits(x, "formula")) {
    if (length(x) != 3) {
      stop("Formula must be two sided.", call. = FALSE)
    }

    mf <- stats::model.frame(formula = x, data = data, ...)

    x <- mf[[1]]
    if (ncol(mf) == 1) {
      y <- NULL
    } else if (ncol(mf) == 2) {
      y <- mf[[2]]
    } else {
      stop("Formula must have only one term on the RHS.", call. = FALSE)
    }

    if (!is.null(y) && !is.factor(y)) y <- factor(y)
  }

  ### Character ----
  if (is.character(x)) {
    if (!x %in% names(data)) {
      stop("Column ", x, " missing from data.", call. = FALSE)
    }
    x <- data[[x]]
  }

  if (is.character(y) && length(y) == 1) {
    if (!y %in% names(data)) {
      stop("Column ", y, " missing from data.", call. = FALSE)
    }
    y <- data[[y]]
  }

  ## Validate x,y ----
  if (!is.numeric(x)) {
    stop("Cannot compute effect size for a non-numeric vector.", call. = FALSE)
  } else if (inherits(x, "Pair")) {
    x <- -apply(x, 1, diff)
  }

  # If y is a factor
  if (!is.null(y)) {
    if (!is.numeric(y)) {
      if (length(unique(y)) > 2) {
        stop("Grouping variable y has more that 2 levels.",
             call. = FALSE
        )
      }
      if (ifelse(inherits(x, "Pair"), nrow(x), length(x)) != length(y)) {
        stop("Grouping variable must be the same length.", call. = FALSE)
      }

      data <- split(x, y)
      data <- Filter(length, data)
      x <- data[[1]]
      y <- data[[2]]
    } else {
      # Only relevant when y is not a factor
      if (verbose && length(unique(y)) == 2) {
        warning(
          "'y' is numeric but has only 2 unique values. If this is a grouping variable, convert it to a factor.",
          call. = FALSE
        )
      }
    }
  }

  list(x = x, y = y)
}



#' @keywords internal
#' @importFrom stats model.frame
.get_data_multi_group <- function(x, groups, data, ...) {
  if (inherits(frm <- x, "formula")) {
    mf <- stats::model.frame(formula = frm, data = data, ...)

    if (length(frm) != 3 | ncol(mf) != 2) {
      stop("Formula must have the form of 'outcome ~ group'.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- factor(mf[[2]])
  } else if (inherits(x, "list")) {
    groups <- rep(seq_along(x), sapply(x, length))
    x <- unsplit(x, groups)
  } else if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
  } else if (length(x) != length(groups)) {
    stop("x and g must be of the same length.", call. = FALSE)
  }

  data.frame(x, groups)
}

#' @keywords internal
#' @importFrom stats model.frame reshape
.get_data_nested_groups <- function(x, groups, blocks, data = NULL, ...) {
  if (inherits(frm <- x, "formula")) {
    if ((length(frm) != 3L) ||
        (length(frm[[3L]]) != 3L) ||
        (frm[[3L]][[1L]] != as.name("|"))) {
      stop("Formula must have the 'x ~ groups | blocks'.", call. = FALSE)
    }

    frm[[3L]][[1L]] <- as.name("+")

    mf <- stats::model.frame(formula = frm, data = data, ...)

    if (ncol(mf) != 3) {
      stop("Formula must have only two terms on the RHS.", call. = FALSE)
    }

    x <- mf[[1]]
    groups <- mf[[2]]
    blocks <- mf[[3]]
  } else if (inherits(x, c("table", "matrix", "array", "data.frame"))) {
    data <- data.frame(
      x = c(x),
      groups = rep(factor(seq_len(ncol(x))),
                   each = nrow(x)
      ),
      blocks = rep(
        factor(seq_len(nrow(x))),
        ncol(x)
      )
    )

    x <- data[[1]]
    groups <- data[[2]]
    blocks <- data[[3]]
  } else if (is.character(x)) {
    x <- data[[x]]
    groups <- data[[groups]]
    blocks <- data[[blocks]]
  } else if (length(x) != length(groups) || length(x) != length(blocks)) {
    stop("x, groups and blocks must be of the same length.", call. = FALSE)
  }

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
