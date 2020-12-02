# Rules ---------------------------------------------------------------


#' Interpretation Grid
#'
#' Create a container for interpretation rules of thumb. Usually used in conjunction with [interpret].
#'
#' @param values Vector of reference values (edges defining categories or
#'   critical values).
#' @param labels Labels associated with each category. If `NULL`, will try to
#'   infer it from `values` (if it is a named vector or a list), otherwise, will
#'   return the breakpoints.
#' @param name Name of the set of rules (stored as a 'rule_name' attribute).
#' @param right logical, for threshold-type rules, indicating if the thresholds
#'   themselves should be included in the interval to the right (lower values)
#'   or in the interval to the left (higher values).
#'
#'
#'
#' @seealso interpret
#'
#' @examples
#' rules(c(0.05), c("significant", "not significant"), right = FALSE)
#' rules(c(0.2, 0.5, 0.8), c("small", "medium", "large"))
#' rules(c("small" = 0.2, "medium" = 0.5), name = "Cohen's Rules")
#'
#' @export
rules <- function(values, labels = NULL, name = NULL, right = TRUE) {

  if (is.null(labels)) {
    if (is.list(values)) {
      values <- unlist(values)
    }
    if (is.null(names(values))) {
      labels <- values
    } else{
      labels <- names(values)
    }
  }

  # Sanity checks
  if (length(labels) < length(values)) {
    stop("There cannot be less labels than reference values!")
  } else if (length(labels) > length(values) + 1) {
    stop("Too many labels for the number of reference values!")
  }

  if (length(values) == length(labels) - 1) {
    if (is.unsorted(values)) {
      stop("Reference values must be sorted.")
    }
  } else {
    right <- NULL
  }

  # Store and return
  out <- list(
    values = values,
    labels = labels
  )

  if (is.null(name)) {
    attr(out, "rule_name") <- "Custom rules"
  } else{
    attr(out, "rule_name") <- name
  }

  attr(out, "right") <- right
  class(out) <- c("rules", "list")
  out
}




#' @rdname rules
#' @param x An arbitrary R object.
#' @export
is.rules <- function(x) inherits(x, "rules")




# Interpret ---------------------------------------------------------------



#' Generic function for interpretation
#'
#' Interpret a value based on a set of rules. See [rules()].
#'
#' @param x Vector of value break points (edges defining categories).
#' @param rules Set of [rules()].
#' @param ... Currently not used.
#' @inheritParams rules
#'
#' @seealso rules
#' @examples
#' rules_grid <- rules(c(0.01, 0.05), c("very significant", "significant", "not significant"))
#' interpret(0.001, rules_grid)
#' interpret(0.021, rules_grid)
#' interpret(0.08, rules_grid)
#' interpret(c(0.01, 0.005, 0.08), rules_grid)
#'
#' interpret(c(0.35, 0.15), c("small" = 0.2, "large" = 0.4), name = "Cohen's Rules")
#' interpret(c(0.35, 0.15), rules(c(0.2, 0.4), c("small", "medium", "large")))
#' @export
interpret <- function(x, ...) {
  UseMethod("interpret")
}


#' @rdname interpret
#' @export
interpret.numeric <- function(x, rules, name = attr(rules, "rule_name"), ...) {

  if (!inherits(rules, "rules")) {
    rules <- rules(rules)
  }

  if (length(x) > 1) {
    out <- sapply(x, .interpret, rules)
  } else {
    out <- .interpret(x, rules)
  }

  if (is.null(name)) {
    attr(out, "rule_name") <- "Custom rules"
  } else{
    attr(out, "rule_name") <- name
  }

  class(out) <- c("effectsize_interpret", class(out))
  out
}


#' @keywords internal
.interpret <- function(x, rules) {
  if (is.na(x)) return(NA)

  if (length(rules$values) == length(rules$labels)) {
    index <- which.min(abs(x - rules$values))
  } else{
    if (isTRUE(attr(rules, "right"))) {
      check <- x <= rules$values
    } else {
      check <- x < rules$values
    }

    if (any(check)) {
      index <- min(which(check))
    } else {
      index <- length(rules$labels)
    }
  }
  rules$labels[index]
}
