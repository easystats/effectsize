# Rules ---------------------------------------------------------------


#' Interpretation Grid
#'
#' Create a container for interpretation rules of thumb. Usually used in conjunction with \link{interpret}.
#'
#' @param values Vector of reference values (edges defining categories or critical values).
#' @param labels Labels associated with each category. If \code{NULL}, will try to infer it from \code{values} (if it is a named vector or a list), otherwise, will return the breakpoints.
#'
#'
#' @seealso interpret
#'
#' @examples
#' rules(c(0.05), c("significant", "not significant"))
#' rules(c(0.2, 0.5, 0.8), c("small", "medium", "large"))
#' rules(c("small" = 0.2, "medium" = 0.5))
#' @export
rules <- function(values, labels = NULL) {

  if(is.null(labels)){
    if(is.list(values)){
      values <- unlist(values)
    }
    if(is.null(names(values))){
      labels <- values
    } else{
      labels <- names(values)
    }
  }

  # Sanity checks
  if(length(labels) < length(values)){
    stop("There cannot be less labels than reference values!")
  } else if(length(labels) > length(values) + 1){
    stop("Too many labels for the number of reference values!")
  }

  if (length(values) == length(labels) - 1) {
    if (is.unsorted(values)){
      stop("Reference values must be sorted.")
    }
  }

  # Store and return
  out <- list(
    values = values,
    labels = labels
  )
  class(out) <- c("rules", "list")
  out
}




#' @rdname rules
#' @param x An arbitrary R object.
#' @export
is.rules <- function(x) inherits(x, "rules")



#' @importFrom utils head
#' @export
print.rules <- function(x, ...){

  if(length(x$values) == length(x$labels)){
    insight::print_color("# Reference values\n\n", "blue")
    df <- data.frame(t(rep(NA, length(x$labels))))
    names(df) <- x$labels
    df[1, ] <- x$values
    cat(insight::format_table(df))
  } else{
    insight::print_color("# Reference thresholds\n\n", "blue")
    cat(paste0(head(x$labels, -1), " < ", x$values, " < ", x$labels[-1], collapse = " < "))
  }
}








# Interpret ---------------------------------------------------------------



#' Generic function for interpretation
#'
#' Interpret a value based on a set of rules. See \link{rules}.
#'
#' @param x Vector of value break points (edges defining categories).
#' @param rules Set of \link{rules}.
#'
#' @seealso rules
#' @examples
#' rules_grid <- rules(c(0.01, 0.05), c("very significant", "significant", "not significant"))
#' interpret(0.001, rules_grid)
#' interpret(0.021, rules_grid)
#' interpret(0.08, rules_grid)
#' interpret(c(0.01, 0.005, 0.08), rules_grid)
#'
#' interpret(c(0.35, 0.15), c("small" = 0.2, "large" = 0.4))
#' interpret(c(0.35, 0.15), rules(c(0.2, 0.4), c("small", "medium", "large")))
#' @export
interpret <- function(x, rules) {

  if(!inherits(rules, "rules")){
    rules <- rules(rules)
  }

  if (length(x) > 1) {
    return(sapply(x, .interpret, rules))
  } else {
    return(.interpret(x, rules))
  }
}




#' @keywords internal
.interpret <- function(x, rules) {
  if(length(rules$values) == length(rules$labels)){
    index <- which.min(abs(x - rules$values))
  } else{
    check <- x < rules$values
    if (TRUE %in% check) {
      index <- min(which(check))
    } else {
      index <- length(rules$labels)
    }
  }
  rules$labels[index]
}
