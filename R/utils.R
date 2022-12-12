#' @keywords internal
".someattributes<-" <- function(x, value) {
  for (a in names(value)) {
    attr(x, a) <- value[[a]]
  }
  x
}

#' @keywords internal
.nlist <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))

  if (all(has_name)) {
    return(dots)
  }

  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}

#' @keywords internal
#' @importFrom insight model_info
.get_model_info <- function(model, model_info = NULL, ...) {
  if (is.null(model_info)) model_info <- insight::model_info(model)

  model_info
}



#' @keywords internal
.safe_ranktransform <- function(x, verbose = TRUE, ...) {
  if (insight::n_unique(x) == 1) {
    return(rep(mean(seq_along(x)), length(x)))
  }
  datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
}


#' @keywords internal
.is_BF_of_type <- function(x, type, msg = type) {
  if (inherits(x, "BFBayesFactor")) {
    if (!inherits(x@numerator[[1]], type)) {
      insight::format_error(sprintf("'x' is not a %s!", msg))
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @keywords internal
.is_htest_of_type <- function(x, pattern, msg) {
  if (inherits(x, "htest")) {
    if (!grepl(pattern, x$method)) {
      insight::format_error(sprintf("'x' is not a %s!", msg))
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# CI Utils ----------------------------------------------------------------

#' @keywords internal
.test_ci <- function(ci) {
  if (is.null(ci)) {
    return(FALSE)
  }
  if (!is.numeric(ci) ||
    length(ci) != 1L ||
    ci < 0 ||
    ci > 1) {
    insight::format_error("ci must be a single numeric value between (0, 1)")
  }
  return(TRUE)
}

#' @keywords internal
.adjust_ci <- function(ci, alternative) {
  if (alternative == "two.sided") {
    return(ci)
  }

  2 * ci - 1
}

#' @keywords internal
.limit_ci <- function(out, alternative, lb, ub) {
  if (alternative == "two.sided") {
    return(out)
  }

  if (alternative == "less") {
    out$CI_low <- lb
  } else if (alternative == "greater") {
    out$CI_high <- ub
  }

  out
}

#' @keywords internal
.match.alt <- function(alternative) {
  match.arg(alternative, c("two.sided", "less", "greater"))
}
