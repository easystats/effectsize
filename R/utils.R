#' @keywords internal
".someattributes<-" <- function(x, value) {
  for (a in names(value)) {
    attr(x, a) <- value[[a]]
  }
  x
}

#' @keywords internal
.nlist <- function (...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))

  if (all(has_name)) return(dots)

  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}
