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
.get_object <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- tryCatch(
      {
        get(obj_name, envir = parent.frame())
      },
      error = function(e) {
        NULL
      }
    )
    if (is.null(model) ||
      # prevent self reference
      inherits(model, "parameters_model")) {
      model <- tryCatch(
        {
          get(obj_name, envir = globalenv())
        },
        error = function(e) {
          NULL
        }
      )
    }
  }
  model
}


.safe_ranktransform <- function(x, verbose = TRUE, ...) {
  if (insight::n_unique(x) == 1) {
    return(rep(mean(seq_along(x)), length(x)))
  }
  datawizard::ranktransform(x, method = "average", ..., verbose = FALSE)
}
