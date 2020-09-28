#' @keywords internal
.get_object <- function(x, attribute_name = "object_name") {
  obj_name <- attr(x, attribute_name, exact = TRUE)
  model <- NULL
  if (!is.null(obj_name)) {
    model <- tryCatch({
      get(obj_name, envir = parent.frame())
    }, error = function(e) {
      NULL
    })
    if (is.null(model) ||
        # prevent self reference
        inherits(model, "parameters_model")) {
      model <- tryCatch({
        get(obj_name, envir = globalenv())
      }, error = function(e) {
        NULL
      })
    }
  }
  model
}
