#' @export
#' @rdname cohens_d
#' @param append_CL Should the Common Language Effect Sizes be printed as well?
#'   Not applicable to Glass' Delta (See [d_to_common_language()])
#' @param ... Not used.
print.effectsize_difference <- function(x, digits = 2, append_CL = FALSE, ...) {
  x_orig <- x
  print.effectsize_table(x, digits = digits)

  if (append_CL && ("Cohens_d" %in% colnames(x) || "Hedges_g" %in% colnames(x))) {
    cl <- d_to_common_language(x$Cohens_d)
    cl <- sapply(cl, insight::format_value, digits = digits, as_percent = TRUE)
    cl <- paste(paste0("* ", names(cl),": ",cl), collapse = "\n")
    cat("\n")
    insight::print_color(cl, "cyan")
  }

  invisible(x_orig)
}