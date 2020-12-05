#' @export
#' @rdname cohens_d
#' @inheritParams insight::format_value
#' @param append_CL Should the Common Language Effect Sizes be printed as well?
#'   Not applicable to Glass' Delta (See [d_to_common_language()])
#' @param ... Not used.
print.effectsize_difference <- function(x, digits = 2, append_CL = FALSE, ...) {
  x_orig <- x

  footer <- caption <- NULL

  ## Add footer
  if (any(colnames(x) %in% c("Cohens_d", "Hedges_g"))) {
    footer <- paste0(" - Estimate using ", ifelse(attr(x, "pooled_sd"), "pooled SD", "un-pooled SD"), "\n")
  }

  if (any(colnames(x) == "Hedges_g")) {
    correction <- paste0(" - Sample samle bias corrected using ",
                         ifelse(attr(x, "correction") == 1, "Hedges and Olkin's", "Hunter and Schmidt's"),
                         " correction.\n")
    footer <- paste0(footer, correction)
  }

  if (!is.null(footer)) footer <- c(footer, "cyan")

  x <- .print_effectsize_table(x, digits = digits)
  cat(insight::export_table(x, digits = digits, caption = caption, footer = footer))

  if (append_CL && any(colnames(x_orig) %in% c("Cohens_d", "Hedges_g")) && !attr(x_orig, "paired")) {
    # Common lang
    cl <- d_to_common_language(x_orig[[any(colnames(x_orig) %in% c("Cohens_d", "Hedges_g"))]])
    cl <- lapply(cl, insight::format_value, as_percent = TRUE, digits = digits)
    cl <- data.frame(cl, check.names = FALSE)
    cat(insight::export_table(cl, digits = digits, caption = c("\n\n# Common Language Effect Sizes", "blue")))
  }

  invisible(x_orig)
}
