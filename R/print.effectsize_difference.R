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
  # note <- NULL
  # if (any(colnames(x) %in% c("Cohens_d", "Hedges_g"))) {
  #   note <- c(note, ifelse(attr(x, "pooled_sd"), "Pooled SD", "Unpooled SD"))
  # }
  # if (attr(x, "correction")) {
  #   note <- c(note, "Bias-corrected")
  # }
  # if (!is.null(note)) cat(c("\n[",paste0(note, collapse = "; ")), "]\n")

  if (append_CL && any(colnames(x) %in% c("Cohens_d", "Hedges_g"))) {
    # Common lang
    cl <- d_to_common_language(x[[any(colnames(x) %in% c("Cohens_d", "Hedges_g"))]])
    cl <- sapply(cl, function(ff) sprintf("%g%%", round(ff * 100, digits = digits)))
    cl <- paste(paste0("\n* ", names(cl), ": ", cl), collapse = "")

    footer <- c(cl, "cyan")
  }

  x <- .print_effectsize_table(x, digits = digits)
  cat(insight::export_table(x, digits = digits, caption = caption, footer = footer))

  invisible(x_orig)
}
