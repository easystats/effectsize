#' @export
#' @rdname cohens_d
#' @inheritParams insight::format_value
#' @param append_CL Should the Common Language Effect Sizes be printed as well?
#'   Not applicable to Glass' Delta (See [d_to_common_language()])
#' @param ... Not used.
print.effectsize_difference <- function(x, digits = 2, append_CL = FALSE, ...) {
  x_orig <- x
  mu <- attr(x, "mu", exact = TRUE)

  footer <- caption <- NULL

  ## Add footer
  if (mu != 0) {
    mu <- sprintf("\n- Deviation from a fifference of %s.", mu)
    footer <- c(footer, list(c(mu, "cyan")))
  }

  if (!is.null(sd_type <- attr(x, "pooled_sd", exact = TRUE))) {
    sd_type <- sprintf(
      "\n- Estimated using %s.",
      ifelse(sd_type, "pooled SD", "un-pooled SD")
    )

    footer <- c(footer, list(c(sd_type, "cyan")))
  }

  if (any(colnames(x) == "Hedges_g")) {
    correction <- sprintf(
      "\n- Bias corrected using %s method.",
      ifelse(attr(x, "correction") == 1, "Hedges and Olkin's", "Hunter and Schmidt's")
    )

    footer <- c(footer, list(c(correction, "cyan")))
  }

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
