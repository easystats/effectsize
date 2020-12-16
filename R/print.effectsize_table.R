#' @export
print.effectsize_table <- function(x, digits = 2, ...) {
  x_orig <- x

  footer <- caption <- NULL

  ## Footer
  ## MSB: Move to own printing function?
  if (!is.null(std_method <- attr(x_orig, "std_method"))) {
    std_method <- sprintf(
      "\n# Standardization method: %s\n",
      std_method
    )
    footer <- list(c(std_method, "blue"))
  }

  x <- .print_effectsize_table(x, digits = digits)

  cat(insight::export_table(x, digits = digits, caption = caption, footer = footer))

  invisible(x_orig)
}

#' @keywords internal
.print_effectsize_table <- function(x, digits = 2) {
  i <- is_effectsize_name(colnames(x))
  colnames(x)[i] <- es_info$label[es_info$name == colnames(x)[i]]

  if ("CI" %in% colnames(x)) {
    ci_level <- x$CI[1]

    x$CI <- insight::format_ci(x$CI_low,
      x$CI_high,
      ci = NULL,
      digits = digits,
      width = "auto"
    )

    colnames(x)[colnames(x) == "CI"] <- sprintf("%g%% CI", round(ci_level * 100, digits = digits))

    x$CI_low <- x$CI_high <- NULL
  }

  x
}
