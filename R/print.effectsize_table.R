#' @export
print.effectsize_table <- function(x, digits = 2, ...){
  x_orig <- x

  i <- is_effectsize_name(colnames(x))
  colnames(x)[i] <- es_info$label[es_info$name == colnames(x)[i]]

  if ("CI" %in% colnames(x)) {
    ci_level <- x$CI[1]

    x$CI <- insight::format_ci(x$CI_low,
                               x$CI_high,
                               ci = NULL,
                               digits = digits,
                               width = "auto")

    colnames(x)[colnames(x) == "CI"] <- sprintf("%g%% CI", round(ci_level * 100, digits = digits))

    x$CI_low <- x$CI_high <- NULL
  }

  cat(insight::export_table(x, digits = digits))

  ## MSB: Move to own printing function?
  if (!is.null(method <- attr(x_orig, "std_method"))) {
    method <- paste0(toupper(substr(method, 1L, 1L)), substr(method, 2L, nchar(method)))
    insight::print_color(color = "blue", paste0("\n# Standardization method: ", method, "\n"))
  }

  invisible(x_orig)
}

