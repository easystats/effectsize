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

    colnames(x)[colnames(x) == "CI"] <-
      paste0(insight::format_value(ci_level, as_percent = TRUE, digits = 0),
             " CI")

    x$CI_low <- x$CI_high <- NULL
  }

  cat(insight::format_table(x, digits = digits))

  invisible(x_orig)
}

