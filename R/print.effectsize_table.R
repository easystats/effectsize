#' @export
print.effectsize_table <- function(x, digits = 2, ...){
  x_orig <- x

  if ("CI" %in% colnames(x)) {
    ci_level <- x$CI[1]

    x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL,
                               digits = digits, width = "auto")

    colnames(x)[colnames(x) == "CI"] <-
      paste0(insight::format_value(ci_level, as_percent = TRUE, digits = 0),
             " CI")


    x$CI_low <- x$CI_high <- NULL
  }

  cat(insight::format_table(x, digits = digits))

  return(invisible(x_orig))
}

