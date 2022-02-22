#' @export
print.rules <- function(x, digits = "signif2", ...) {
  x_fmt <- format(x, digits = digits, ...)

  if (length(x$values) == length(x$labels)) {
    cat(insight::export_table(x_fmt, align = "rl", format = NULL, sep = " ~ ", ...))
  } else {
    cat(insight::export_table(x_fmt, align = "rcccl", format = NULL, sep = " ", ...))
  }
  invisible(x)
}


#' @importFrom utils head tail
#' @export
format.rules <- function(x, ...) {
  name <- attr(x, "rule_name")

  V <- insight::format_value(x$values, ...)
  L <- x$labels

  if (length(V) == length(L)) {
    title_type <- "Values"

    out <- data.frame(
      Labels = L,
      Values = V
    )
  } else {
    title_type <- "Thresholds"

    if (isTRUE(attr(x, "right"))) {
      gLeft <- " < "
      gRight <- " <= "
    } else {
      gLeft <- " <= "
      gRight <- " < "
    }

    out <- data.frame(
      " " = c(NA, V),
      " " = c(NA, rep(gLeft, length(V))),
      Label = L,
      " " = c(rep(gRight, length(V)), NA),
      " " = c(V, NA),
      check.names = FALSE
    )
  }

  attr(out, "table_caption") <-
    c(sprintf("# Reference %s (%s)\n\n", title_type, name), .pcl["interpret"])
  out
}


#' @export
print.effectsize_interpret <- function(x, ...) {
  orig_x <- x

  name <- attr(attr(x, "rules"), "rule_name")
  attr(x, "rules") <- NULL

  class(x) <- class(x)[-1]
  print(x, ...)

  insight::print_color(paste0("(Rules: ", name, ")\n"), .pcl["interpret"])

  invisible(orig_x)
}
