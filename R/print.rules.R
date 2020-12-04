#' @importFrom utils head tail
#' @export
print.rules <- function(x, ...) {
  orig_x <- x

  name <- attr(x, "rule_name")

  if (length(x$values) == length(x$labels)) {
    df <- data.frame(setNames(as.list(x$values), x$labels), check.names = FALSE)
    df <- cbind("Label " = "Value ", df)
    cat(insight::export_table(df, caption = c(paste0("# Reference values (", name, ")"), "blue")))
  } else {
    if (isTRUE(attr(x, "right"))) {
      gLeft <- " <= "
      gRight <- " < "
    } else {
      gLeft <- " < "
      gRight <- " <= "
    }
    insight::print_color(paste0("# Reference thresholds (", name, ")\n\n"), "blue")
    cat(paste0(
      paste0(head(x$labels, -1), gLeft, x$values,
        collapse = gRight
      ),
      gRight,
      tail(x$labels, 1)
    ))
  }
  invisible(orig_x)
}


#' @export
print.effectsize_interpret <- function(x, ...) {
  orig_x <- x

  name <- attr(x, "rule_name")
  attr(x, "rule_name") <- NULL

  class(x) <- class(x)[-1]
  print(x, ...)

  insight::print_color(paste0("(Rules: ", name, ")\n"), "blue")

  invisible(orig_x)
}
