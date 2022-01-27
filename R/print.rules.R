#' @importFrom utils head tail
#' @export
print.rules <- function(x, ...) {
  orig_x <- x

  name <- attr(x, "rule_name")

  if (length(x$values) == length(x$labels)) {
    title_type <- "Values"

    df <- data.frame(
      Labels = x$labels,
      Values = x$values
    )

    out <- insight::export_table(df, align = "rl", sep = " ~ ")

  } else {
    title_type <- "Thresholds"

    if (isTRUE(attr(x, "right"))) {
      gLeft <- " < "
      gRight <- " <= "
    } else {
      gLeft <- " <= "
      gRight <- " < "
    }

    df <- data.frame(
      " " = c("", x$values),
      " " = c("", rep(gLeft, length(x$values))),
      Label = x$labels,
      " " = c(rep(gRight, length(x$values)), ""),
      " " = c(x$values, ""),
      check.names = FALSE
    )

    out <- insight::export_table(df, align = "rcccl", sep = " ")
  }

  insight::print_color(sprintf("# Reference %s (%s)\n\n", title_type, name), "blue")
  cat(out)
  invisible(orig_x)
}


#' @export
print.effectsize_interpret <- function(x, ...) {
  orig_x <- x

  name <- attr(attr(x, "rules"), "rule_name")
  attr(x, "rules") <- NULL

  class(x) <- class(x)[-1]
  print(x, ...)

  insight::print_color(paste0("(Rules: ", name, ")\n"), "blue")

  invisible(orig_x)
}
