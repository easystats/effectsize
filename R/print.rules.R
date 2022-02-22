#' @export
print.rules <- function(x, digits = "signif2", ...) {
  x_fmt <- format(x, digits = digits, ...)

  if (length(x$values) == length(x$labels)) {
    cat(insight::export_table(x_fmt, align = "rl", format = NULL, sep = " ~ ", ...))
  } else {
    cat(insight::export_table(x_fmt, align = "rcl", format = NULL, sep = " ", ...))
  }
  invisible(x)
}


#' @export
print_md.rules <- function(x, digits = "signif2", ...) {
  x_fmt <- format(x, digits = digits, output = "markdown", ...)

  if (length(x$values) == length(x$labels)) {
    insight::export_table(x_fmt, align = "rl", format = "markdown", ...)
  } else {
    insight::export_table(x_fmt, align = "rcl", format = "markdown", ...)
  }
}


#' @export
print_html.rules <- function(x, digits = "signif2", ...) {
  x_fmt <- format(x, digits = digits, output = "html", ...)

  if (length(x$values) == length(x$labels)) {
    insight::export_table(x_fmt, align = "rl", format = "html", ...)
  } else {
    insight::export_table(x_fmt, align = "rcl", format = "html", ...)
  }
}



#' @export
format.rules <- function(x, digits = "signif2", output = "text", ...) {
  name <- attr(x, "rule_name")

  V <- insight::format_value(x$values, ...)
  V <- insight::format_value(V, width = max(nchar(V)), ...)
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
      gLeft <- " <"
      gRight <- "<= "
    } else {
      gLeft <- " <="
      gRight <- "< "
    }

    out <- data.frame(
      Lower = paste0(c("", V), c("", rep(gLeft, length(V)))),
      Label = L,
      Upper = paste0(c(rep(gRight, length(V)), ""), c(V, "")),
      check.names = FALSE
    )
  }

  if (output == "text") {
    colnames(out)[c(1,3)] <- " "
    caption <- c(sprintf("# Reference %s (%s)", title_type, name),
                 .pcl["interpret"])
  } else {
    caption <- sprintf("Reference %s (%s)", title_type, name)
  }

  attr(out, "table_caption") <- caption

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
