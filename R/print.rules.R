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
print_html.rules <- function(x, digits = "signif2", ...) {
  x_fmt <- format(x, digits = digits, output = "html", ...)

  if (length(x$values) == length(x$labels)) {
    insight::export_table(x_fmt, align = "rl", format = "html", ...)
  } else {
    insight::export_table(x_fmt, align = "rcl", format = "html", ...)
  }
}


#' @export
print_md.rules <- function(x, digits = "signif2", value_name = NULL, title = NULL, ...) {
  name <- attr(x, "rule_name")
  V <- insight::format_value(x$values, digits = digits)
  labs <- x$labels
  if (is.null(value_name)) value_name <- "x"

  if (length(V) == length(labs)) {
    out <- paste0("\n- **",
                  value_name, " ~= ", V, "**",
                  " -> ", labs)
  } else {
    is_right <- isTRUE(attr(x, "right"))
    if (is_right) {
      R <- " <= "
      L <- " < "
    } else {
      R <- " < "
      L <- " <= "
    }
    R <- c("", rep(R, length(labs)-1))
    L <- c(rep(L, length(labs)-1), "")


    out <- paste0("\n- **",
                  c("", V),
                  R, value_name, L,
                  c(V, ""), "**",
                  " ~ ", labs)
  }

  if (is.null(title)) {
    header <- sprintf('\n\n`"%s"`:', name)
  } else {
    header <- sprintf('\n\n%s (`"%s"`):', title, name)
  }

  cat(header, out)

  invisible(x)
}


#' @export
format.rules <- function(x, digits = "signif2", output = "text", ...) {
  name <- attr(x, "rule_name")

  V <- insight::format_value(x$values, digits = digits, ...)
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
    if (length(L) != length(V)) {
      colnames(out)[c(1, 3)] <- " "
    }
    caption <- c(
      sprintf("# Reference %s (%s)", title_type, name),
      .pcl["interpret"]
    )
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
