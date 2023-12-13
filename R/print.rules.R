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
print_md.rules <- function(x, value = "x", title = "`{.rn}`:", ...) {

  rule_name <- attr(x, "rule_name")
  title <- gsub("\\{\\.rn\\}", rule_name, title)

  values <- insight::format_value(x$values, digits = "signif3", protect_integers = TRUE)
  labels <- insight::format_capitalize(x$labels)

  if (length(labels) > length(values)) {
    right <- attr(x, "right")

    k <- length(labels)

    if (right) {
      l <- "<"
      r <- "<="
    } else {
      l <- "<="
      r <- "<"
    }

    first <- sprintf("\n- **%s %s %s** - %s", value, r, values[1], labels[1])
    last <- sprintf("\n- **%s %s %s** - %s", values[k-1], l, value, labels[k])

    nth <- ""
    if (k > 2L) {
      nth <- sprintf("\n- **%s %s %s %s %s** - %s",
                     values[1:(k-2)], l, value, r, values[2:(k-1)],
                     labels[2:(k-1)])
      nth <- paste0(nth, collapse = "")
    }

    fmt_rules <- paste0(c(first, nth, last), collapse = "")
  } else {
    fmt_rules <- sprintf("\n- **%s =~ %s** - %s",
                         value, values, labels)
    fmt_rules <- paste0(fmt_rules, collapse = "")
  }

  cat("\n")
  cat(title)
  cat(fmt_rules)
  cat("\n")

  invisible(x)
}

#' @export
print_md.rules_list <- function(x, value = "x", title = "`{.rn}`:", ...) {
  .mapply(print_md, list(x = x, title = title), MoreArgs = list(value = value))
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
format.rules <- function(x, digits = "signif3", output = "text", ...) {
  name <- attr(x, "rule_name")

  V <- insight::format_value(x$values, digits = digits, protect_integers = TRUE, ...)
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