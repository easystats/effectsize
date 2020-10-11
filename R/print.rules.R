#' @importFrom utils head tail
#' @export
print.rules <- function(x, ...){
  orig_x <- x

  name <- attr(x, "rule_name")

  if(length(x$values) == length(x$labels)){
    df <- as.data.frame(setNames(as.list(x$values), x$labels))
    insight::print_color(paste0("# Reference values (",name,")\n\n"), "blue")
    cat(insight::format_table(df))
  } else{
    insight::print_color(paste0("# Reference thresholds (",name,")\n\n"), "blue")
    cat(paste0(
      paste0(head(x$labels, -1), " < ", x$values,
             collapse = " < "),
      " < ",
      tail(x$labels, 1)
    ))
  }
  invisible(orig_x)
}


#' @export
print.effectsize_interpret <- function(x, ...){
  orig_x <- x

  name <- attr(x, "rule_name")
  attr(x, "rule_name") <- NULL

  class(x) <- class(x)[-1]
  print(x, ...)

  insight::print_color(paste0("\n(Rules: ", name, ")\n"), "blue")

  invisible(orig_x)
}