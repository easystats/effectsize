#' @importFrom utils head
#' @export
print.rules <- function(x, ...){
  orig_x <- x
  if(length(x$values) == length(x$labels)){
    df <- as.data.frame(setNames(as.list(x$values), x$labels))
    insight::print_color("# Reference values\n\n", "blue")
    cat(insight::format_table(df))
  } else{
    insight::print_color("# Reference thresholds\n\n", "blue")
    cat(paste0(
      paste0(head(x$labels, -1), " < ", x$values,
             collapse = " < "),
      " < ",
      tail(x$labels, 1)
    ))
  }
  invisible(orig_x)
}