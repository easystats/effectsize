#' @importFrom utils head
#' @export
print.rules <- function(x, ...){
  orig_x <- x
  if(length(x$values) == length(x$labels)){
    insight::print_color("# Reference values\n\n", "blue")
    df <- data.frame(t(rep(NA, length(x$labels))))
    names(df) <- x$labels
    df[1, ] <- x$values
    cat(insight::format_table(df))
  } else{
    insight::print_color("# Reference thresholds\n\n", "blue")
    cat(paste0(head(x$labels, -1), " < ", x$values, " < ", x$labels[-1], collapse = " < "))
  }
  invisible(orig_x)
}