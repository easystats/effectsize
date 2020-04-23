#' @export
print.equivalence_test_effectsize <- function(x, digits = 2, ...){
  x_orig <- x

  insight::print_color("# Test for Practical Equivalence\n\n", "blue")

  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))

  # ROPE_Equivalence
  x$H0 <- x$ROPE_Equivalence
  x$ROPE_Equivalence <- NULL

  # CI
  ci_level <- x$CI[1]
  # x$CI <- NULL
  x$CI <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL,
                             digits = digits, width = "auto")
  colnames(x)[colnames(x) == "CI"] <-
    paste0(insight::format_value(ci_level, as_percent = TRUE, digits = 0),
           " CI")
  x$CI_low <- x$CI_high <- NULL

  # print
  cat(insight::format_table(x, digits = digits))

  invisible(x_orig)
}