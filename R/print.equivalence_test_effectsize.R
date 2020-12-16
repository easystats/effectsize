#' @export
print.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  x_orig <- x

  caption <- footer <- subtitle <- NULL

  ## Title (caption)
  if (attr(x, "rule", exact = TRUE) == "cet") {
    caption <- "# Conditional Test for Practical Equivalence\n"
  } else {
    caption <- "# Test for Practical Equivalence\n"
  }
  caption <- c(caption, "blue")

  ## Rope range
  .rope <- attr(x, "rope", exact = TRUE)
  subtitle <- sprintf("  ROPE: [%.*f %.*f]", digits, .rope[1], digits, .rope[2])


  ## ROPE_Equivalence
  if (attr(x, "rule", exact = TRUE) == "bayes") {
    footer <- c("\n(Using Bayesian guidlines)", "green")
  }

  colnames(x)[colnames(x) == "ROPE_Equivalence"] <- "H0"
  x <- .print_effectsize_table(x)



  cat(insight::export_table(x,
    digits = digits,
    caption = caption, footer = footer, subtitle = subtitle
  ))

  invisible(x_orig)
}
