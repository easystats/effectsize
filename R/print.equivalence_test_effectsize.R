#' @export
print.equivalence_test_effectsize <- function(x, digits = 2, ...) {
  x_orig <- x

  ## Title
  if (attr(x, "rule", exact = TRUE) == "cet") {
    title <- "# Conditional Test for Practical Equivalence\n\n"
  } else {
    title <- "# Test for Practical Equivalence\n\n"
  }
  insight::print_color(title, "blue")

  ## Rope range
  .rope <- attr(x, "rope", exact = TRUE)
  cat(sprintf("  ROPE: [%.*f %.*f]\n\n", digits, .rope[1], digits, .rope[2]))


  ## ROPE_Equivalence
  colnames(x)[colnames(x) == "ROPE_Equivalence"] <- "H0"

  print.effectsize_table(x)

  if (attr(x, "rule", exact = TRUE) == "bayes") {
    insight::print_color("\n(Using Bayesian guidlines)\n\n", "green")
  }

  invisible(x_orig)
}