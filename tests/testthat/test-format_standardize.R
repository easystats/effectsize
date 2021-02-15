if (require("testthat") && require("effectsize")) {
  test_that("format_standardize", {
    expect_equal(
      format_standardize(c(-1, 0, 1)),
      structure(3:1, .Label = c("+1 SD", "Mean", "-1 SD"), class = "factor")
    )


    skip_if_not_installed("bayestestR")
    ref <- bayestestR::distribution_normal(1000)

    expect_equal(
      format_standardize(c(-1, 0, 1, 2), reference = ref),
      structure(4:1, .Label = c("+2 SD", "+1 SD", "Mean", "-1 SD"),
                class = "factor")
    )

    expect_equal(
      format_standardize(c(-1, 0, 1, 2), reference = ref, robust = TRUE),
      structure(4:1, .Label = c("+2 MAD", "+1 MAD", "Median", "-1 MAD"),
                class = "factor")
    )

    expect_equal(
      format_standardize(c(-1, 0, 1, 2), reference = ref, robust = TRUE, digits = 2),
      structure(4:1, .Label = c("+2.00 MAD", "+1.00 MAD", "Median", "-1.00 MAD"),
                class = "factor")
    )
  })
}
