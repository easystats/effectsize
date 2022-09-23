
test_that("format_standardize", {
  expect_equal(
    format_standardize(c(-1, 0, 1), digits = 0),
    structure(3:1, .Label = c("+1 SD", "Mean", "-1 SD"), class = "factor")
  )


  skip_if_not_installed("bayestestR")
  ref <- bayestestR::distribution_normal(1000)

  expect_equal(
    format_standardize(c(-1, 0, 1, 2), reference = ref, digits = 0),
    structure(4:1,
              .Label = c("+2 SD", "+1 SD", "Mean", "-1 SD"),
              class = "factor"
    )
  )

  expect_equal(
    format_standardize(c(-1, 0, 1, 2), reference = ref, robust = TRUE, digits = 0),
    structure(4:1,
              .Label = c("+2 MAD", "+1 MAD", "Median", "-1 MAD"),
              class = "factor"
    )
  )

  expect_equal(
    format_standardize(c(-1, 0, 1, 2), reference = ref, robust = TRUE, digits = 2, protect_integers = FALSE),
    structure(4:1,
              .Label = c("+2.00 MAD", "+1.00 MAD", "Median", "-1.00 MAD"),
              class = "factor"
    )
  )
})
