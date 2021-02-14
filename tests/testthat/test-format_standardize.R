if (require("testthat") && require("effectsize")) {
  test_that("format_standardize", {
    set.seed(123)
    expect_equal(
      format_standardize(c(-1, 0, 1)),
      structure(3:1, .Label = c("+1 SD", "Mean", "-1 SD"), class = "factor")
    )

    set.seed(123)
    mod1 <- format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000))

    expect_equal(
      mod1,
      structure(4:1, .Label = c(
        "+2 SD", "+1e+00 SD", "-2e-02 SD",
        "-1 SD"
      ), class = "factor")
    )

    set.seed(123)
    mod2 <- format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE)

    expect_equal(
      mod2,
      structure(4:1, .Label = c("+2 MAD", "+1 MAD", "-1e-02 MAD", "-1 MAD"), class = "factor")
    )

    set.seed(123)
    mod3 <- format_standardize(c(-1, 0, 1, 2), reference = rnorm(1000), robust = TRUE, digits = 2)

    expect_equal(
      mod3,
      structure(4:1, .Label = c(
        "+2.06 MAD", "+1.03 MAD", "-9.55e-03 MAD",
        "-1.05 MAD"
      ), class = "factor")
    )
  })
}
