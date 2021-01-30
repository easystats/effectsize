if (require("testthat") && require("effectsize")) {
  test_that("signed rank", {
    x <- c(-1, 2, -3, 4)

    sr <- ranktransform(x, sign = TRUE)
    r <- ranktransform(x, sign = FALSE)

    expect_equal(sr, x) # unchanged
    expect_equal(r, c(2, 3, 1, 4))


    x <- c(1, -2, -2, 4, 0, 3, -14, 0)
    expect_warning(ranktransform(x, sign = TRUE))
    expect_true(all(is.na(suppressWarnings(ranktransform(x, sign = TRUE)[c(5, 8)]))))
  })
}
