if (require("testthat") && require("effectsize")) {
  test_that("signed rank", {
    x <- c(-1,2,-3,4)

    sr <- effectsize::ranktransform(x, sign = TRUE)
    r <- effectsize::ranktransform(x, sign = FALSE)

    testthat::expect_equal(sr, x) # unchanged
    testthat::expect_equal(r, c(2, 3, 1, 4))
  })
}