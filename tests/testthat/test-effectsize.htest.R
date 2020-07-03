if (require("testthat") && require("effectsize")) {
  test_that("t-test", {

    ## On samples
    htest <- t.test(mtcars$mpg - 15)
    testthat::expect_equal(effectsize::effectsize(htest)$d, 0.858, tol = 0.001)

    ## paired
    htest <- t.test(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
    testthat::expect_equal(effectsize::effectsize(htest)$d, 2.852, tol = 0.001)

    ## two sample
    htest <- t.test(mpg ~ am, mtcars, var.equal = TRUE)
    testthat::expect_equal(effectsize::effectsize(htest)$d, -1.499, tol = 0.001)
  })
}