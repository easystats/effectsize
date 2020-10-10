if (require("testthat") && require("effectsize")) {
  test_that("t-test", {

    ## One sample
    htest <- t.test(mtcars$mpg - 15)
    testthat::expect_equal(effectsize::effectsize(htest)$d, 0.858, tol = 0.001)

    ## paired
    htest <- t.test(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
    testthat::expect_equal(effectsize::effectsize(htest)$d, 2.852, tol = 0.001)

    ## two sample
    htest <- t.test(mpg ~ am, mtcars, var.equal = TRUE)
    testthat::expect_equal(effectsize::effectsize(htest)$d, -1.499, tol = 0.001)
  })


  test_that("Chisq-test", {
    contingency_table <-
      as.table(rbind(c(760, 330, 470), c(480, 240, 480), c(480, 240, 480)))

    Xsq1 <- chisq.test(contingency_table)
    Xsq2 <- chisq.test(contingency_table/10)

    testthat::expect_equal(effectsize(Xsq1)$cramers_v,
                           effectsize(Xsq2)$cramers_v)

    testthat::expect_equal(
      effectsize(chisq.test(table(mtcars$cyl)))$cramers_v,
      cramers_v(table(mtcars$cyl))$cramers_v
    )
  })
}