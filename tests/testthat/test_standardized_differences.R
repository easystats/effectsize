if (require("testthat") && require("effectsize")) {
  test_that("cohens_d", {
    rez_t <- t.test(iris$Sepal.Length, iris$Sepal.Width)
    rez_d <- cohens_d(iris$Sepal.Length, iris$Sepal.Width)
    testthat::expect_true(sign(rez_t$statistic) == sign(rez_d$Cohens_d))
  })
}