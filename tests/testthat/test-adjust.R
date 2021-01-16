if (require("testthat") && require("effectsize")) {
  data(iris)
  test_that("adjust", {
    adj <- effectsize::adjust(iris[c("Sepal.Length", "Species")], multilevel = TRUE, bayesian = FALSE)
    expect_equal(
      head(adj$Sepal.Length),
      c(0.08698, -0.11302, -0.31302, -0.41302, -0.01302, 0.38698),
      tolerance = 1e-3
    )
  })
}
