if (require("testthat") && require("effectsize")) {
  test_that("normalize", {
    x <- normalize(iris)
    testthat::expect_equal(mean(x$Sepal.Length), 0.43, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 0.635, tolerance = 0.01)

    testthat::skip_if_not_installed("dplyr")
    x <- normalize(dplyr::group_by(iris, .data$Species))
    testthat::expect_equal(mean(x$Sepal.Length), 0.509, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 0.562, tolerance = 0.01)
  })



  test_that("ranktransform", {
    x <- ranktransform(iris)
    testthat::expect_equal(mean(x$Sepal.Length), 75.5, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 114, tolerance = 0.01)

    testthat::skip_if_not_installed("dplyr")
    x <- ranktransform(dplyr::group_by(iris, .data$Species))
    testthat::expect_equal(mean(x$Sepal.Length), 25.5, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 25.5, tolerance = 0.01)
  })



  test_that("change_scale", {
    x <- change_scale(iris)
    testthat::expect_equal(mean(x$Sepal.Length), 42.9, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 63.6, tolerance = 0.01)

    testthat::skip_if_not_installed("dplyr")
    x <- change_scale(dplyr::group_by(iris, .data$Species))
    testthat::expect_equal(mean(x$Sepal.Length), 50.9, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 56.3, tolerance = 0.01)
  })


  test_that("adjust", {
    x <- adjust(iris)
    testthat::expect_equal(mean(x$Sepal.Length), 0, tolerance = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 0, tolerance = 0.01)
  })
}
