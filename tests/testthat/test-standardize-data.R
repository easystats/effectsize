if (require("testthat") && require("effectsize") && require("dplyr") && require("rlang")) {

  # standardize.numeric -----------------------------------------------------
  test_that("standardize.numeric", {
    x <- standardize(seq(0, 1, length.out = 100))
    testthat::expect_equal(mean(x), 0, tol = 0.01)

    x <- standardize(seq(0, 1, length.out = 100), two_sd = TRUE)
    testthat::expect_equal(sd(x), 0.5, tol = 0.01)

    x <- standardize(seq(0, 1, length.out = 100), robust = TRUE)
    testthat::expect_equal(median(x), 0, tol = 0.01)

    x <- standardize(seq(0, 1, length.out = 100), robust = TRUE, two_sd = TRUE)
    testthat::expect_equal(mad(x), 0.5, tol = 0.01)

    testthat::expect_message(standardize(c(0, 0, 0, 1, 1)))
  })


  # standardize.data.frame --------------------------------------------------
  test_that("standardize.data.frame", {
    data(iris)
    x <- standardize(iris)
    testthat::expect_equal(mean(x$Sepal.Length), 0, tol = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(dplyr::filter(x, .data$Species == 'virginica')$Sepal.Length), 0.89, tol = 0.01)

    x <- standardize(dplyr::group_by(iris, Species))
    testthat::expect_equal(mean(x$Sepal.Length), 0, tol = 0.01)
    testthat::expect_length(levels(x$Species), 3)
    testthat::expect_equal(mean(dplyr::filter(x, .data$Species == 'virginica')$Sepal.Length), 0, tol = 0.01)
  })


  test_that("standardize.data.frame, NAs", {
    data(iris)
    set.seed(123)
    iris$Sepal.Width[sample(1:150, 10)] <- NA
    iris$Sepal.Length[sample(1:150, 10)] <- NA

    x <- standardize(iris)
    testthat::expect_equal(head(x$Sepal.Length), c(-0.9163, -1.1588, -1.4013, -1.5226, -1.0376, -0.5526), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width), c(0.9965, -0.1377, 0.316, 0.0891, 1.2233, 1.9038), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length), as.numeric(NA))

    x <- standardize(iris, two_sd = TRUE)
    testthat::expect_equal(head(x$Sepal.Length), c(-0.4582, -0.5794, -0.7007, -0.7613, -0.5188, -0.2763), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width), c(0.4982, -0.0689, 0.158, 0.0446, 0.6116, 0.9519), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length), as.numeric(NA))

    x <- standardize(dplyr::group_by(iris, .data$Species))
    testthat::expect_equal(head(x$Sepal.Length), c(0.2086, -0.3681, -0.9447, -1.233, -0.0797, 1.0735), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width), c(0.1441, -1.1586, -0.6375, -0.8981, 0.4047, 1.1863), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length), as.numeric(NA))
  })


  test_that("standardize.data.frame, apend", {
    data(iris)
    set.seed(123)
    iris$Sepal.Width[sample(1:150, 10)] <- NA
    iris$Sepal.Length[sample(1:150, 10)] <- NA

    x <- standardize(iris, append = TRUE)
    testthat::expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
                                          "Species", "Sepal.Length_z", "Sepal.Width_z", "Petal.Length_z",
                                          "Petal.Width_z"))
    testthat::expect_equal(head(x$Sepal.Length_z), c(-0.9163, -1.1588, -1.4013, -1.5226, -1.0376, -0.5526), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width_z), c(0.9965, -0.1377, 0.316, 0.0891, 1.2233, 1.9038), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length_z), as.numeric(NA))

    x <- standardize(iris, two_sd = TRUE, append = TRUE)
    testthat::expect_equal(head(x$Sepal.Length_z), c(-0.4582, -0.5794, -0.7007, -0.7613, -0.5188, -0.2763), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width_z), c(0.4982, -0.0689, 0.158, 0.0446, 0.6116, 0.9519), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length_z), as.numeric(NA))

    x <- standardize(dplyr::group_by(iris, .data$Species), append = TRUE)
    testthat::expect_equal(head(x$Sepal.Length_z), c(0.2086, -0.3681, -0.9447, -1.233, -0.0797, 1.0735), tol = 0.01)
    testthat::expect_equal(head(x$Sepal.Width_z), c(0.1441, -1.1586, -0.6375, -0.8981, 0.4047, 1.1863), tol = 0.01)
    testthat::expect_equal(mean(x$Sepal.Length_z), as.numeric(NA))
  })



  test_that("standardize.data.frame, weights", {
    x <- rexp(30)
    w <- rpois(30, 20) + 1

    expect_equal(sqrt(cov.wt(cbind(x,x), w)$cov[1,1]),
                 attr(standardize(x, weights = w),"scale"))
    expect_equal(standardize(x, weights = w),
                 standardize(data.frame(x), weights = w)$x)

    # name and vector give same results
    expect_equal(standardize(mtcars, exclude = "cyl", weights = mtcars$cyl),
                 standardize(mtcars, weights = "cyl"))

    if (require(dplyr)) {
      d <- dplyr::group_by(mtcars, am)
      expect_warning(standardize(d, weights = d$cyl))
    }

  })
}
