# library(testthat)

test_that("means_ratio", {
  # Direction ---------------------------------------------------------------
  rez_t <- t.test(iris$Sepal.Length, iris$Sepal.Width)
  rez_means_ratio <- means_ratio(iris$Sepal.Length, iris$Sepal.Width)
  expect_equal(sign(rez_t$statistic), sign(log(rez_means_ratio[[1]])),
    ignore_attr = TRUE
  )


  # Alternative -------------------------------------------------------------
  d1 <- means_ratio(iris$Sepal.Length, iris$Sepal.Width, ci = 0.80)
  d2 <- means_ratio(iris$Sepal.Length, iris$Sepal.Width, ci = 0.90, alternative = "l")
  d3 <- means_ratio(iris$Sepal.Length, iris$Sepal.Width, ci = 0.90, alternative = "g")
  expect_equal(d1$CI_high, d2$CI_high)
  expect_equal(d1$CI_low, d3$CI_low)
})

test_that("means_ratio - adjusted", {
  x <- means_ratio(wt ~ am, data = mtcars, adjust = TRUE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1.56, tolerance = 0.001)
  expect_equal(x$CI_low, 1.32, tolerance = 0.001)
  expect_equal(x$CI_high, 1.845, tolerance = 0.001)

  x <- means_ratio(
    x = subset(mtcars, am == 1)$wt,
    y = subset(mtcars, am == 0)$wt,
    adjust = TRUE
  )
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1 / 1.56, tolerance = 0.001)
  expect_equal(x$CI_high, 1 / 1.320838, tolerance = 0.001)
  expect_equal(x$CI_low, 1 / 1.844883, tolerance = 0.001)
})

test_that("means_ratio - not adjusted", {
  x <- means_ratio(wt ~ am, data = mtcars, adjust = FALSE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1.564, tolerance = 0.001)
  expect_equal(x$CI_low, 1.323, tolerance = 0.001)
  expect_equal(x$CI_high, 1.848, tolerance = 0.001)
})

test_that("means_ratio paired - adjusted", {
  data(sleep)
  expect_error(means_ratio(extra ~ group, data = sleep), "negative")

  sleep$y <- sleep$extra + 4
  x <- means_ratio(y ~ group,
    data = sleep,
    adjust = TRUE, paired = TRUE
  )
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 0.752, tolerance = 0.001)
  expect_equal(x$CI_low, 0.652, tolerance = 0.001)
  expect_equal(x$CI_high, 0.867, tolerance = 0.001)
})

test_that("means_ratio paired - not adjusted", {
  data(sleep)
  sleep$y <- sleep$extra + 4
  x <- means_ratio(y ~ group,
    data = sleep,
    adjust = FALSE, paired = TRUE
  )
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 0.75, tolerance = 0.001)
  expect_equal(x$CI_low, .651, tolerance = 0.001)
  expect_equal(x$CI_high, 0.865, tolerance = 0.001)
})
