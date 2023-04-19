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

  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
  rom1 <- means_ratio(x, y)
  rom2 <- means_ratio(y, x)
  expect_equal(rom1[[1]], 1 / rom2[[1]], tolerance = 0.01)
  expect_equal(rom1$CI_high, 1 / rom2$CI_low, tolerance = 0.01)
  expect_equal(rom1$CI_low, 1 / rom2$CI_high, tolerance = 0.01)

  rom1 <- means_ratio(x, y, log = TRUE)
  rom2 <- means_ratio(y, x, log = TRUE)
  expect_equal(rom1[[1]], -rom2[[1]], tolerance = 0.01)
  expect_equal(rom1$CI_high, -rom2$CI_low, tolerance = 0.05)
  expect_equal(rom1$CI_low, -rom2$CI_high, tolerance = 0.05)

  expect_error(means_ratio(rep(0, 4), 1:4), regexp = "zero")
})

test_that("means_ratio - adjusted", {
  x <- means_ratio(wt ~ am, data = mtcars, adjust = TRUE)
  expect_equal(colnames(x)[1], "Means_ratio_adjusted")
  expect_equal(x[[1]], 1.56, tolerance = 0.001)
  expect_equal(x$CI_low, 1.32, tolerance = 0.001)
  expect_equal(x$CI_high, 1.845, tolerance = 0.001)

  x <- means_ratio(
    x = subset(mtcars, am == 1)$wt,
    y = subset(mtcars, am == 0)$wt,
    adjust = TRUE
  )
  expect_equal(colnames(x)[1], "Means_ratio_adjusted")
  expect_equal(x[[1]], 1 / 1.56, tolerance = 0.001)
  expect_equal(x$CI_high, 1 / 1.320838, tolerance = 0.001)
  expect_equal(x$CI_low, 1 / 1.844883, tolerance = 0.001)

  x2 <- means_ratio(
    x = subset(mtcars, am == 1)$wt,
    y = subset(mtcars, am == 0)$wt,
    adjust = TRUE,
    log = TRUE
  )

  expect_equal(x2[[1]], log(1 / 1.561023), tolerance = 0.001)
  expect_equal(x2$CI_high, log(1 / 1.320838), tolerance = 0.001)
  expect_equal(x2$CI_low, log(1 / 1.844883), tolerance = 0.001)
})

test_that("means_ratio - not adjusted", {
  x <- means_ratio(wt ~ am, data = mtcars, adjust = FALSE)
  expect_equal(colnames(x)[1], "Means_ratio")
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
  expect_equal(colnames(x)[1], "Means_ratio_adjusted")
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
  expect_equal(colnames(x)[1], "Means_ratio")
  expect_equal(x[[1]], 0.75, tolerance = 0.001)
  expect_equal(x$CI_low, .651, tolerance = 0.001)
  expect_equal(x$CI_high, 0.865, tolerance = 0.001)
})
