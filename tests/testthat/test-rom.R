
test_that("cohens_d errors and warnings", {
  # Direction ---------------------------------------------------------------
  rez_t <- t.test(iris$Sepal.Length, iris$Sepal.Width)
  rez_rom <- rom(iris$Sepal.Length, iris$Sepal.Width)
  expect_equal(sign(rez_t$statistic), sign(rez_rom$RR-1),
               ignore_attr = TRUE)


  # Alternative -------------------------------------------------------------
  d1 <- rom(iris$Sepal.Length, iris$Sepal.Width, ci = 0.80)
  d2 <- rom(iris$Sepal.Length, iris$Sepal.Width, ci = 0.90, alternative = "l")
  d3 <- rom(iris$Sepal.Length, iris$Sepal.Width, ci = 0.90, alternative = "g")
  expect_equal(d1$CI_high, d2$CI_high)
  expect_equal(d1$CI_low, d3$CI_low)
})

test_that("rom - adjusted", {
  x <- rom(wt ~ am, data = mtcars, adjust = TRUE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1.56, tolerance = 0.001)
  expect_equal(x$CI_low, 1.32, tolerance = 0.001)
  expect_equal(x$CI_high, 1.845, tolerance = 0.001)

  x <- rom(x = subset(mtcars, am == 1)$wt,
           y = subset(mtcars, am == 0)$wt,
           adjust = TRUE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1/1.56, tolerance = 0.001)
  expect_equal(x$CI_high, 1/1.320838, tolerance = 0.001)
  expect_equal(x$CI_low, 1/1.844883, tolerance = 0.001)
})

test_that("rom - not adjusted", {
  x <- rom(wt ~ am, data = mtcars, adjust = FALSE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 1.564, tolerance = 0.001)
  expect_equal(x$CI_low, 1.323, tolerance = 0.001)
  expect_equal(x$CI_high, 1.848, tolerance = 0.001)
})

test_that("rom paired - adjusted", {
  data(sleep)
  sleep$y = sleep$extra + 4
  x <- rom(y ~ group, data = sleep,
           adjust = TRUE, paired = TRUE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 0.752, tolerance = 0.001)
  expect_equal(x$CI_low, 0.652, tolerance = 0.001)
  expect_equal(x$CI_high, 0.867, tolerance = 0.001)
})

test_that("rom paired - not adjusted", {
  data(sleep)
  sleep$y = sleep$extra + 4
  x <- rom(y ~ group, data = sleep,
           adjust = FALSE, paired = TRUE)
  expect_equal(colnames(x)[1], "Means_Ratio")
  expect_equal(x[[1]], 0.75, tolerance = 0.001)
  expect_equal(x$CI_low, .651, tolerance = 0.001)
  expect_equal(x$CI_high, 0.865, tolerance = 0.001)
})


