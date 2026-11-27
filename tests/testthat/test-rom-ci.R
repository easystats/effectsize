test_that("means_ratio one-sided CIs use log-scale bounds", {
  x <- c(1, 2, 3)
  y <- c(4, 5, 6)

  # Ratio scale: lower bound for alternative = "less" is 0 (exp(-Inf)), not 1
  res_less <- means_ratio(x, y, alternative = "less")
  expect_identical(res_less$CI_low, 0)
  expect_lt(res_less$CI_low, res_less[[1]])
  expect_lt(res_less[[1]], res_less$CI_high)

  res_greater <- means_ratio(x, y, alternative = "greater")
  expect_identical(res_greater$CI_high, Inf)
  expect_lt(res_greater$CI_low, res_greater[[1]])

  # Log scale: lower bound for alternative = "less" is -Inf, not 0
  res_less_log <- means_ratio(x, y, log = TRUE, alternative = "less")
  expect_identical(res_less_log$CI_low, -Inf)
  expect_lt(res_less_log$CI_low, res_less_log[[1]])
  expect_lt(res_less_log[[1]], res_less_log$CI_high)

  res_greater_log <- means_ratio(x, y, log = TRUE, alternative = "greater")
  expect_identical(res_greater_log$CI_high, Inf)
  expect_lt(res_greater_log$CI_low, res_greater_log[[1]])

  # Consistency between ratio and log scales for ratio > 1 as well
  d_less <- means_ratio(iris$Sepal.Length, iris$Sepal.Width, alternative = "less")
  d_less_log <- means_ratio(
    iris$Sepal.Length,
    iris$Sepal.Width,
    log = TRUE,
    alternative = "less"
  )
  expect_identical(d_less$CI_low, 0)
  expect_identical(d_less_log$CI_low, -Inf)
  expect_equal(log(d_less$CI_high), d_less_log$CI_high, tolerance = 1e-6)
})
