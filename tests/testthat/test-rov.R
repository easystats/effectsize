test_that("variance_ratio | independent", {
  x <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
  y <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)

  rov1 <- variance_ratio(x, y)
  rov2 <- variance_ratio(y, x)

  expect_equal(1 / rov2[[1]], rov1[[1]])
  expect_equal(1 / rov2$CI_low, rov1$CI_high)
  expect_equal(1 / rov2$CI_high, rov1$CI_low)

  lrov1 <- variance_ratio(x, y, log = TRUE)
  lrov2 <- variance_ratio(y, x, log = TRUE)

  expect_equal(-lrov2[[1]], lrov1[[1]])
  expect_equal(-lrov2$CI_low, lrov1$CI_high)
  expect_equal(-lrov2$CI_high, lrov1$CI_low)

  expect_error(variance_ratio(x), regexp = "missing")
})


test_that("variance_ratio | paired", {
  sleep2 <- reshape(sleep,
    direction = "wide",
    idvar = "ID", timevar = "group"
  )
  pdat1 <- Pair(sleep2$extra.1, sleep2$extra.2)
  pdat2 <- Pair(sleep2$extra.2, sleep2$extra.1)

  rov1 <- variance_ratio(pdat1)
  rov2 <- variance_ratio(pdat2)

  expect_equal(1 / rov2[[1]], rov1[[1]])
  expect_equal(1 / rov2$CI_low, rov1$CI_high)
  expect_equal(1 / rov2$CI_high, rov1$CI_low)
})
