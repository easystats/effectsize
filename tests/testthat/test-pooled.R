test_that("sd_pooled", {
  expect_equal(sd_pooled(1:4, 1:3 * 5), 3.316625, tolerance = 0.001)
  expect_equal(mad_pooled(1:4, 1:3 * 5), 2.2239, tolerance = 0.001)

  expect_equal(sd_pooled(c(1:3, 40), 1:3 * 5), 15.06652, tolerance = 0.001)
  expect_equal(mad_pooled(c(1:3, 40), 1:3 * 5), 2.2239, tolerance = 0.001)

  x <- 1:5
  y <- 1:5
  expect_equal(mad_pooled(x, y), mad(c(x, y)))
  expect_equal(sd_pooled(x, y), sd(c(x, y)) * sqrt(9 / 8))
})
