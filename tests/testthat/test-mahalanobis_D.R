library(testthat)


test_that("mahalanobis_D", {
  set.seed(456)
  # should be equal
  x <- data.frame(A = bayestestR::distribution_normal(1000, sd = 2),
                  B = sample(bayestestR::distribution_normal(1000, sd = 33)),
                  C = sample(bayestestR::distribution_normal(1000, sd = 50)),
                  C = sample(bayestestR::distribution_normal(1000, sd = 17)))
  y <- within(x, {B <- B + 15})

  D <- mahalanobis_D(x, y)
  d <- cohens_d(-x$B, -y$B)
  expect_equal(D[[1]], d[[1]], tolerance = 0.01)
  expect_equal(D[[3]], d[[3]], tolerance = 0.1)
  expect_equal(D[[4]], d[[4]], tolerance = 0.1)


  # should be equal
  x2 <- within(x, {B <- scale(3*A + B) * 30})
  d <- colMeans(y) - colMeans(x2)
  expect_equal(c(sqrt(t(d) %*% solve(cov_pooled(x2, y)) %*% d)),
               mahalanobis_D(x2, y, ci = NULL)[[1]], tolerance = 0.01)

  # test input types
  # TODO
})