# library(testthat)

test_that("mahalanobis_d | vs cohens_d", {
  set.seed(456)
  x <- data.frame(A = bayestestR::distribution_normal(1000, sd = 2),
                  B = sample(bayestestR::distribution_normal(1000, sd = 33)),
                  C = sample(bayestestR::distribution_normal(1000, sd = 50)),
                  D = sample(bayestestR::distribution_normal(1000, sd = 17)))


  # Simple:
  y <- within(x, {B <- B + 15})
  D <- mahalanobis_d(x, y)
  d <- cohens_d(-x$B, -y$B)
  expect_equal(D[[1]], d[[1]], tolerance = 0.01)
  expect_equal(D[[3]], d[[3]], tolerance = 0.1)
  expect_equal(D[[4]], d[[4]], tolerance = 0.1)


  # Standardized:
  y2 <- within(y, {B <- A + B})
  d <- unlist(mapply(cohens_d, x, y2, MoreArgs = list(ci = NULL)))
  R <- cov2cor(cov_pooled(x, y2))
  expect_equal(c(sqrt(t(d) %*% R %*% d)),
               mahalanobis_d(x, y2, ci = NULL)[[1]], tolerance = 0.01)
})


test_that("mahalanobis_d | vs inputs", {
  # TODO
})

