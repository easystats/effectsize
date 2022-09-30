test_that("mahalanobis_d | two sample | vs cohens_d", {
  set.seed(456)
  x <- data.frame(
    A = bayestestR::distribution_normal(1000, sd = 2),
    B = sample(bayestestR::distribution_normal(1000, sd = 33)),
    C = sample(bayestestR::distribution_normal(1000, sd = 50)),
    D = sample(bayestestR::distribution_normal(1000, sd = 17))
  )


  # Simple:
  y <- within(x, {
    B <- B + 15
  })
  D <- mahalanobis_d(x, y)
  d <- cohens_d(-x$B, -y$B)
  expect_equal(D[[1]], d[[1]], tolerance = 0.01)
  expect_equal(D[[3]], d[[3]], tolerance = 0.1)
  expect_equal(D[[4]], d[[4]], tolerance = 0.1)
  expect_equal(D[[1]], sqrt(mahalanobis(rep(0, 4), colMeans(x) - colMeans(y), cov_pooled(x, y)))) # TRUE!


  # Standardized:
  y2 <- within(y, {
    B <- A + B
  })
  d <- unlist(mapply(cohens_d, x, y2, MoreArgs = list(ci = NULL)))
  R <- cov2cor(cov_pooled(x, y2))
  expect_equal(c(sqrt(t(d) %*% R %*% d)),
    mahalanobis_d(x, y2, ci = NULL)[[1]],
    tolerance = 0.01
  )
})



test_that("mahalanobis_d | one sample | vs cohens_d", {
  set.seed(456)
  x <- data.frame(
    A = bayestestR::distribution_normal(1000, sd = 2),
    B = sample(bayestestR::distribution_normal(1000, mean = 15, sd = 33)),
    C = sample(bayestestR::distribution_normal(1000, sd = 50)),
    D = sample(bayestestR::distribution_normal(1000, sd = 17))
  )


  # Simple:
  D <- mahalanobis_d(x)
  d <- cohens_d(x$B)
  expect_equal(D[[1]], d[[1]], tolerance = 0.01)
  expect_equal(D[[3]], d[[3]], tolerance = 0.1)
  expect_equal(D[[4]], d[[4]], tolerance = 0.1)
  expect_equal(D[[1]], sqrt(mahalanobis(rep(0, 4), colMeans(x), cov(x)))) # TRUE!

  # Standardized:
  d <- unlist(mapply(cohens_d, x, MoreArgs = list(ci = NULL)))
  R <- cor(x)
  expect_equal(c(sqrt(t(d) %*% R %*% d)),
    mahalanobis_d(x, ci = NULL)[[1]],
    tolerance = 0.01
  )
})


test_that("mahalanobis_d | inputs", {
  data("mtcars")

  D <- mahalanobis_d(mtcars[, c("mpg", "hp")])
  expect_equal(mahalanobis_d(cbind(mpg, hp) ~ 1, data = mtcars), D)
  expect_equal(mahalanobis_d(mpg + hp ~ 1, data = mtcars), D)

  D <- mahalanobis_d(
    mtcars[mtcars$am == 0, c("mpg", "hp")],
    mtcars[mtcars$am == 1, c("mpg", "hp")]
  )
  expect_equal(mahalanobis_d(cbind(mpg, hp) ~ am, data = mtcars), D)
  expect_equal(mahalanobis_d(mpg + hp ~ am, data = mtcars), D)

  mtcars$mpg[1] <- NA
  expect_warning(mahalanobis_d(mtcars[, c("mpg", "hp")]), regexp = "dropped")
  expect_warning(D1 <- mahalanobis_d(mpg + hp ~ 1, data = mtcars), regexp = "dropped")
  expect_equal(D1, mahalanobis_d(mpg + hp ~ 1, data = mtcars[-1, ]))
})


test_that("mahalanobis_d | mu types", {
  mu <- 0
  expect_error(D1 <- mahalanobis_d(mtcars[, c("mpg", "hp")], mu = mu), regexp = NA)

  mu <- 2
  expect_error(D2 <- mahalanobis_d(mtcars[, c("mpg", "hp")], mu = mu), regexp = NA)

  expect_false(D1[[1]] == D2[[1]])

  mu <- list(mpg = 3, hp = -14)
  expect_error(D3 <- mahalanobis_d(mtcars[, c("mpg", "hp")], mu = mu), regexp = NA)

  expect_equal(attr(D3, "mu"), dist(rbind(mu, 0)), ignore_attr = TRUE)

  mu <- c(mpg = 3, hp = -14)
  expect_error(mahalanobis_d(mtcars[, c("mpg", "hp")], mu = mu), regexp = NA)
})


test_that("mahalanobis_d | rotation", {
  set.seed(1235)
  m <- matrix(rnorm(1000, mean = 0.2), 200, 5)
  m2 <- m %*% solve(matrix(runif(25), 5, 5))

  expect_equal(D <- mahalanobis_d(m), mahalanobis_d(m2))

  PCA <- princomp(m)
  PCs <- sweep(PCA$scores, 2, PCA$center)
  expect_equal(mahalanobis_d(PCs), D, tolerance = 0.01)
})
