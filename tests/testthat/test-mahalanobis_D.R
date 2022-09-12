library(testthat)


test_that("mahalanobis_D", {
  skip()
  # should be equal
  S <- diag(c(1, 30))
  x <- MASS::mvrnorm(100, c(0, 0), S, empirical = TRUE) |> as.data.frame()
  y <- MASS::mvrnorm(100, c(0, 15), S, empirical = TRUE) |> as.data.frame()
  mahalanobis_D(x, y)
  cohens_d(-x$V2, -y$V2)

  # should be equal
  S[1,2] <- S[2,1] <- 3
  x <- MASS::mvrnorm(100, c(0, 0), S, empirical = TRUE) |> as.data.frame()
  y <- MASS::mvrnorm(100, c(0, 15), S, empirical = TRUE) |> as.data.frame()
  d <- colMeans(x) - colMeans(y)
  sqrt(t(d) %*% solve(cov_pooled(x, y)) %*% d)
  mahalanobis_D(x, y)

  # test input types
})

