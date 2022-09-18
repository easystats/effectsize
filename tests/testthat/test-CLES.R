# library(testthat)

test_that("CLES", {
  x <- 1:3
  y <- c(1, 1:3)
  # CV from https://rpsychologist.com/cohend/
  expect_equal(p_superiority(x, y)[[1]], 0.5719, tolerance = 0.001)
  expect_equal(cohens_u3(x, y)[[1]], 0.6012, tolerance = 0.001)
  expect_equal(p_overlap(x, y)[[1]], 0.8979, tolerance = 0.001)

  x <- c(-1,0,1)
  y <- x + 2*qnorm(0.6)
  expect_equal(cohens_u2(x, y)[[1]], 0.6, tolerance = 0.00001)
  expect_equal(cohens_u1(x, y)[[1]], 1/3, tolerance = 0.00001)
})

test_that("CLES | direaction", {
  expect_equal(d_to_u1(1), d_to_u1(-1))
  expect_equal(d_to_u2(1), d_to_u2(-1))
  expect_equal(d_to_overlap(1), d_to_overlap(-1))

  expect_equal(d_to_p_superiority(1), 1 - d_to_p_superiority(-1))
  expect_equal(d_to_u3(1), 1 - d_to_u3(-1))
})


test_that("CLES vs converters", {
  set.seed(3)
  x <<- rnorm(1000)
  y <<- rnorm(500, mean = 0.2)

  d <- cohens_d(x, y)
  expect_equal(d_to_u3(d), cohens_u3(x, y))
  expect_equal(d_to_p_superiority(d), p_superiority(x, y))
  expect_equal(d_to_overlap(d), p_overlap(x, y))
  expect_equal(d_to_u2(d), cohens_u2(x, y))
  expect_equal(d_to_u1(d), cohens_u1(x, y))

  rb <- rank_biserial(x, y)
  expect_equal(rb_to_p_superiority(rb), p_superiority(x, y, parametric = FALSE))
})

test_that("CLES | mu", {
  set.seed(3)
  x <<- rnorm(500)
  y <<- rnorm(500, mean = 0.2)

  P0 <- p_superiority(x, y, ci = NULL)
  P1 <- p_superiority(x, y, ci = NULL, mu = 0.2)
  expect_true(all(P0[[1]] > P1[[1]]))

  U10 <- cohens_u1(x, y, ci = NULL)
  U11 <- cohens_u1(x, y, ci = NULL, mu = 0.2)
  expect_true(all(U10[[1]] < U11[[1]]))

  U20 <- cohens_u2(x, y, ci = NULL)
  U21 <- cohens_u2(x, y, ci = NULL, mu = 0.2)
  expect_true(all(U20[[1]] < U21[[1]]))

  U30 <- cohens_u3(x, y, ci = NULL)
  U31 <- cohens_u3(x, y, ci = NULL, mu = 0.2)
  expect_true(all(U30[[1]] > U31[[1]]))

  OVL0 <- p_overlap(x, y, ci = NULL)
  OVL1 <- p_overlap(x, y, ci = NULL, mu = 0.2)
  expect_true(all(OVL1[[1]] < OVL0[[1]]))
})


test_that("CLES | par vs non-par", {
  set.seed(3)
  x <<- rnorm(500)
  y <<- rnorm(500, mean = 0.2)

  expect_equal(p_superiority(x, y), p_superiority(x, y, parametric = FALSE),
               tolerance = 0.1, ignore_attr = TRUE)

  expect_equal(cohens_u2(x, y), cohens_u2(x, y, parametric = FALSE),
               tolerance = 0.1, ignore_attr = TRUE)

  skip_on_cran()
  expect_equal(cohens_u3(x, y), cohens_u3(x, y, parametric = FALSE),
               tolerance = 0.1, ignore_attr = TRUE)

  expect_equal(p_overlap(x, y), p_overlap(x, y, parametric = FALSE),
               tolerance = 0.1, ignore_attr = TRUE)
})

test_that("CLES | errors", {
  expect_error(cohens_u1(1:3, 1:4, parametric = FALSE), "parametric")

  expect_error(p_superiority(1:3), "two")
  expect_error(cohens_u1(1:3), "two")
  expect_error(cohens_u2(1:3), "two")
  expect_error(cohens_u3(1:3), "two")
  expect_error(p_overlap(1:3), "two")
})

