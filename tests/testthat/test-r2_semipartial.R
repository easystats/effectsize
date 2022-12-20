# library(testthat)

test_that("r2_semipartial basic", {
  # Type terms
  m <- lm(log(mpg) ~ factor(cyl) + disp + hp * drat, data = mtcars)

  sr2_trms <- r2_semipartial(m, type = "terms")
  sr2_pars <- r2_semipartial(m, type = "parameters")

  expect_equal(sr2_trms[[2]], c(0.0162, 0.0469, 0.0023, 6e-04, 4e-04), tolerance = 0.01)
  expect_equal(sr2_pars[[2]], c(0.0091, 0, 0.0469, 0.0023, 6e-04, 4e-04), tolerance = 0.01)

  # CI

  # Alternative -------------------------------------------------------------
  expect_equal(r2_semipartial(m, alternative = "greater")[c(4, 5)],
    data.frame(CI_low = c(0, 0.0004, 0, 0, 0), CI_high = 1),
    ignore_attr = TRUE, tolerance = 0.01
  )

  expect_equal(r2_semipartial(m, alternative = "less")[c(4, 5)],
    data.frame(CI_low = rep(0, 5), CI_high = c(0.0479, 0.1033, 0.0139, 0.0062, 0.0053)),
    ignore_attr = TRUE, tolerance = 0.01
  )

  expect_equal(r2_semipartial(m, alternative = "two.sided")[c(4, 5)],
    data.frame(CI_low = rep(0, 5), CI_high = c(0.054, 0.1141, 0.0161, 0.0073, 0.0062)),
    ignore_attr = TRUE, tolerance = 0.01
  )
})


test_that("r2_semipartial basic", {
  skip_if_not_installed("performance")

  mr1 <- lm(log(mpg) ~ factor(cyl) + hp * drat, data = mtcars)
  mr2 <- lm(log(mpg) ~ disp + hp * drat, data = mtcars)
  mf <- lm(log(mpg) ~ factor(cyl) + disp + hp * drat, data = mtcars)

  R2r1 <- performance::r2(mr1)[[1]]
  R2r2 <- performance::r2(mr2)[[1]]
  R2f <- performance::r2(mf)[[1]]

  expect_equal(r2_semipartial(mf)[2, 2], R2f - R2r1, ignore_attr = TRUE)
  expect_equal(r2_semipartial(mf)[1, 2], R2f - R2r2, ignore_attr = TRUE)
})
