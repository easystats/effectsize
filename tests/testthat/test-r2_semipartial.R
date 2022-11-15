
test_that("r2_semipartial basic", {

  expected1 <- c(0.0162425886, 0.0468682764, 0.0022934024, 0.0005546468, 0.0004055358)

  expected2 <- c(0.009101912272537, 0.000000004084085, 0.046868276401258,
    0.002293402440118, 0.000554646819724, 0.000405535802258)

  # Type terms
  m <- lm(log(mpg) ~ factor(cyl) + disp + hp * drat, data = mtcars)
  expect_equal(r2_semipartial(m, type = "terms")$r2_semipartial,
               expected1)

  # Type parameters
  expect_equal(r2_semipartial(m, type = "parameters")$r2_semipartial,
               expected2)

  # Alternative -------------------------------------------------------------
  expect_equal(r2_semipartial(m, alternative = "greater")[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = 1), ignore_attr = TRUE)

  expect_equal(r2_semipartial(m, alternative = "less")[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = c(
                 0.105, 0.234, 0.097, 0.047, 0.036)), ignore_attr = TRUE,
               tolerance = 0.01)

  expect_equal(r2_semipartial(m, alternative = "two.sided")[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = c(
                 0.152, 0.275, 0.141, 0.095, 0.084)), ignore_attr = TRUE,
               tolerance = 0.01)

  # Confidence interval -------------------------------------------------------------

  expect_equal(r2_semipartial(m, ci = .95)[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = 1), ignore_attr = TRUE)

  expect_equal(r2_semipartial(m, ci = .90)[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = 1), ignore_attr = TRUE)

  expect_equal(r2_semipartial(m, ci = .80)[c(4,5)],
               data.frame(CI_low = rep(0, 5), CI_high = 1), ignore_attr = TRUE)

})
