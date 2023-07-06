test_that("basic examples", {
  # t.test
  x <- t.test(mpg ~ vs, data = mtcars)
  expect_warning(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # cor.test
  # no need to specify the data argument
  x <- cor.test(~ qsec + drat, data = mtcars)
  expect_warning(effectsize(x), "'htest' method is not")

  # wilcox.test
  x <- wilcox.test(mpg ~ vs, data = mtcars, exact = FALSE)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # friedman.test
  wb <- aggregate(warpbreaks$breaks, by = list(
    w = warpbreaks$wool, t = warpbreaks$tension
  ), FUN = mean)
  x <- friedman.test(x ~ w | t, data = wb)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = wb))

  # kruskal.test
  # Cannot get it to work
  airquality2 <- airquality
  airquality2$Month <- as.factor(airquality2$Month)
  airquality2$Ozone <- ifelse(is.na(airquality2$Ozone), 10, airquality2$Ozone)
  x <- kruskal.test(Ozone ~ Month, data = airquality2)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = airquality2))
})

test_that("edge cases", {
  # Example 1
  tt1 <- t.test(mpg ~ I(am + cyl == 4), data = mtcars)
  dd1 <- cohens_d(mpg ~ I(am + cyl == 4), data = mtcars, pooled_sd = FALSE)

  expect_warning(effectsize(tt1), "Unable to retrieve data")
  expect_no_warning(effectsize(tt1, data = mtcars))
  expect_identical(effectsize(tt1, data = mtcars)[[1]], dd1[[1]])

  # Example 2
  dat <- mtcars
  tt2 <- t.test(dat$mpg[dat$am == 1], dat$mpg[dat$am == 0])
  dd2 <- cohens_d(dat$mpg[dat$am == 1], dat$mpg[dat$am == 0], pooled_sd = FALSE)

  rm("dat")
  expect_warning(effectsize(tt2), "Unable to retrieve data")
  expect_no_warning(effectsize(tt2, data = mtcars))

  expect_identical(effectsize(tt2, data = mtcars)[[1]], dd2[[1]])

  # Example 3
  col_y <- "mpg"
  tt3 <- t.test(mtcars[[col_y]])
  dd3 <- cohens_d(mtcars[[col_y]])

  rm("col_y")
  expect_warning(effectsize(tt3), "Unable to retrieve data")
  expect_no_warning(effectsize(tt3, data = mtcars))
  expect_identical(effectsize(tt3, data = mtcars)[[1]], dd3[[1]])

  # Example 4
  tt4 <- t.test(mpg ~ as.factor(am), data = mtcars)

  expect_warning(effectsize(tt4), "Unable to retrieve data")
  expect_no_warning(effectsize(tt4, data = mtcars))

  # wilcox.test
  x <- wilcox.test(mpg ~ as.factor(vs), data = mtcars, exact = FALSE)
  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = mtcars))

  # friedman.test does not allow formula modifiers, skipping

  # kruskal.test
  airquality2 <- airquality
  airquality2$Month <- as.factor(airquality2$Month)
  airquality2$Ozone <- ifelse(is.na(airquality2$Ozone), 10, airquality2$Ozone)
  x <- kruskal.test(Ozone ~ as.factor(Month), data = airquality2)

  expect_error(effectsize(x), "Unable to retrieve data")
  expect_no_warning(effectsize(x, data = airquality2))

  # Paired t-test
  x <- t.test(mpg ~ 1, data = mtcars)
  expect_no_warning(effectsize(x, data = mtcars))

  x <- t.test(Pair(mpg, hp) ~ 1, data = mtcars)
  expect_no_warning(effectsize(x, data = mtcars))
})
