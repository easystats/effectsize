test_that("printing symbols works as expected", {
  withr::with_options(
    list(es.use_symbols = TRUE),
    code = {
      RCT <- matrix(c(71, 50, 30, 100), nrow = 2L)
      m <- lm(mpg ~ cyl + am, data = mtcars)

      expect_snapshot(phi(RCT))
    }
  )
})
