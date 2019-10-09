context("standardize_parameters")



test_that("standardize_parameters (simple)", {
  df <- iris
  r <- as.numeric(cor.test(df$Sepal.Length, df$Petal.Length)$estimate)

  model <- lm(Sepal.Length ~ Petal.Length, data = df)
  es <- standardize_parameters(model)[2, 2]
  testthat::expect_equal(es, r, tol = 0.01)
})

