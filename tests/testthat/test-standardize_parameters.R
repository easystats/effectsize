if (require("testthat") && require("effectsize")) {
  test_that("standardize_parameters (simple)", {
    df <- iris
    r <- as.numeric(cor.test(df$Sepal.Length, df$Petal.Length)$estimate)

    model <- lm(Sepal.Length ~ Petal.Length, data = df)
    es <- standardize_parameters(model)[2, 2]
    testthat::expect_equal(es, r, tol = 0.01)
  })

  test_that("standardize_parameters (lm with ci)", {
    model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

    testthat::expect_equal(
      standardize_parameters(model, method = "refit")$Std_Coefficient,
      c(0.044, -0.072, -0.060, 0.844),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, method = "posthoc")$Std_Coefficient,
      c(0, -0.072, -0.060, 0.844),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, method = "smart")$Std_Coefficient,
      c(0, -0.170, -0.142, 0.844),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, method = "basic")$Std_Coefficient,
      c(0, -0.034, -0.028, 0.844),
      tol = 0.01
    )

    ## CI
    testthat::expect_equal(
      standardize_parameters(model, method = "basic")$CI_low,
      c(0, -0.294, -0.433, 0.491),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, method = "basic")$CI_high,
      c(0, 0.225, 0.375, 1.196),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, ci = 0.8, method = "basic")$CI_low,
      c(0, -0.203, -0.292, 0.614),
      tol = 0.01
    )

    testthat::expect_equal(
      standardize_parameters(model, ci = 0.8, method = "basic")$CI_high,
      c(0, 0.135, 0.234, 1.073),
      tol = 0.01
    )
  })

  if (require(rstanarm)) {
    test_that("standardize_parameters (simple)", {
      testthat::skip_on_cran()
      set.seed(1234)
      suppressWarnings(
        model <- stan_glm(Sepal.Length ~ Species + Petal.Width,
                          data = iris,
                          iter = 500, refresh = 0)
      )

      testthat::expect_equal(
        suppressWarnings(standardize_parameters(model, method = "refit")$Std_Median),
        c(0.037, -0.061, -0.060, 0.838),
        tol = 0.01
      )

      testthat::expect_equal(
        suppressWarnings(standardize_parameters(model, method = "posthoc")$Std_Median),
        c(NA, -0.050, -0.030,  0.833),
        tol = 0.01
      )
    })
  }
}
