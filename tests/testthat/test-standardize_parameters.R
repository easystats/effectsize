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
    test_that("standardize_parameters (Bayes)", {
      testthat::skip_on_cran()
      testthat::skip_on_ci()
      set.seed(1234)
      suppressWarnings(
        model <- stan_glm(Sepal.Length ~ Species + Petal.Width,
                          data = iris,
                          iter = 500, refresh = 0)
      )

      testthat::expect_equal(
        suppressWarnings(standardize_parameters(model, method = "refit")$Std_Median),
        c(0.065, -0.094, -0.100, 0.862),
        tol = 0.01
      )

      testthat::expect_equal(
        suppressWarnings(standardize_parameters(model, method = "posthoc")$Std_Median),
        c(NA, -0.058, -0.053,  0.838),
        tol = 0.01
      )
    })
  }

  if (require(lme4)) {
    test_that("Pseudo std", {
      set.seed(1)
      N <- 10
      k <- 10
      ID <- rep(1:N, each = k)
      x_between <- rnorm(N)[ID]
      x_within <- rnorm(N*k)
      y <- ID/N + x_between + x_within + rnorm(N*k, sd = 2)

      dat <- data.frame(y, x_within, x_between, ID)

      fit <- lmer(y ~ x_within + x_between + (x_within | ID),
                  data = dat)


      m0 <- lmer(y ~ 1 + (1 | ID),
                 data = dat)
      m0v <- insight::get_variance(m0)

      b <- fixef(fit)[-1]
      SD_y <- c(sqrt(m0v$var.residual), sqrt(m0v$var.intercept))
      SD_x <- c(sd(dat$x_within),sd(tapply(dat$x_between,dat$ID,mean)))
      pseudo_std <- b * SD_x / SD_y

      std <- standardize_parameters(fit, method = "pseudo")

      expect_equal(std$Std_Coefficient[-1], unname(pseudo_std))
      expect_warning(standardize_parameters(fit, method = "pseudo", robust = TRUE))
    })
  }
}
