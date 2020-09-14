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

  test_that("standardize_parameters (with dunction interactions)", {
    X <- scale(rnorm(100),T,F)
    Z <- scale(rnorm(100),T,F)
    Y <- scale(Z + X * Z + rnorm(100),T,F)

    m1 <- lm(Y ~ X * Z)
    m2 <- lm(Y ~ X * scale(Z))
    m3 <- lm(Y ~ scale(X) * Z)
    m4 <- lm(Y ~ scale(X) * scale(Z))

    testthat::expect_equal(
      standardize_parameters(m1, method = "basic")$Std_Coefficient,
      standardize_parameters(m2, method = "basic")$Std_Coefficient
    )
    testthat::expect_equal(
      standardize_parameters(m1, method = "basic")$Std_Coefficient,
      standardize_parameters(m3, method = "basic")$Std_Coefficient
    )
    testthat::expect_equal(
      standardize_parameters(m1, method = "basic")$Std_Coefficient,
      standardize_parameters(m4, method = "basic")$Std_Coefficient
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
    test_that("standardize_parameters (Pseudo - GLMM)", {
      set.seed(1)
      N <- 30
      k <- 100
      ID <- rep(1:N, each = k)
      x_between <- rnorm(N)[ID]
      x_within <- rnorm(N*k)
      c_within <- sample(letters[1:3], size = N*k, replace = TRUE)
      y <- ID/N + x_between + x_within + rnorm(N*k, sd = 2)

      dat <- data.frame(y, x_within, c_within, x_between, ID)



      fit <- lmer(y ~ scale(x_within) * x_between + c_within + (scale(x_within) | ID),
                        data = dat)

      expect_warning(standardize_parameters(fit, method = "pseudo", robust = TRUE))

      ## Interactions with level1 and level1 factors
      b <- fixef(fit)[-1]
      mm <- model.matrix(fit)[,-1]
      SD_x <- numeric(ncol(mm))
      SD_x[c(1,3,4,5)] <- apply(mm[,c(1,3,4,5)], 2, sd)
      SD_x[2] <- apply(mm[,2, drop = F], 2, function(.x) sd(tapply(.x, dat$ID, mean)))
      m0 <- lmer(y ~ 1 + (1 | ID), data = dat)
      m0v <- insight::get_variance(m0)
      SD_y <- c(sqrt(m0v$var.residual), sqrt(m0v$var.intercept))
      SD_y <- SD_y[c(1,2,1,1,1)]

      expect_equal(
        data.frame(Deviation_Response_Pseudo = c(NA,SD_y),Deviation_Pseudo = c(NA,SD_x)),
        standardize_info(fit)[, c("Deviation_Response_Pseudo", "Deviation_Pseudo")]
      )
      expect_equal(
        standardize_parameters(fit, method = "pseudo")$Std_Coefficient[-1],
        unname(b * SD_x/SD_y)
      )


      ## scaling should not affect

      m1 <- lmer(y ~ x_within + x_between + c_within + (x_within | ID),
                       data = dat)
      m2 <- lmer(y ~ scale(x_within) + x_between + c_within + (scale(x_within) | ID),
                       data = dat)
      m3 <- lmer(scale(y) ~ x_within + x_between + c_within + (x_within | ID),
                       data = dat)
      m4 <- lmer(y ~ x_within + scale(x_between) + c_within + (x_within | ID),
                       data = dat)
      std1 <- standardize_parameters(m1, method = "pseudo")

      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m2, method = "pseudo")$Std_Coefficient, tol = 0.001)
      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m3, method = "pseudo")$Std_Coefficient, tol = 0.001)
      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m4, method = "pseudo")$Std_Coefficient, tol = 0.001)
    })
  }
}
