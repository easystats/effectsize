if (require("testthat") && require("effectsize")) {
  test_that("standardize_parameters (simple)", {
    df <- iris
    r <- as.numeric(cor.test(df$Sepal.Length, df$Petal.Length)$estimate)

    model <- lm(Sepal.Length ~ Petal.Length, data = df)
    es <- standardize_parameters(model)
    testthat::expect_equal(es[2,2], r, tol = 0.01)
  })


  test_that("standardize_parameters (model_parameters)", {
    model <<- lm(mpg ~ cyl + am, data = mtcars)
    mp <<- parameters::model_parameters(model)

    s1 <- standardize_parameters(model, method = "basic")
    s2 <- standardize_parameters(mp, method = "basic")

    testthat::expect_equal(s1$Parameter, s2$Parameter)
    testthat::expect_equal(s1$Std_Coefficient, s2$Std_Coefficient)
    testthat::expect_equal(s1$CI_low, s2$CI_low)
    testthat::expect_equal(s1$CI_high, s2$CI_high)
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

  test_that("standardize_parameters (with function interactions)", {
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
    # testthat::expect_equal(
    #   standardize_parameters(m1, method = "basic")$Std_Coefficient,
    #   standardize_parameters(m4, method = "basic")$Std_Coefficient
    # )
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
        c(0, -0.058, -0.053,  0.838),
        tol = 0.01
      )

      posts <- standardize_posteriors(model, method = "posthoc")
      testthat::expect_equal(dim(posts), c(1000, 4))
      testthat::expect_is(posts, "data.frame")
    })
  }

  if (require(lme4)) {
    test_that("standardize_parameters (Pseudo - GLMM)", {
      set.seed(1)

      dat <- data.frame(X = rnorm(1000),
                        Z = rnorm(1000),
                        C = sample(letters[1:3], size = 1000, replace = TRUE),
                        ID = sort(rep(letters, length.out = 1000)))
      dat <- transform(dat, Y = X + Z + rnorm(1000))
      dat <- cbind(dat,parameters::demean(dat,c("X","Z"),"ID"))


      m <- lmer(Y ~ scale(X_within) * X_between + C + (scale(X_within) | ID),
                data = dat)

      ## No robust methods... (yet)
      expect_warning(standardize_parameters(m, method = "pseudo", robust = TRUE))


      ## Correctly identify within and between terms
      dev_resp <- standardize_info(m, include_pseudo = TRUE)$Deviation_Response_Pseudo
      expect_equal(length(unique(dev_resp[c(2, 4, 5, 6)])), 1)
      expect_true(dev_resp[2] != dev_resp[3])


      ## Calc
      b <- fixef(m)[-1]
      mm <- model.matrix(m)[,-1]
      SD_x <- numeric(ncol(mm))

      SD_x[c(1,3,4,5)] <- apply(mm[,c(1,3,4,5)], 2, sd)
      SD_x[2] <- sd(tapply(mm[,2], dat$ID, mean))

      m0 <- lmer(Y ~ 1 + (1 | ID), data = dat)
      m0v <- insight::get_variance(m0)
      SD_y <- c(sqrt(m0v$var.residual), sqrt(m0v$var.intercept))
      SD_y <- SD_y[c(1,2,1,1,1)]

      expect_equal(
        data.frame(Deviation_Response_Pseudo = c(NA,SD_y),Deviation_Pseudo = c(NA,SD_x)),
        standardize_info(m, include_pseudo = TRUE)[, c("Deviation_Response_Pseudo", "Deviation_Pseudo")]
      )
      expect_equal(
        standardize_parameters(m, method = "pseudo")$Std_Coefficient[-1],
        unname(b * SD_x/SD_y)
      )


      ## scaling should not affect
      m1 <- lmer(Y ~ X_within + X_between + C + (X_within | ID),
                 data = dat)
      m2 <- lmer(scale(Y) ~ X_within + X_between + C + (X_within | ID),
                 data = dat)
      m3 <- lmer(Y ~ scale(X_within) + X_between + C + (scale(X_within) | ID),
                 data = dat)
      m4 <- lmer(Y ~ X_within + scale(X_between) + C + (X_within | ID),
                 data = dat)

      std1 <- standardize_parameters(m1, method = "pseudo")
      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m2, method = "pseudo")$Std_Coefficient, tol = 0.001)
      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m3, method = "pseudo")$Std_Coefficient, tol = 0.001)
      expect_equal(std1$Std_Coefficient,
                   standardize_parameters(m4, method = "pseudo")$Std_Coefficient, tol = 0.001)



      ## Give warning for within that is also between
      mW <- lmer(Y ~ X_between + Z_within + C + (1 | ID), dat)
      mM <- lmer(Y ~ X + Z + C + (1 | ID), dat)

      expect_warning(standardize_parameters(mW, method = "pseudo"), NA)
      expect_warning(standardize_parameters(mM, method = "pseudo"))
    })
  }
}
