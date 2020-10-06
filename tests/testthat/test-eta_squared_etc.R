if (require("testthat") && require("effectsize")) {
  test_that("aov", {
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    fit <- aov(Sepal.Length ~ Species * Sepal.Big, df)

    # eta
    testthat::expect_equal(eta_squared(fit, partial = FALSE)$Eta_Sq,
                           c(0.618, 0.046, 0.000),
                           tol = 0.001)
    testthat::expect_equal(eta_squared(fit, partial = TRUE)$Eta_Sq_partial,
                           c(0.649, 0.121, 0.001),
                           tol = 0.001)

    # omega
    testthat::expect_equal(omega_squared(fit, partial = FALSE)$Omega_Sq,
                           c(0.612, 0.043, -0.004),
                           tol = 0.001)
    testthat::expect_equal(omega_squared(fit, partial = TRUE)$Omega_Sq_partial,
                           c(0.638, 0.112, -0.012),
                           tol = 0.001)

    # epsilon
    testthat::expect_equal(epsilon_squared(fit, partial = FALSE)$Epsilon_Sq,
                           c(0.614, 0.044, -0.004),
                           tol = 0.001)
    testthat::expect_equal(epsilon_squared(fit, partial = TRUE)$Epsilon_Sq_partial,
                           c(0.644, 0.115, -0.012),
                           tol = 0.001)

    # Cohen's f/f2
    testthat::expect_equal(cohens_f2(fit, partial = FALSE)$Cohens_f2,
                           c(1.623, 0.049, 0.000),
                           tol = 0.001)
    testthat::expect_equal(cohens_f2(fit, partial = TRUE)$Cohens_f2_partial,
                           c(1.850, 0.139, 0.001),
                           tol = 0.001)
    testthat::expect_equal(cohens_f(fit, partial = FALSE)$Cohens_f,
                           c(1.273, 0.220, 0.021),
                           tol = 0.001)
    testthat::expect_equal(cohens_f(fit, partial = TRUE)$Cohens_f_partial,
                           c(1.360, 0.373, 0.036),
                           tol = 0.001)
    testthat::expect_equal(cohens_f(fit, squared = TRUE), cohens_f2(fit))
    testthat::expect_equal(cohens_f2(fit, squared = FALSE), cohens_f(fit))
  })

  test_that("aovlist", {
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)

    res <- eta_squared(model, partial = TRUE)
    testthat::expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    res <- omega_squared(model, partial = TRUE)
    testthat::expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    res <- epsilon_squared(model, partial = TRUE)
    testthat::expect_true(all(c("Group", "Parameter") %in% colnames(res)))

    testthat::expect_warning(eta_squared(model, partial = FALSE))
    testthat::expect_warning(omega_squared(model, partial = FALSE))
    testthat::expect_warning(epsilon_squared(model, partial = FALSE))
  })

  test_that("mlm / anova table", {

    mtcars$am_f <- factor(mtcars$am)
    mtcars$cyl_f <- factor(mtcars$cyl)

    mod <- lm(cbind(mpg, qsec) ~ am_f * cyl_f, data = mtcars)

    testthat::expect_equal(eta_squared(mod)$Eta_Sq_partial,
                           c(0.674, 0.413, 0.050),
                           tol = 0.001)
  })

  test_that("Cohen's f - R2 change", {
    data(hardlyworking)
    m1 <- lm(salary ~ xtra_hours, data = hardlyworking)
    m2 <- lm(salary ~ xtra_hours + n_comps, data = hardlyworking)

    fsD <- cohens_f2(m1, model2 = m2)[,1:4]
    fs <- cohens_f2(m2)[-1,-1] # this only works because of the defaul type-I errors! :(
    rownames(fsD) <- rownames(fs) <- 1
    testthat::expect_equal(fsD, fs)

    if (require("performance")) {
      fsD <- cohens_f2(m1, model2 = m2)

      R2_1 <- performance::r2(m1)[[1]]
      R2_2 <- performance::r2(m2)[[1]]

      testthat::expect_equal(fsD$Cohens_f2_partial,
                             unname((R2_2 - R2_1) / (1 - R2_2)))
    }
  })
}



