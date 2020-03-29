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
}



