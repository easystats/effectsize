if (require("testthat") && require("effectsize")) {
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



