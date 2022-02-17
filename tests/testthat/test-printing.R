if (require("testthat") && require("effectsize")) {
  test_that("effectsize table", {
    es <- eta_squared(a <- aov(mpg ~ cyl + gear, mtcars))
    e2 <- eta_squared(a, generalized = "gear")
    e3 <- eta_squared(a, generalized = TRUE)
    expect_output(print(es), regexp = "Eta2")
    expect_output(print(es), regexp = "One-sided CIs: upper bound fixed at [1.00]", fixed = TRUE)
    expect_output(print(e2), regexp = "Observed variables: gear", fixed = TRUE)
    expect_output(print(e3), regexp = "Observed variables: All", fixed = TRUE)

    # Rounded bound
    RCT <- matrix(c(71, 30, 31, 13, 50, 100, 4, 5, 7), nrow = 3, byrow = TRUE)
    expect_output(print(phi(RCT)), regexp = "[1.41~]", fixed = TRUE)
  })


  test_that("std effectsize table", {
    es <- standardize_parameters(mod <- lm(mpg ~ cyl + gear, mtcars))
    expect_output(print(es), regexp = "refit")

    es <- standardize_parameters(mod, method = "basic")
    expect_output(print(es), regexp = "basic")

    es <- standardize_parameters(mod, robust = TRUE)
    expect_output(print(es), regexp = "median")

    es <- standardize_parameters(mod, two_sd = TRUE)
    expect_output(print(es), regexp = "two")

    es <- standardize_parameters(mod, include_response = FALSE)
    expect_output(print(es), regexp = "unstandardized")
  })


  test_that("effectsize difference", {
    d <- cohens_d(1:3, c(1, 1:3))
    expect_output(print(d), regexp = "Cohen")
    expect_output(print(d), regexp = " pooled", fixed = TRUE)
    expect_output(print(d, append_CLES = TRUE), regexp = "U3")

    d <- cohens_d(1:3, c(1, 1:3), pooled_sd = FALSE)
    expect_output(print(d), regexp = "un-pooled")

    d <- cohens_d(1:5, c(1, 1:4), paired = TRUE)
    expect_error(expect_output(print(d), regexp = "pooled"))
  })

  test_that("equivalence test effectsize", {
    d <- cohens_d(1:3, c(1, 1:3))
    equtest <- equivalence_test(d)
    expect_output(print(equtest), regexp = "ROPE")
  })


  test_that("rules", {
    r1 <- rules(1:3, letters[1:4], name = "XX")
    expect_output(print(r1), regexp = "Thresholds")
    expect_output(print(r1), regexp = "<=")
    expect_output(print(r1), regexp = "XX")


    r2 <- rules(1:3, letters[1:3], name = "YY")
    expect_output(print(r2), regexp = "Values")
    expect_output(print(r2), regexp = "YY")


    expect_output(print(interpret(0, r1)), '"a"')
    expect_output(print(interpret(0, r1)), 'XX')
  })
}
