if (require("testthat") && require("effectsize")) {
  test_that("effectsize table", {
    es <- eta_squared(aov(mpg ~ cyl + gear, mtcars))
    testthat::expect_output(print(es), regexp = "Eta2")

    stdz1 <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars))
    stdz2 <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), method = "basic")
    testthat::expect_output(print(stdz1), regexp = "refit")
    testthat::expect_output(print(stdz2), regexp = "basic")
  })


  test_that("std effectsize table", {
    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars))
    testthat::expect_output(print(es), regexp = "refit")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), method = "basic")
    testthat::expect_output(print(es), regexp = "basic")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), robust = TRUE)
    testthat::expect_output(print(es), regexp = "median")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), two_sd = TRUE)
    testthat::expect_output(print(es), regexp = "two")
  })


  test_that("effectsize difference", {
    d <- cohens_d(1:3, c(1, 1:3))
    testthat::expect_output(print(d), regexp = "Cohen")
    testthat::expect_output(print(d), regexp = " pooled", fixed = TRUE)
    testthat::expect_output(print(d, append_CL = TRUE), regexp = "U3")

    d <- cohens_d(1:3, c(1, 1:3), pooled_sd = FALSE)
    testthat::expect_output(print(d), regexp = "un-pooled")

    d <- cohens_d(1:5, c(1,1:4), paired = TRUE)
    testthat::expect_error(testthat::expect_output(print(d), regexp = "pooled"))

    d <- hedges_g(1:3, c(1, 1:3), correction = 1)
    testthat::expect_output(print(d), regexp = "Hedges and Olkin")

    d <- hedges_g(1:3, c(1, 1:3), correction = 2)
    testthat::expect_output(print(d), regexp = "Hunter and Schmidt")
  })

  test_that("equivalence test effectsize", {
    d <- cohens_d(1:3, c(1, 1:3))
    equtest <- equivalence_test(d)
    testthat::expect_output(print(equtest), regexp = "ROPE")
  })


  test_that("rules", {
    r1 <- rules(1:3, letters[1:4])
    testthat::expect_output(print(r1), regexp = "thresholds")
    testthat::expect_output(print(r1), regexp = "<=")


    r2 <- rules(1:3, letters[1:3])
    testthat::expect_output(print(r2), regexp = "values")
  })
}
