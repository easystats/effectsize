if (require("testthat") && require("effectsize")) {
  test_that("effectsize table", {
    es <- eta_squared(aov(mpg ~ cyl + gear, mtcars))
    expect_output(print(es), regexp = "Eta2")

    stdz1 <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars))
    stdz2 <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), method = "basic")
    expect_output(print(stdz1), regexp = "refit")
    expect_output(print(stdz2), regexp = "basic")
  })


  test_that("std effectsize table", {
    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars))
    expect_output(print(es), regexp = "refit")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), method = "basic")
    expect_output(print(es), regexp = "basic")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), robust = TRUE)
    expect_output(print(es), regexp = "median")

    es <- standardize_parameters(lm(mpg ~ cyl + gear, mtcars), two_sd = TRUE)
    expect_output(print(es), regexp = "two")
  })


  test_that("effectsize difference", {
    d <- cohens_d(1:3, c(1, 1:3))
    expect_output(print(d), regexp = "Cohen")
    expect_output(print(d), regexp = " pooled", fixed = TRUE)
    expect_output(print(d, append_CL = TRUE), regexp = "U3")

    d <- cohens_d(1:3, c(1, 1:3), pooled_sd = FALSE)
    expect_output(print(d), regexp = "un-pooled")

    d <- cohens_d(1:5, c(1, 1:4), paired = TRUE)
    expect_error(expect_output(print(d), regexp = "pooled"))

    d <- hedges_g(1:3, c(1, 1:3), correction = 1)
    expect_output(print(d), regexp = "Hedges and Olkin")

    d <- hedges_g(1:3, c(1, 1:3), correction = 2)
    expect_output(print(d), regexp = "Hunter and Schmidt")
  })

  test_that("equivalence test effectsize", {
    d <- cohens_d(1:3, c(1, 1:3))
    equtest <- equivalence_test(d)
    expect_output(print(equtest), regexp = "ROPE")
  })


  test_that("rules", {
    r1 <- rules(1:3, letters[1:4])
    expect_output(print(r1), regexp = "thresholds")
    expect_output(print(r1), regexp = "<=")


    r2 <- rules(1:3, letters[1:3])
    expect_output(print(r2), regexp = "values")
  })
}
