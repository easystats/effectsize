if (require("testthat") && require("effectsize")) {
  test_that("cohens_d errors and warnings", {

    # Direction ---------------------------------------------------------------
    rez_t <- t.test(iris$Sepal.Length, iris$Sepal.Width)
    rez_d <- cohens_d(iris$Sepal.Length, iris$Sepal.Width)
    expect_equal(sign(rez_t$statistic), sign(rez_d$Cohens_d), ignore_attr = TRUE)


    # Errors and warnings -----------------------------------------------------
    df <- data.frame(
      a = 1:10,
      b = 2:11,
      c = rep(letters[1:2], each = 5),
      d = c("a", "b", "b", "c", "c", "b", "c", "a", "a", "b"),
      e = rep(0:1, each = 5)
    )
    df$exp_a <- exp(df$a)
    a2 <- 1:11

    expect_error(cohens_d(a ~ c, data = df), regexp = NA)
    expect_error(cohens_d("a", "c", data = df), regexp = NA)
    expect_error(cohens_d("a", "b", data = df), regexp = NA)
    expect_error(cohens_d(a2, df$b), regexp = NA)
    expect_error(cohens_d(b ~ e, data = df), regexp = NA)

    expect_error(cohens_d(df$a ~ df$c), regexp = NA)
    expect_equal(cohens_d("exp_a", "c", data = df), cohens_d(exp(a) ~ c, data = df))

    expect_error(cohens_d(a ~ b, data = df))
    expect_error(cohens_d(a ~ d, data = df))
    expect_error(cohens_d("a", "d", data = df))
    expect_error(cohens_d("c", "c", data = df))
    expect_error(cohens_d(a2, df$c))
    expect_error(cohens_d("a", "aa", data = df))

    expect_warning(cohens_d("b", "e", data = df))
  })

  test_that("cohens_d - mu", {
    expect_equal(cohens_d(mtcars$mpg - 5),
      cohens_d(mtcars$mpg, mu = 5),
      ignore_attr = TRUE
    )

    x <- 1:9
    y <- c(1, 1:9)
    expect_equal(cohens_d(x - 3, y),
      cohens_d(x, y, mu = 3),
      ignore_attr = TRUE
    )

    # t.test(x, y, mu = 3.125, var.equal = TRUE)
    d <- cohens_d(x, y, mu = 3.125)
    expect_equal(d[[1]], -0.969, tolerance = 0.01)
    expect_equal(d$CI_low, -1.913, tolerance = 0.01)
    expect_equal(d$CI_high[[1]], 0, tolerance = 0.01)
  })

  test_that("cohens_d - pooled", {
    x <- cohens_d(wt ~ am, data = mtcars, pooled_sd = TRUE)
    expect_equal(colnames(x)[1], "Cohens_d")
    expect_equal(x[[1]], 1.892, tolerance = 0.001)
    expect_equal(x$CI_low, 1.030, tolerance = 0.001)
    expect_equal(x$CI_high, 2.732, tolerance = 0.001)
  })

  test_that("cohens_d - non-pooled", {
    x <- cohens_d(wt ~ am, data = mtcars, pooled_sd = FALSE)
    expect_equal(colnames(x)[1], "Cohens_d")
    expect_equal(x[[1]], 1.934, tolerance = 0.001)
    expect_equal(x$CI_low, 1.098798, tolerance = 0.001)
    expect_equal(x$CI_high, 2.833495, tolerance = 0.001)
  })

  test_that("hedges_g (and other bias correction things", {
    expect_warning(x <- hedges_g(wt ~ am, data = mtcars, correction = TRUE))
    expect_equal(colnames(x)[1], "Hedges_g")
    expect_equal(x[[1]], 1.844, tolerance = 0.001)
    expect_equal(x$CI_low, 1.004, tolerance = 0.001)
    expect_equal(x$CI_high, 2.664, tolerance = 0.001)
  })

  test_that("glass_delta", {
    # must be 2 samples
    expect_error(glass_delta(1:10))

    skip_if_not_installed("boot")
    skip_if_not_installed("base", minimum_version = "3.6.0")

    expect_error(glass_delta(wt, data = mtcars))

    set.seed(8007)
    x <- glass_delta(wt ~ am, data = mtcars)
    expect_equal(colnames(x)[1], "Glass_delta")
    expect_equal(x[[1]], 2.200, tolerance = 0.001)
    expect_equal(x$CI_low, 1.490089, tolerance = 0.001)
    expect_equal(x$CI_high, 3.858925, tolerance = 0.001)
  })


  test_that("fixed values", {
    skip_if_not_installed("bayestestR")

    x1 <- bayestestR::distribution_normal(1e4, mean = 0, sd = 1)
    x2 <- bayestestR::distribution_normal(1e4, mean = 1, sd = 1)
    expect_equal(cohens_d(x1, x2)$Cohens_d, -1, tolerance = 1e-3)


    x1 <- bayestestR::distribution_normal(1e4, mean = 0, sd = 1)
    x2 <- bayestestR::distribution_normal(1e4, mean = 1.5, sd = 2)

    expect_equal(cohens_d(x1, x2)[[1]], -sqrt(0.9), tolerance = 1e-2)
    expect_equal(glass_delta(x2, x1, ci = NULL)[[1]], 1.5, tolerance = 1e-2)
  })

  test_that("Missing values", {
    x <- c(1, 2, NA, 3)
    y <- c(1, 1, 2, 3)
    expect_equal(cohens_d(x, y)[[1]], 0.2564946, tolerance = 0.01) # indep
    expect_equal(cohens_d(x, y, paired = TRUE)[[1]], 0.5773503, tolerance = 0.01) # paired

    # no length problems
    expect_error(cohens_d(mtcars$mpg - 23), regexp = NA)
  })
}
