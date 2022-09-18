if (require("testthat") && require("effectsize")) {
  test_that("is_effectsize_name works", {
    expect_false(is_effectsize_name("is_effectsize_name"))
    expect_true(is_effectsize_name("Eta2"))
    expect_equal(get_effectsize_label("hEDgES_G"), "Hedges' g")
  })

  test_that("validate data from formula", {
    expect_error(cohens_d(mpg ~ cyl, data = mtcars), "exactly")
    expect_error(cohens_d(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    d1 <- cohens_d(mpg ~ cyl,
      data = mtcars,
      subset = cyl < 8
    )

    x <- mtcars$cyl < 8
    d2 <- cohens_d(mpg ~ cyl,
      data = mtcars,
      subset = x
    )

    x <- mtcars$cyl
    d3 <- cohens_d(mpg ~ cyl,
      data = mtcars,
      subset = x < 8
    )

    d4 <- cohens_d(mpg ~ cyl,
      data = mtcars,
      subset =
        c(
          TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
          TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
          TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
          FALSE, TRUE
        )
    )

    expect_equal(d1, d2)
    expect_equal(d1, d3)
    expect_equal(d1, d4)

    expect_error(rank_biserial(mpg ~ cyl, data = mtcars), "exactly")
    expect_error(rank_biserial(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    expect_error(sd_pooled(mpg ~ cyl, data = mtcars), "exactly")
    expect_error(sd_pooled(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    expect_error(cohens_u1(mpg ~ cyl, data = mtcars), "exactly")
    expect_error(cohens_u1(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    d <- expand.grid(id = 1:30, g = 1:4)
    d$y <- rnorm(nrow(d)) + d$g
    expect_equal(
      rank_epsilon_squared(y ~ g, data = d, subset = g < 4, ci = NULL),
      rank_epsilon_squared(y ~ g, data = subset(d, g < 4), ci = NULL)
    )

    expect_equal(
      kendalls_w(y ~ g | id, data = d, subset = g < 4, ci = NULL),
      kendalls_w(y ~ g | id, data = subset(d, g < 4), ci = NULL)
    )
  })
}
