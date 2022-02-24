if (require("testthat") && require("effectsize")) {
  test_that("is_effectsize_name works", {
    expect_false(is_effectsize_name("is_effectsize_name"))
    expect_true(is_effectsize_name("Eta2"))
    expect_equal(get_effectsize_label("hEDgES_G"), "Hedges' g")
  })

  test_that("validate data from formula", {
    expect_error(cohens_d(mpg ~ cyl, data = mtcars))
    expect_error(cohens_d(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    expect_error(rank_biserial(mpg ~ cyl, data = mtcars))
    expect_error(rank_biserial(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    expect_error(sd_pooled(mpg ~ cyl, data = mtcars))
    expect_error(sd_pooled(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

    expect_error(cles(mpg ~ cyl, data = mtcars))
    expect_error(cles(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

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
