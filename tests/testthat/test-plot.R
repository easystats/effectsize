if (require(effectsize) && require(testthat)) {
  test_that("plot methods", {
    skip_if_not_installed("see")
    skip_if_not_installed("ggplot2")
    expect_error(plot(d <- cohens_d(mpg ~ am, data = mtcars)), NA)
    expect_s3_class(plot(d), "ggplot")

    expect_error(plot(eqi <- equivalence_test(d)), NA)
    expect_s3_class(plot(eqi), "ggplot")
  })
}
