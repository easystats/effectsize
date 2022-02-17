if (require(effectsize) && require(testthat) &&
    require(see) && require(ggplot2)) {
  test_that("plot methods", {
    skip_if_not_installed("see", "0.6.8")
    expect_error(plot(d <- cohens_d(mpg ~ am, data = mtcars)), NA)
    expect_s3_class(plot(d), "ggplot")

    expect_error(plot(eqi <- equivalence_test(d)), NA)
    expect_s3_class(plot(eqi), "ggplot")
  })
}