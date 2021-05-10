if (require("testthat") && require("effectsize")) {
  test_that("sd_pooled", {
    expect_equal(sd_pooled(1:4, 1:3 * 5), 3.316625, tolerance = 0.001)
    expect_equal(mad_pooled(1:4, 1:3 * 5), 3.297154, tolerance = 0.001)

    expect_equal(sd_pooled(c(1:3, 40), 1:3 * 5), 15.06652, tolerance = 0.001)
    expect_equal(mad_pooled(c(1:3, 40), 1:3 * 5), 3.297154, tolerance = 0.001)
  })
}
