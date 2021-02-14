if (require("testthat") && require("effectsize")) {
  test_that(".factor_to_numeric works", {
    expect_equal(.factor_to_numeric(as.factor(c(1, 2, 3, 4))), c(1, 2, 3, 4))
    expect_equal(.factor_to_numeric(as.factor(c(1, 2, NA, 4))), c(1, 2, NA, 4))
    expect_equal(.factor_to_numeric(c(1, 2, 3, 4)), c(1, 2, 3, 4))
  })

  test_that("is_effectsize_name works", {
    expect_false(is_effectsize_name("is_effectsize_name"))
    expect_true(is_effectsize_name("Eta2"))
  })
}
