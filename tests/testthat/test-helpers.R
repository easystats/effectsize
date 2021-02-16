if (require("testthat") && require("effectsize")) {
  test_that("is_effectsize_name works", {
    expect_false(is_effectsize_name("is_effectsize_name"))
    expect_true(is_effectsize_name("Eta2"))
  })
}
