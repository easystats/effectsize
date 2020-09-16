if (require("testthat") && require("effectsize")) {
  test_that("odds_to_d", {
    testthat::expect_equal(odds_to_d(0.2), -0.887, tolerance = 0.01)
    testthat::expect_equal(odds_to_d(-1.45, log = TRUE), -0.7994, tolerance = 0.01)
    testthat::expect_equal(d_to_odds(-0.887), 0.2, tolerance = 0.01)
    testthat::expect_equal(d_to_odds(-0.7994, log = TRUE), -1.45, tolerance = 0.01)
  })


  test_that("d_to_r", {
    testthat::expect_equal(d_to_r(d = 1.1547), 0.5, tolerance = 0.01)
    testthat::expect_equal(r_to_d(r = 0.5), 1.1547, tolerance = 0.01)

    testthat::expect_equal(odds_to_r(odds = d_to_odds(d = r_to_d(0.5))), 0.5, tol = 0.001)
    testthat::expect_equal(odds_to_d(r_to_odds(d_to_r(d = 1), log = TRUE), log = TRUE), 1, tolerance = 0.001)
  })
}