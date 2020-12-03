if (require("testthat") && require("effectsize")) {
  test_that("oddsratio_to_d", {
    testthat::expect_equal(oddsratio_to_d(0.2), -0.887, tolerance = 0.01)
    testthat::expect_equal(oddsratio_to_d(-1.45, log = TRUE), -0.7994, tolerance = 0.01)
    testthat::expect_equal(d_to_oddsratio(-0.887), 0.2, tolerance = 0.01)
    testthat::expect_equal(d_to_oddsratio(-0.7994, log = TRUE), -1.45, tolerance = 0.01)
  })


  test_that("d_to_r", {
    testthat::expect_equal(d_to_r(1.1547), 0.5, tolerance = 0.01)
    testthat::expect_equal(r_to_d(0.5), 1.1547, tolerance = 0.01)

    testthat::expect_equal(oddsratio_to_r(d_to_oddsratio(r_to_d(0.5))), 0.5, tolerance = 0.001)
    testthat::expect_equal(oddsratio_to_d(r_to_oddsratio(d_to_r(1), log = TRUE), log = TRUE), 1, tolerance = 0.001)
  })

  test_that("oddsratio_to_RR", {
    p0 <- 0.4
    p1 <- 0.7

    OR <- probs_to_odds(p1) / probs_to_odds(p0)
    RR <- p1 / p0

    testthat::expect_equal(riskratio_to_oddsratio(RR, p0 = p0), OR)
    testthat::expect_equal(oddsratio_to_riskratio(OR, p0 = p0), RR)
    testthat::expect_equal(oddsratio_to_riskratio(1 / OR, p0 = p1), 1 / RR)

    testthat::expect_equal(riskratio_to_oddsratio(log(RR), p0 = p0, log = TRUE), log(OR))
    testthat::expect_equal(oddsratio_to_riskratio(log(OR), p0 = p0, log = TRUE), log(RR))
  })
}
