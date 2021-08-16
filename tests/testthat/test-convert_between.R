if (require("testthat") && require("effectsize")) {
  test_that("oddsratio_to_d", {
    expect_equal(oddsratio_to_d(0.2), -0.887, tolerance = 0.01)
    expect_equal(oddsratio_to_d(-1.45, log = TRUE), -0.7994, tolerance = 0.01)
    expect_equal(d_to_oddsratio(-0.887), 0.2, tolerance = 0.01)
    expect_equal(d_to_oddsratio(-0.7994, log = TRUE), -1.45, tolerance = 0.01)
  })


  test_that("d_to_r", {
    expect_equal(d_to_r(1.1547), 0.5, tolerance = 0.01)
    expect_equal(r_to_d(0.5), 1.1547, tolerance = 0.01)

    expect_equal(oddsratio_to_r(d_to_oddsratio(r_to_d(0.5))), 0.5, tolerance = 0.001)
    expect_equal(oddsratio_to_d(r_to_oddsratio(d_to_r(1), log = TRUE), log = TRUE), 1, tolerance = 0.001)
  })

  test_that("oddsratio_to_RR", {
    p0 <- 0.4
    p1 <- 0.7

    OR <- probs_to_odds(p1) / probs_to_odds(p0)
    RR <- p1 / p0

    expect_equal(riskratio_to_oddsratio(RR, p0 = p0), OR)
    expect_equal(oddsratio_to_riskratio(OR, p0 = p0), RR)
    expect_equal(oddsratio_to_riskratio(1 / OR, p0 = p1), 1 / RR)

    expect_equal(riskratio_to_oddsratio(log(RR), p0 = p0, log = TRUE), log(OR))
    expect_equal(oddsratio_to_riskratio(log(OR), p0 = p0, log = TRUE), log(RR))
  })

  test_that("odds_to_probs", {
    expect_equal(odds_to_probs(3), 0.75, tolerance = 0.01)
    expect_equal(probs_to_odds(0.75), 3, tolerance = 0.01)
    expect_equal(probs_to_odds(0.75, log = TRUE), 1.098, tolerance = 0.01)
    expect_equal(odds_to_probs(1.098, log = TRUE), 0.75, tolerance = 0.01)

    expect_equal(
      ncol(df <- odds_to_probs(
        iris,
        select = c("Sepal.Length"),
        exclude = c("Petal.Length"),
        log = TRUE
      )), 5
    )

    expect_equal(
      ncol(probs_to_odds(
        df,
        select = c("Sepal.Length"),
        exclude = c("Petal.Length"),
        log = TRUE
      )), 5
    )
  })
}
