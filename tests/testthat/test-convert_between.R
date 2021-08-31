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


    m <- glm(am ~ factor(cyl), data = mtcars,
             family = binomial())

    w <- capture_warnings(RR <- oddsratio_to_riskratio(m))
    expect_match(w[1],"p0")
    expect_match(w[2],"CIs")
    expect_true("(Intercept)" %in% RR$Parameter)
    expect_false("(p0)" %in% RR$Parameter)
    # these values confirmed from emmeans
    expect_equal(RR$Coefficient, c(0.7272, 0.5892, 0.1964), tolerance = 0.001)
    expect_equal(RR$CI_low, c(NA, 0.1118, 0.0232), tolerance = 0.001)
    expect_equal(RR$CI_high, c(NA, 1.157, 0.703), tolerance = 0.001)

    expect_warning(RR <- oddsratio_to_riskratio(m, p0 = 0.05), "CIs")
    expect_true("(p0)" %in% RR$Parameter)
    expect_false("(Intercept)" %in% RR$Parameter)
    # these values confirmed from emmeans
    expect_equal(RR$Coefficient, c(0.05, 0.29173, 0.06557), tolerance = 0.001)
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

  test_that("between anova", {
    expect_equal(eta2_to_f2(0.25), 1/3)
    expect_equal(eta2_to_f(0.25), sqrt(eta2_to_f2(0.25)))

    expect_equal(f2_to_eta2(1/3), 0.25)
    expect_equal(f_to_eta2(1/sqrt(3)), f2_to_eta2(1/3))
  })
}
