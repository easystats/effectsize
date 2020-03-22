test_that("r", {
  res1 <- cor.test(iris[[1]], iris[[2]])
  res2 <- t_to_r(t = res1$statistic, res1$parameter)

  testthat::expect_equal(unname(res1$estimate), res2$r, tolerance = 0.01)
  testthat::expect_equal(unname(res1$conf.int[1]), res2$CI_low, tolerance = 0.01)
  testthat::expect_equal(unname(res1$conf.int[2]), res2$CI_high, tolerance = 0.01)
})


if (require("DescTools")) {
  test_that("Cramers V", {
    contingency_table <- as.table(rbind(c(762, 327, 468),
                                        c(484, 239, 477),
                                        c(484, 239, 477)))

    res1 <- DescTools::CramerV(contingency_table, conf.level = 0.95, method = "ncchisq")
    res2 <- effectsize::cramers_v(contingency_table)

    testthat::expect_equal(unname(res1[1]), res2$cramers_v, tolerance = 0.01)
    testthat::expect_equal(unname(res1[2]), res2$CI_low, tolerance = 0.01)
    testthat::expect_equal(unname(res1[3]), res2$CI_high, tolerance = 0.01)
  })
}

if (require("MBESS", quietly = TRUE)) {
  test_that("d", {
    res1 <- unlist(MBESS::ci.smd(4, n.1 = 40, n.2 = 30))
    res2 <- t_to_d(4, 68)

    testthat::expect_equal(unname(res1[2]), res2$d, tolerance = 0.01)
    testthat::expect_equal(unname(res1[1]), res2$CI_low, tolerance = 0.01)
    testthat::expect_equal(unname(res1[3]), res2$CI_high, tolerance = 0.01)
  })


  test_that("eta2", {
    res1 <- unlist(MBESS::ci.pvaf(4, 3, 123, N = 127, conf.level = 0.9))
    res2 <- F_to_eta2(4, 3, 123)

    # no equvilant for actual eta2
    testthat::expect_equal(unname(res1[1]), res2$CI_low, tolerance = 0.01)
    testthat::expect_equal(unname(res1[3]), res2$CI_high, tolerance = 0.01)
  })
}

