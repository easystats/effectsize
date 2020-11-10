if (require("testthat") && require("effectsize")) {
  test_that("r", {
    res1 <- cor.test(iris[[1]], iris[[2]])
    res2 <- t_to_r(t = res1$statistic, res1$parameter)

    testthat::expect_equal(unname(res1$estimate), res2$r, tolerance = 0.01)
    testthat::expect_equal(unname(res1$conf.int[1]), res2$CI_low, tolerance = 0.02)
    testthat::expect_equal(unname(res1$conf.int[2]), res2$CI_high, tolerance = 0.01)
  })


  test_that("Cramers V", {
    contingency_table <- as.table(rbind(c(762, 327, 468),
                                        c(484, 239, 477),
                                        c(484, 239, 477)))
    res <- cramers_v(contingency_table)

    testthat::expect_equal(res$Cramers_v, 0.072, tolerance = 0.01)
    testthat::expect_equal(res$CI_low, 0.047, tolerance = 0.01)
    testthat::expect_equal(res$CI_high, 0.091, tolerance = 0.01)
  })


  test_that("d", {
    res <- t_to_d(4, 68)

    testthat::expect_equal(res$d, 0.970, tolerance = 0.01)
    testthat::expect_equal(res$CI_low, 0.464, tolerance = 0.01)
    testthat::expect_equal(res$CI_high, 1.469, tolerance = 0.01)
  })

  test_that("eta2", {
    res <- F_to_eta2(4, 3, 123)

    testthat::expect_equal(res$Eta2_partial, 0.089, tolerance = 0.01)
    testthat::expect_equal(res$CI_low, 0.014, tolerance = 0.02)
    testthat::expect_equal(res$CI_high, 0.163, tolerance = 0.01)

    resf2 <- F_to_f2(4, 3, 123)
    resf <- F_to_f(4, 3, 123)
    testthat::expect_equal(as.data.frame(unname(resf2[-2])),
                           unname(res[-2] / (1-res[-2])))
    testthat::expect_equal(as.data.frame(unname(resf[-2])),
                           unname(sqrt(res[-2] / (1-res[-2]))))
    testthat::expect_equal(F_to_f(4, 3, 123), F_to_f2(4, 3, 123, squared = FALSE))
    testthat::expect_equal(F_to_f2(4, 3, 123), F_to_f(4, 3, 123, squared = TRUE))
  })
}
