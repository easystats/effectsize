if (require("testthat") && require("effectsize")) {
  test_that("xtab", {
    xtab <- as.table(rbind(
      c(762, 327, 468),
      c(484, 239, 477),
      c(484, 239, 477)
    ))
    chisq <- chisq.test(xtab, correct = FALSE)

    res <- chisq_to_cramers_v(
      chisq$statistic,
      n = sum(xtab),
      nrow = nrow(xtab),
      ncol = ncol(xtab)
    )
    expect_equal(res, cramers_v(xtab), ignore_attr = TRUE)


    res <- chisq_to_phi(
      chisq$statistic,
      n = sum(xtab),
      nrow = nrow(xtab),
      ncol = ncol(xtab)
    )
    expect_equal(res, phi(xtab), ignore_attr = TRUE)
  })

  test_that("r", {
    res1 <- cor.test(iris[[1]], iris[[2]])

    res2 <- t_to_r(t = res1$statistic, res1$parameter)
    expect_equal(res2$r, res1$estimate, tolerance = 0.01, ignore_attr = TRUE)
    expect_equal(res2$CI_low, res1$conf.int[1], tolerance = 0.02, ignore_attr = TRUE)
    expect_equal(res2$CI_high, res1$conf.int[2], tolerance = 0.01, ignore_attr = TRUE)

    res3 <- F_to_r(res1$statistic^2, 1, res1$parameter)
    expect_equal(res3$r, -res1$estimate, tolerance = 0.01, ignore_attr = TRUE)
    expect_equal(res3$CI_low, -res1$conf.int[2], tolerance = 0.02, ignore_attr = TRUE)
    expect_equal(res3$CI_high, -res1$conf.int[1], tolerance = 0.02, ignore_attr = TRUE)
    expect_error(F_to_r(3, 2, 3))

    res4 <- z_to_r(res1$statistic, res1$parameter)
    expect_equal(res4$r, res1$estimate, tolerance = 0.01, ignore_attr = TRUE)
    expect_equal(res4$CI_low, res1$conf.int[1], tolerance = 0.02, ignore_attr = TRUE)
    expect_equal(res4$CI_high, res1$conf.int[2], tolerance = 0.02, ignore_attr = TRUE)
  })

  test_that("d", {
    res <- t_to_d(4, 68)
    expect_equal(res$d, 0.970, tolerance = 0.01)
    expect_equal(res$CI_low, 0.464, tolerance = 0.01)
    expect_equal(res$CI_high, 1.469, tolerance = 0.01)

    res <- t_to_d(4, 68, paired = TRUE)
    expect_equal(res$d, 0.970/2, tolerance = 0.01)
    expect_equal(res$CI_low, 0.464/2, tolerance = 0.01)
    expect_equal(res$CI_high, 1.469/2, tolerance = 0.01)

    res <- F_to_d(16, 1, 68)
    expect_equal(res$d, 0.970, tolerance = 0.01)
    expect_equal(res$CI_low, 0.464, tolerance = 0.01)
    expect_equal(res$CI_high, 1.469, tolerance = 0.01)
    expect_error(F_to_d(16, 2, 68))

    res <- z_to_d(4, 68)
    expect_equal(res$d, 0.970, tolerance = 0.01)
    expect_equal(res$CI_low, 0.494, tolerance = 0.01)
    expect_equal(res$CI_high, 1.446, tolerance = 0.01)

    # depr arg
    expect_warning(F_to_d(4^2, 1, 68, pooled = TRUE))
    expect_warning(t_to_d(4, 68, pooled = TRUE))
    expect_warning(z_to_d(4, 68, pooled = TRUE))
  })

  test_that("eta2", {
    res <- F_to_eta2(4, 3, 123)
    expect_equal(res[[1]], 0.089, tolerance = 0.01)
    expect_equal(res$CI_low, 0.014, tolerance = 0.02)
    expect_equal(res$CI_high, 0.163, tolerance = 0.01)
    expect_equal(t_to_eta2(2, 123), F_to_eta2(4, 1, 123))

    res <- F_to_epsilon2(4, 3, 123)
    expect_equal(res[[1]], 0.067, tolerance = 0.01)
    expect_equal(res$CI_low, 0.002, tolerance = 0.01)
    expect_equal(res$CI_high, 0.133, tolerance = 0.01)
    expect_equal(t_to_epsilon2(2, 123), F_to_epsilon2(4, 1, 123))

    res <- F_to_omega2(4, 3, 123)
    expect_equal(res[[1]], 0.066, tolerance = 0.01)
    expect_equal(res$CI_low, 0.002, tolerance = 0.01)
    expect_equal(res$CI_high, 0.132, tolerance = 0.01)
    expect_equal(t_to_epsilon2(2, 123), F_to_epsilon2(4, 1, 123))

    res <- F_to_eta2(4, 3, 123)
    resf2 <- F_to_f2(4, 3, 123)
    resf <- F_to_f(4, 3, 123)
    expect_equal(resf2[-2], res[-2] / (1 - res[-2]), ignore_attr = TRUE)
    expect_equal(resf[-2]^2, res[-2] / (1 - res[-2]), ignore_attr = TRUE)
    expect_equal(F_to_f(4, 3, 123), F_to_f2(4, 3, 123, squared = FALSE))
    expect_equal(F_to_f2(4, 3, 123), F_to_f(4, 3, 123, squared = TRUE))
  })
}
