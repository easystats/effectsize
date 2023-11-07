test_that("rm_d | paired data", {
  data("sleep")
  sleep2 <- reshape(sleep,
    direction = "wide",
    idvar = "ID", timevar = "group"
  )

  d1 <- rm_d(
    sleep$extra[sleep$group == 1],
    sleep$extra[sleep$group == 2]
  )
  d2 <- rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2)
  expect_no_message(d3 <- rm_d(extra ~ group | ID, data = sleep))

  expect_equal(d1, d2)
  expect_equal(d1, d3)

  d_z <- rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "z")
  d_z2 <- hedges_g(sleep$extra[sleep$group == 1] - sleep$extra[sleep$group == 2])

  expect_equal(d_z[[1]], d_z2[[1]])
  expect_equal(d_z$CI_low, d_z2$CI_low, tolerance = 0.01)
  expect_equal(d_z$CI_high, d_z2$CI_high, tolerance = 0.1)

  expect_error(
    rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "r"),
    regexp = "replications"
  )


  # equal with equal variance:
  dat <- data.frame(
    V1 = c(
      -0.32150435528124, -4.02978032779713, 0.159645811226589,
      1.95179927058772, 0.168527299289471, 3.4499229496418,
      -1.87888939495506, 0.431333839911973, -0.26004200470096,
      0.328986912076835
    ),
    V2 = c(
      2.50107579495566, -0.32926747002329, 4.01118960037018,
      1.44969284040984, -1.46241877172319, 1.25068499614667,
      0.028928469929524, 3.05532100575796, -1.67014771817319,
      3.16494125234984
    )
  )

  d_rm <- rm_d("V1", "V2", data = dat, adjust = FALSE)
  d_av <- rm_d("V1", "V2", data = dat, adjust = FALSE, method = "av")
  d_b <- rm_d("V1", "V2", data = dat, adjust = FALSE, method = "b")
  d_d <- rm_d("V1", "V2", data = dat, adjust = FALSE, method = "d")

  expect_equal(d_rm[[1]], d_d[[1]])
  expect_equal(d_av[[1]], d_d[[1]])
  expect_equal(d_b[[1]], d_d[[1]])
})

test_that("rm_d | replication data", {
  data("rouder2016")

  expect_message(
    rm_d(rt ~ cond | id, data = rouder2016),
    regexp = "requires paired data"
  )
  expect_no_message(
    rm_d(rt ~ cond | id, data = rouder2016, verbose = FALSE),
  )
  expect_no_message(
    rm_d(rt ~ cond | id, data = rouder2016, method = "d"),
  )

  d_av <- rm_d(rt ~ cond | id,
    data = rouder2016, method = "av",
    adjust = FALSE, verbose = FALSE
  )
  d_z <- rm_d(rt ~ cond | id,
    data = rouder2016, method = "z",
    adjust = FALSE, verbose = FALSE
  )
  d_d <- rm_d(rt ~ cond | id,
    data = rouder2016, method = "d",
    adjust = FALSE
  )
  d_r <- rm_d(rt ~ cond | id,
    data = rouder2016, method = "r",
    adjust = FALSE
  )

  # values takes from
  # https://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
  expect_equal(d_av[[1]], -0.8357347, tolerance = 0.001)
  expect_equal(d_z[[1]], -1.353713, tolerance = 0.001)
  expect_equal(d_d[[1]], -0.2497971, tolerance = 0.001)
  expect_equal(d_r[[1]], -0.052298 / 0.20195, tolerance = 0.001)

  # also:
  expect_equal(d_d[[1]], cohens_d(rt ~ cond, data = rouder2016)[[1]], tolerance = 0.001)

  rouder2016_wide <- tapply(rouder2016[["rt"]], rouder2016[1:2], mean, na.rm = TRUE)
  d_z2 <- cohens_d(rouder2016_wide[, 1] - rouder2016_wide[, 2])
  expect_equal(d_z[[1]], d_z2[[1]])
  expect_equal(d_z$CI_low, d_z2$CI_low, tolerance = 0.02)
  expect_equal(d_z$CI_high, d_z2$CI_high, tolerance = 0.02)

  d_rm <- rm_d(rt ~ cond | id, data = rouder2016, verbose = FALSE)
  d_rm2 <- rm_d(rouder2016_wide[, 1], rouder2016_wide[, 2])
  expect_equal(d_rm2, d_rm)


  # # compare CIs to lmeInfo::g_mlm
  # mod1 <- nlme::lme(rt ~ cond, random = ~ cond | id,
  #                   data = rouder2016,
  #                   control = nlme::nlmeControl(opt = "nlm"))
  # g_r <- lmeInfo::g_mlm(mod1, p_const = c(0, -1), r_const = c(0, 0, 0, 1))
  # g_r_ci <- lmeInfo::CI_g(g_r)
  # g_d <- lmeInfo::g_mlm(mod1, p_const = c(0, -1), r_const = c(1, 1, 1, 1))
  # g_d_ci <- lmeInfo::CI_g(g_d)

  d_r2 <- rm_d(rt ~ cond | id, data = rouder2016, method = "r")
  expect_equal(d_r2$CI_low, -0.342050038016442, tolerance = 0.05)
  expect_equal(d_r2$CI_high, -0.17575096606632, tolerance = 0.05)


  d_d2 <- rm_d(rt ~ cond | id, data = rouder2016, method = "d")
  expect_equal(d_d2$CI_low, -0.329333102235032, tolerance = 0.05)
  expect_equal(d_d2$CI_high, -0.168904897964021, tolerance = 0.05)
})
