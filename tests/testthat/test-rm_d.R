test_that("rm_d | paired data", {

  data("sleep")
  sleep2 <- reshape(sleep, direction = "wide",
                    idvar = "ID", timevar = "group")

  d1 <- rm_d(sleep$extra[sleep$group==1],
             sleep$extra[sleep$group==2])
  d2 <- rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2)
  expect_no_message(d3 <- rm_d(extra ~ group | ID, data = sleep))

  expect_equal(d1, d2)
  expect_equal(d1, d3)

  d_z <- rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "z")
  d_z2 <- hedges_g(sleep$extra[sleep$group==1] - sleep$extra[sleep$group==2])

  expect_equal(d_z[[1]], d_z2[[1]])
  expect_equal(d_z$CI_low, d_z2$CI_low, tolerance = 0.01)
  expect_equal(d_z$CI_high, d_z2$CI_high, tolerance = 0.1)

  expect_error(
    rm_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, method = "r"),
    regexp = "replications"
  )
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

  d_av <- rm_d(rt ~ cond | id, data = rouder2016, method = "av",
               adjust = FALSE, verbose = FALSE)
  d_z <- rm_d(rt ~ cond | id, data = rouder2016, method = "z",
              adjust = FALSE, verbose = FALSE)
  d_d <- rm_d(rt ~ cond | id, data = rouder2016, method = "d",
              adjust = FALSE)
  d_r <- rm_d(rt ~ cond | id, data = rouder2016, method = "r",
              adjust = FALSE)

  # values takes from
  # https://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
  expect_equal(d_av[[1]], -0.8357347, tolerance = 0.001)
  expect_equal(d_z[[1]], -1.353713, tolerance = 0.001)
  expect_equal(d_d[[1]], -0.2497971, tolerance = 0.001)
  expect_equal(d_r[[1]], -0.052298/0.20195, tolerance = 0.001)

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
