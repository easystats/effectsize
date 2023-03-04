# library(testthat)

test_that("contingency table", {
  contingency_table <- as.table(rbind(
    c(762, 327, 468),
    c(484, 239, 477),
    c(484, 239, 477)
  ))
  res <- cramers_v(contingency_table, adjust = FALSE)

  expect_equal(res$Cramers_v, 0.072, tolerance = 0.01)
  expect_equal(res$CI_low, 0.051, tolerance = 0.01)
  expect_equal(res$CI_high, 1)

  expect_error(phi(contingency_table), "appropriate")

  expect_equal(tschuprows_t(contingency_table), res, ignore_attr = TRUE)

  ## Size does not affect estimate
  xtab <- rbind(
    c(760, 330, 470),
    c(480, 240, 480),
    c(480, 240, 480)
  )

  cv1 <- cramers_v(xtab, adjust = FALSE)
  cv2 <- cramers_v(xtab / 2, adjust = FALSE)

  expect_equal(cv1$Cramers_v, cv2$Cramers_v)

  # Upper bound of phi is the ratio between phi / V and sqrt(min(K,L)-1)
  expect_equal(cohens_w(xtab, alternative = "greater")$CI_high, sqrt(2))
  expect_equal(cohens_w(xtab)[[1]] / cramers_v(xtab, adjust = FALSE)[[1]], sqrt(2))

  # Tschuprows_t with non-square tables
  xtab <- rbind(
    c(9, 0, 1),
    c(0, 1, 0)
  )
  expect_equal(cramers_v(xtab, adjust = FALSE)[[1]], 1)
  expect_true(tschuprows_t(xtab)[[1]] < cramers_v(xtab, adjust = FALSE)[[1]])


  ## 2*2 tables return phi and cramers_v
  xtab <- rbind(
    c(760, 330),
    c(480, 240)
  )

  expect_equal(
    cramers_v(xtab, adjust = FALSE)[[1]],
    phi(xtab, adjust = FALSE)[[1]]
  )

  res <- pearsons_c(xtab)
  expect_equal(res[[1]], 0.032, tolerance = 0.01)


  ## 2*2 perfect correlation
  xtab <- rbind(
    c(100, 0),
    c(0, 200)
  )
  expect_equal(V <- cramers_v(xtab, adjust = FALSE)[[1]], 1)
  expect_true(pearsons_c(xtab)[[1]] < V) # C is not perfect


  ## 2*2 0 correlation
  xtab <- rbind(
    c(50, 50),
    c(100, 100)
  )
  expect_equal(cramers_v(xtab, adjust = FALSE)$Cramers_v, 0)


  ## Empty rows/columns
  xtab <- rbind(
    c(50, 50, 0),
    c(100, 100, 0)
  )
  expect_error(cramers_v(xtab, adjust = FALSE), "empty")

  ## 0
  xtab <- table(mtcars$am, mtcars$vs)
  phi3 <- phi(xtab, adjust = TRUE)
  expect_equal(phi3$phi_adjusted, 0)
  expect_equal(phi3$CI_low, 0)
  expect_equal(phi3$CI_high, 1)
})


test_that("goodness of fit", {
  expect_error(cramers_v(table(mtcars$cyl)), "goodness")

  w1 <- cohens_w(table(mtcars$cyl), p = c(0.34375, 0.21875, 0.43750))
  w2 <- cohens_w(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

  Fei1 <- fei(table(mtcars$cyl), p = c(0.34375, 0.21875, 0.43750))
  Fei2 <- fei(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

  expect_equal(w1[[1]], 0)
  expect_true(w1[[1]] < w2[[1]])
  expect_true(Fei1[[1]] < Fei2[[1]])
  expect_true(Fei2[[1]] < w2[[1]])
  expect_equal(w2[[1]] * sqrt(0.1 / 0.9), Fei2[[1]])
  expect_true(w1$CI_low < w2$CI_low)
  expect_true(w2$CI_low < w2$CI_high)
  expect_equal(w2$CI_high, sqrt(0.9 / 0.1))

  C <- pearsons_c(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))
  expect_equal(C[[1]], sqrt(49.289 / (49.289 + sum(table(mtcars$cyl)))), tolerance = 0.001)
  expect_equal(C$CI_high, 1)

  # some weird exeptions...
  df <- subset(mtcars, am == "0")
  expect_equal(cohens_w(table(df$am, df$cyl))[[1]], 0.64, tolerance = 0.01)
  expect_equal(cohens_w(table(df$am, df$cyl)), cohens_w(table(df$cyl)))
  expect_equal(cohens_w(table(df$am, df$cyl)), cohens_w(table(df$cyl, df$am)))

  # p is a table
  O <- as.table(c(10, 20, 30, 40))
  E_vec <- c(11, 13, 44, 23)
  E_tab <- as.table(E_vec)

  expect_equal(
    cohens_w(O, p = E_vec, rescale.p = TRUE),
    cohens_w(O, p = E_tab, rescale.p = TRUE)
  )
  expect_equal(
    fei(O, p = E_vec, rescale.p = TRUE),
    fei(O, p = E_tab, rescale.p = TRUE)
  )
  expect_equal(
    pearsons_c(O, p = E_vec, rescale.p = TRUE),
    pearsons_c(O, p = E_tab, rescale.p = TRUE)
  )
})

test_that("oddsratio & riskratio", {
  ## Risk ratio
  RCT <- rbind(
    c(30, 71),
    c(100, 50)
  )
  OR <- oddsratio(RCT)
  RR <- riskratio(RCT)
  ARR <- arr(RCT)
  NNT <- nnt(RCT)
  p0 <- RCT[1, 2] / sum(RCT[, 2])

  expect_lt(NNT$CI_low, NNT$CI_high)
  expect_lt(NNT$CI_high, 0)

  NNT_0 <- nnt(RCT / 9.8)
  expect_lt(NNT_0$CI_low, NNT_0$CI_high)
  expect_lt(NNT_0$CI_low, 0)
  expect_gt(NNT_0$CI_high, 0)

  expect_equal(
    oddsratio_to_riskratio(OR$Odds_ratio, p0),
    RR$Risk_ratio
  )
  expect_equal(
    riskratio_to_oddsratio(RR$Risk_ratio, p0),
    OR$Odds_ratio
  )
  expect_equal(
    oddsratio_to_arr(OR$Odds_ratio, p0),
    ARR$ARR
  )

  expect_error(riskratio(RCT, log = TRUE), NA)


  ## OR
  data("mtcars")
  expect_error(oddsratio(mtcars$am, mtcars$cyl), "only")

  mtcars$Ind <- mtcars$cyl > 4
  # m <- glm(am ~ Ind, data = mtcars, family = binomial())

  # confirmed by emmeans
  or <- oddsratio(mtcars$am, mtcars$Ind)
  expect_equal(or[[1]], 0.1171875, tolerance = 0.001)
  expect_equal(or$CI_low, 0.02219195, tolerance = 0.001)
  expect_equal(or$CI_high, 0.6188238, tolerance = 0.001)

  ARR <- arr(mtcars$am, mtcars$Ind)
  expect_equal(ARR[[1]], -0.4891775, tolerance = 0.001)
  expect_equal(ARR$CI_low, -0.8092576, tolerance = 0.001)
  expect_equal(ARR$CI_high, -0.1690974, tolerance = 0.001)
})


testthat("fei() for 1D tables", {
  data(Titanic)
  Titanic_xtab <- as.table(apply(Titanic, c(2, 4), sum))
  expect_error(fei(Titanic_xtab))
  testthat::expect_no_error(fei(as.matrix(1:10)))
})
