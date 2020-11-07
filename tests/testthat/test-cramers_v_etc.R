if (require("testthat") && require("effectsize")) {
  test_that("contingency table", {
    ## Size does not affect estimate
    xtab <- rbind(c(760, 330, 470),
                  c(480, 240, 480),
                  c(480, 240, 480))

    cv1 <- cramers_v(xtab)
    cv2 <- cramers_v(xtab / 2)

    testthat::expect_equal(cv1$Cramers_v, cv2$Cramers_v)



    ## 2*2 tables return phi and cramers_v
    xtab <- rbind(c(760, 330),
                  c(480, 240))

    testthat::expect_equal(cramers_v(xtab)$Cramers_v,
                           phi(xtab)$phi)


    ## 2*2 perfect correlation
    xtab <- rbind(c(100, 0),
                  c(0, 200))
    testthat::expect_equal(cramers_v(xtab)$Cramers_v, 1)


    ## 2*2 0 correlation
    xtab <- rbind(c(50, 50),
                  c(100, 100))
    testthat::expect_equal(cramers_v(xtab)$Cramers_v, 0)
  })


  test_that("goodness of fit", {
    cv1 <- cramers_v(table(mtcars$cyl), p = c(0.34375, 0.21875 , 0.43750))
    cv2 <- cramers_v(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    testthat::expect_equal(cv1$Cramers_v, 0)
    testthat::expect_true(cv1$Cramers_v < cv2$Cramers_v)
    testthat::expect_true(cv2$CI_low < cv2$CI_high)

    phi1 <- phi(table(mtcars$cyl), p = c(0.34375, 0.21875 , 0.43750))
    phi2 <- phi(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    testthat::expect_equal(phi1$phi, 0)
    testthat::expect_true(phi1$phi < phi2$phi)
    testthat::expect_true(phi2$CI_low < phi2$CI_high)

    # some weird exeptions...
    df <- subset(mtcars, am == "0")
    testthat::expect_equal(cramers_v(table(df$am, df$cyl))[[1]], 0.45, tol = 0.01)
    testthat::expect_equal(cramers_v(table(df$am, df$cyl)), cramers_v(table(df$cyl)))
    testthat::expect_equal(cramers_v(table(df$am, df$cyl)), cramers_v(table(df$cyl, df$am)))

    xtab <- table(mtcars$am, mtcars$vs)
    V <- cramers_v(xtab, adjust = TRUE)
    testthat::expect_equal(unname(unlist(V[c(1,3,4)])), rep(0,3))
  })

  test_that("oddsratio & riskratio", {
    data("mtcars")
    testthat::expect_error(oddsratio(mtcars$am, mtcars$cyl))

    m <- glm(am ~ I(cyl > 4), data = mtcars, family = binomial())
    log_or <- oddsratio(mtcars$am, mtcars$cyl > 4, log = TRUE)

    testthat::expect_equal(coef(m)[2], -log_or$log_Odds_ratio,
                           check.attributes = FALSE)
    testthat::expect_equal(-rev(confint(m)[2,]),
                           unlist(log_or[c("CI_low", "CI_high")]),
                           tol = 0.1, # different methods, give slightly different values
                           check.attributes = FALSE)

    testthat::expect_equal(log_or, oddsratio(mtcars$cyl > 4, mtcars$am, log = TRUE))

    ## Risk ratio
    RCT <- rbind(c(30,  71),
                 c(100, 50))
    OR <- oddsratio(RCT)
    RR <- riskratio(RCT)
    p0 <- 30 / 130

    testthat::expect_equal(oddsratio_to_riskratio(OR$Odds_ratio, p0),
                           RR$Risk_ratio)
    testthat::expect_equal(riskratio_to_oddsratio(RR$Risk_ratio, p0),
                           OR$Odds_ratio)
  })


  test_that("Cohen's g", {

    # From mcnemar.test
    Performance <-
      matrix(c(794, 86, 150, 570),
             nrow = 2,
             dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                             "2nd Survey" = c("Approve", "Disapprove")))
    g <- cohens_g(Performance)
    testthat::expect_equal(g$Cohens_g, 0.136, tol = 0.01)
    testthat::expect_equal(g$CI_low, 0.072, tol = 0.01)
    testthat::expect_equal(g$CI_high, 0.194, tol = 0.01)


    AndersonRainBarrel <- matrix(c(9L, 17L,
                                   5L, 15L), nrow = 2)
    g <- cohens_g(AndersonRainBarrel)
    testthat::expect_equal(g$Cohens_g, 0.273, tol = 0.01)
    testthat::expect_equal(g$CI_low, 0.066, tol = 0.01)
    testthat::expect_equal(g$CI_high, 0.399, tol = 0.01)


    M <- matrix(c(794, 86, 150,
                  570, 794, 86,
                  150, 570, 15),
                nrow = 3)
    g <- cohens_g(M)
    testthat::expect_equal(g$Cohens_g, 0.300, tol = 0.01)
    testthat::expect_equal(g$CI_low, 0.280, tol = 0.01)
    testthat::expect_equal(g$CI_high, 0.319, tol = 0.01)
  })

}