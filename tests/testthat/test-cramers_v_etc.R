if (require("testthat") && require("effectsize")) {
  test_that("contingency table", {
    contingency_table <- as.table(rbind(
      c(762, 327, 468),
      c(484, 239, 477),
      c(484, 239, 477)
    ))
    res <- cramers_v(contingency_table)

    expect_equal(res$Cramers_v, 0.072, tolerance = 0.01)
    expect_equal(res$CI_low, 0.047, tolerance = 0.01)
    expect_equal(res$CI_high, 0.091, tolerance = 0.01)


    ## Size does not affect estimate
    xtab <- rbind(
      c(760, 330, 470),
      c(480, 240, 480),
      c(480, 240, 480)
    )

    cv1 <- cramers_v(xtab)
    cv2 <- cramers_v(xtab / 2)

    expect_equal(cv1$Cramers_v, cv2$Cramers_v)



    ## 2*2 tables return phi and cramers_v
    xtab <- rbind(
      c(760, 330),
      c(480, 240)
    )

    expect_equal(
      cramers_v(xtab)$Cramers_v,
      phi(xtab)$phi
    )


    ## 2*2 perfect correlation
    xtab <- rbind(
      c(100, 0),
      c(0, 200)
    )
    expect_equal(cramers_v(xtab)$Cramers_v, 1)


    ## 2*2 0 correlation
    xtab <- rbind(
      c(50, 50),
      c(100, 100)
    )
    expect_equal(cramers_v(xtab)$Cramers_v, 0)


    ## Empty rows/columns
    xtab <- rbind(
      c(50, 50, 0),
      c(100, 100, 0)
    )
    expect_error(cramers_v(xtab))
  })


  test_that("goodness of fit", {
    cv1 <- cramers_v(table(mtcars$cyl), p = c(0.34375, 0.21875, 0.43750))
    cv2 <- cramers_v(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    expect_equal(cv1$Cramers_v, 0)
    expect_true(cv1$Cramers_v < cv2$Cramers_v)
    expect_true(cv2$CI_low < cv2$CI_high)

    phi1 <- phi(table(mtcars$cyl), p = c(0.34375, 0.21875, 0.43750))
    phi2 <- phi(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    expect_equal(phi1$phi, 0)
    expect_true(phi1$phi < phi2$phi)
    expect_true(phi2$CI_low < phi2$CI_high)

    # some weird exeptions...
    df <- subset(mtcars, am == "0")
    expect_equal(cramers_v(table(df$am, df$cyl))[[1]], 0.45, tolerance = 0.01)
    expect_equal(cramers_v(table(df$am, df$cyl)), cramers_v(table(df$cyl)))
    expect_equal(cramers_v(table(df$am, df$cyl)), cramers_v(table(df$cyl, df$am)))

    xtab <- table(mtcars$am, mtcars$vs)
    V <- cramers_v(xtab, adjust = TRUE)
    expect_equal(unname(unlist(V[c(1, 3, 4)])), rep(0, 3))
  })

  test_that("oddsratio & riskratio", {
    ## Risk ratio
    RCT <- rbind(
      c(30, 71),
      c(100, 50)
    )
    OR <- oddsratio(RCT)
    RR <- riskratio(RCT)
    p0 <- RCT[1, 2] / sum(RCT[, 2])

    expect_equal(
      oddsratio_to_riskratio(OR$Odds_ratio, p0),
      RR$Risk_ratio
    )
    expect_equal(
      riskratio_to_oddsratio(RR$Risk_ratio, p0),
      OR$Odds_ratio
    )


    ## OR
    data("mtcars")
    expect_error(oddsratio(mtcars$am, mtcars$cyl))

    m <- glm(am ~ I(cyl > 4), data = mtcars, family = binomial())
    log_or <- oddsratio(mtcars$am, mtcars$cyl > 4, log = TRUE)

    expect_equal(coef(m)[2], log_or$log_Odds_ratio,
      ignore_attr = TRUE
    )

    expect_equal(log_or, oddsratio(mtcars$cyl > 4, mtcars$am, log = TRUE))

    skip_if_not_installed("MASS")
    expect_equal(confint(m)[2, ],
      unlist(log_or[c("CI_low", "CI_high")]),
      tolerance = 0.1, # different methods, give slightly different values
      ignore_attr = TRUE
    )
  })


  test_that("Cohen's g", {

    # From mcnemar.test
    Performance <-
      matrix(c(794, 86, 150, 570),
        nrow = 2,
        dimnames = list(
          "1st Survey" = c("Approve", "Disapprove"),
          "2nd Survey" = c("Approve", "Disapprove")
        )
      )
    g <- cohens_g(Performance)
    expect_equal(g$Cohens_g, 0.136, tolerance = 0.01)
    expect_equal(g$CI_low, 0.072, tolerance = 0.01)
    expect_equal(g$CI_high, 0.194, tolerance = 0.01)


    AndersonRainBarrel <- matrix(c(
      9L, 17L,
      5L, 15L
    ), nrow = 2)
    g <- cohens_g(AndersonRainBarrel)
    expect_equal(g$Cohens_g, 0.273, tolerance = 0.01)
    expect_equal(g$CI_low, 0.066, tolerance = 0.01)
    expect_equal(g$CI_high, 0.399, tolerance = 0.01)


    M <- matrix(c(
      794, 86, 150,
      570, 794, 86,
      150, 570, 15
    ),
    nrow = 3
    )
    g <- cohens_g(M)
    expect_equal(g$Cohens_g, 0.300, tolerance = 0.01)
    expect_equal(g$CI_low, 0.280, tolerance = 0.01)
    expect_equal(g$CI_high, 0.319, tolerance = 0.01)
  })
}
