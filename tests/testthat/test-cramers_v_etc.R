if (require("testthat") && require("effectsize")) {
  test_that("contingency table", {
    ## Size does not affect estimate
    xtab <- rbind(c(760, 330, 470),
                  c(480, 240, 480),
                  c(480, 240, 480))

    cv1 <- cramers_v(xtab)
    cv2 <- cramers_v(xtab / 2)

    testthat::expect_equal(cv1$cramers_v, cv2$cramers_v)



    ## 2*2 tables return phi and cramers_v
    xtab <- rbind(c(760, 330),
                  c(480, 240))

    testthat::expect_equal(cramers_v(xtab)$cramers_v,
                           phi(xtab)$phi)


    ## 2*2 perfect correlation
    xtab <- rbind(c(100, 0),
                  c(0, 200))
    testthat::expect_equal(cramers_v(xtab)$cramers_v, 1)


    ## 2*2 0 correlation
    xtab <- rbind(c(50, 50),
                  c(100, 100))
    testthat::expect_equal(cramers_v(xtab)$cramers_v, 0)
  })
}