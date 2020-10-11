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


  test_that("goodness of fit", {

    cv1 <- cramers_v(table(mtcars$cyl), p = c(0.34375, 0.21875 , 0.43750))
    cv2 <- cramers_v(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    testthat::expect_equal(cv1$cramers_v, 0)
    testthat::expect_true(cv1$cramers_v < cv2$cramers_v)
    testthat::expect_true(cv2$CI_low < cv2$CI_high)

    phi1 <- phi(table(mtcars$cyl), p = c(0.34375, 0.21875 , 0.43750))
    phi2 <- phi(table(mtcars$cyl), p = c(0.8, 0.1, 0.1))

    testthat::expect_equal(phi1$phi, 0)
    testthat::expect_true(phi1$phi < phi2$phi)
    testthat::expect_true(phi2$CI_low < phi2$CI_high)

  })
}