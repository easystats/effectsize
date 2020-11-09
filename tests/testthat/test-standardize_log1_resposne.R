if (require("testthat") && require("effectsize")) {
  if (packageVersion("insight") > "0.10.0") {
    d <- data.frame(
      time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
      group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
      sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50)
    )
    m <-  lm(log(sum + 1) ~ as.numeric(time) * group, data = d)
    test_that("standardize log(x+1) response", {
      out <- suppressMessages(standardize(m))
      expect_equal(coef(m), c(`(Intercept)` = -0.4575, `as.numeric(time)` = 0.5492, group = 0.3379,
                              `as.numeric(time):group` = 0.15779), tolerance = 0.01)
    })
  }
}