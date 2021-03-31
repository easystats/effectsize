if (require("testthat") && require("effectsize") && require("tidymodels", quietly = TRUE)) {
  test_that("interpret", {
    set.seed(123)
    mod_lm <-
      linear_reg() %>%
      set_engine("lm") %>%
      set_mode("regression") %>%
      fit(mpg ~ am + vs, data = mtcars)

    set.seed(123)
    df_lm <- as.data.frame(eta_squared(mod_lm, ci = 0.95))

    expect_equal(
      df_lm,
      structure(
        list(
          Parameter = c("am", "vs"),
          Eta2_partial = c(0.534050980382337, 0.509657899929074),
          CI = c(0.95, 0.95),
          CI_low = c(0.270477566572333, 0.242410781810851),
          CI_high = c(0.698019260414593, 0.681154789342689)
        ),
        class = "data.frame",
        row.names = 1:2,
        partial = TRUE,
        generalized = FALSE,
        ci = 0.95,
        ci_method = list(method = "ncp", distribution = "t")
      ),
      tolerance = 0.001
    )
  })
}
