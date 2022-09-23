if (require("testthat") && require("effectsize")) {
  test_that("rank_biserial", {
    x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
    y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

    rRB1 <- rank_biserial(x, y, paired = TRUE)
    rRB2 <- rank_biserial(x - y)

    expect_equal(rRB1, rRB2)
    expect_equal(rRB1[[1]], 0.777, tolerance = 0.01)
    expect_equal(rRB1$CI_low, 0.2953631, tolerance = 0.01)
    expect_equal(rRB1$CI_high, 0.9441559, tolerance = 0.01)


    A <- c(48, 48, 77, 86, 85, 85, 16)
    B <- c(14, 34, 34, 77)
    expect_equal(rank_biserial(A, B)[[1]], 0.6071429, tolerance = 0.01)


    df <- data.frame(
      outcome = c(x, y),
      g = factor(rep(0:1, each = 9))
    )
    expect_equal(
      rank_biserial(outcome ~ g, data = df, ci = NULL),
      rank_biserial(df$outcome ~ df$g, ci = NULL)
    )
    expect_equal(
      rank_biserial(outcome ~ g, data = df, ci = NULL),
      rank_biserial("outcome", "g", data = df, ci = NULL)
    )
  })


  test_that("rank_epsilon_squared", {
    skip_if_not_installed("boot")
    skip_if_not_installed("base", minimum_version = "3.6.0")
    x1 <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
    x2 <- c(3.8, 2.7, 4.0, 2.4) # with obstructive airway disease
    x3 <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
    x <- c(x1, x2, x3)
    g <- factor(rep(1:3, c(5, 4, 5)))

    set.seed(1)
    E <- rank_epsilon_squared(x, g)

    expect_equal(E[[1]], 0.05934066, tolerance = 0.01)
    expect_equal(E$CI_low, 0.01726463, tolerance = 0.01)
    expect_equal(E$CI_high, 1)

    expect_equal(
      rank_epsilon_squared(x ~ g, ci = NULL),
      rank_epsilon_squared(x, g, ci = NULL)
    )
  })


  test_that("kendalls_w", {
    skip_if_not_installed("boot")
    skip_if_not_installed("base", minimum_version = "3.6.0")
    M1 <- cbind(
      "Round Out" = c(5.4, 5.85, 5.2),
      "Narrow Angle" = c(5.5, 5.7, 5.6),
      "Wide Angle" = c(5.55, 5.75, 5.5)
    )

    M2 <- data.frame(
      id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
      name = c(
        "Round Out", "Narrow Angle", "Wide Angle",
        "Round Out", "Narrow Angle", "Wide Angle",
        "Round Out", "Narrow Angle", "Wide Angle"
      ),
      value = c(5.4, 5.5, 5.55, 5.85, 5.7, 5.75, 5.2, 5.6, 5.5)
    )

    set.seed(1)
    W1 <- kendalls_w(M1)
    W2 <- kendalls_w(value ~ name | id, data = M2, ci = NULL)
    W3 <- kendalls_w(M2$value, M2$name, M2$id, ci = NULL)
    W4 <- kendalls_w(M2$value ~ M2$name | M2$id, ci = NULL)

    expect_equal(W1[[1]], W2[[1]])
    expect_equal(W1[[1]], W3[[1]])
    expect_equal(W1[[1]], W4[[1]])
    expect_equal(W1[[1]], 0.11111111, tolerance = 0.01)
    expect_equal(W1$CI_low, 0.11111111, tolerance = 0.01)
    expect_equal(W1$CI_high, 1, tolerance = 0.01)

    # Ties
    dat <- data.frame(
      pno = c(
        1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L,
        3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 7L, 7L,
        7L, 7L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L
      ),
      condition = c(
        1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L,
        1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L,
        1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
        2L
      ),
      congruency = c(
        1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
        1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
        2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
        1L, 2L
      ),
      mrt = c(
        0.86, 0.86, 0.86, 0.78, 0.56, 0.56, 0.59, 0.66, 0.48,
        0.5, 0.47, 0.51, 0.48, 0.52, 0.45, 0.47, 0.65, 0.79, 0.7,
        0.81, 0.58, 0.6, 0.57, 0.6, 0.53, 0.61, 0.47, 0.49, 0.56,
        0.64, 0.56, 0.6, 0.56, 0.66, 0.59, 0.63, 0.7, 0.92, 0.8,
        0.96
      )
    )
    dat

    W <- kendalls_w(mrt ~ interaction(condition, congruency) | pno, data = dat, verbose = FALSE)
    expect_equal(W[[1]], 0.4011, tolerance = 0.01)



    # singular ties
    m <- rbind(
      c(1, 2, 3, 4),
      c(7, 7, 7, 7), # THIS
      c(2, 3, 1, 4)
    )

    expect_warning(kendalls_w(m, ci = NULL), "contain ties")
    expect_warning(W <- kendalls_w(m, ci = NULL), "unique ranking")
    expect_equal(W[[1]], 0.4666667, tolerance = 0.001)
    expect_equal(kendalls_w(t(m), blocks_on_rows = FALSE, ci = NULL, verbose = FALSE)[[1]], W[[1]])
  })
}

test_that("wmw_odds", {
  x1 = subset(sleep, group == 1)$extra
  y1 = subset(sleep, group == 2)$extra

  wmw1 = wmw_odds(x = x1,
                y = y1)
  wmw2 = wmw_odds(extra ~ group, data = sleep)

  expect_equal(wmw1, wmw2)
  expect_equal(wmw1[[1]], 0.34, tolerance = 0.01)
  expect_equal(wmw1$CI_low, 0.1118928, tolerance = 0.01)
  expect_equal(wmw1$CI_high, 1.047046, tolerance = 0.01)


  A <- c(48, 48, 77, 86, 85, 85, 16)
  B <- c(14, 34, 34, 77)
  expect_equal(wmw_odds(A, B)[[1]], 4.09, tolerance = 0.01)

  x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
  y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
  df <- data.frame(
    outcome = c(x, y),
    g = factor(rep(0:1, each = 9))
  )
  expect_equal(
    wmw_odds(outcome ~ g, data = df, ci = NULL),
    wmw_odds(df$outcome ~ df$g, ci = NULL)
  )
  expect_equal(
    wmw_odds(outcome ~ g, data = df, ci = NULL),
    wmw_odds("outcome", "g", data = df, ci = NULL)
  )
})
