if (require("testthat") && require("effectsize")) {
  test_that("rank_biserial", {
    skip_if_not_installed("boot")
    skip_if_not_installed("base", minimum_version = "3.6.0")
    x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
    y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
    rRB1 <- {
      set.seed(1)
      rank_biserial(x, y, paired = TRUE)
    }
    rRB2 <- {
      set.seed(1)
      rank_biserial(x - y)
    }
    expect_equal(rRB1, rRB2)
    expect_equal(rRB1[[1]], 0.777, tolerance = 0.01)
    expect_equal(rRB1$CI_low, 0.1977778, tolerance = 0.01)
    expect_equal(rRB1$CI_high, 1, tolerance = 0.01)


    A <- c(48, 48, 77, 86, 85, 85, 16)
    B <- c(14, 34, 34, 77)
    expect_equal(rank_biserial(A, B)[[1]], 0.6071429, tolerance = 0.01)
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
    expect_equal(E$CI_low, 0.01002212, tolerance = 0.01)
    expect_equal(E$CI_high, 0.7408143, tolerance = 0.01)
  })


  test_that("kendalls_w", {
    skip_if_not_installed("boot")
    skip_if_not_installed("base", minimum_version = "3.6.0")
    M1 <- structure(
      c(5.4, 5.85, 5.2, 5.5, 5.7, 5.6, 5.55, 5.75, 5.5),
      .Dim = c(3L, 3L),
      .Dimnames = list(
        c("1", "2", "3"),
        c("Round Out", "Narrow Angle", "Wide Angle")
      )
    )

    M2 <- structure(
      list(
        id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
        name = c(
          "Round Out", "Narrow Angle", "Wide Angle",
          "Round Out", "Narrow Angle", "Wide Angle",
          "Round Out", "Narrow Angle", "Wide Angle"
        ),
        value = c(5.4, 5.5, 5.55, 5.85, 5.7, 5.75, 5.2, 5.6, 5.5)
      ),
      row.names = c(NA, -9L),
      class = c("tbl_df", "tbl", "data.frame")
    )

    W1 <- {
      set.seed(1)
      kendalls_w(M1)
    }
    W2 <- {
      set.seed(1)
      kendalls_w(value ~ name | id, data = M2)
    }
    W3 <- {
      set.seed(1)
      kendalls_w(M2$value, M2$name, M2$id)
    }

    expect_equal(W1, W2)
    expect_equal(W1, W3)
    expect_equal(W1$CI_low, 0.7777778, tolerance = 0.01)
    expect_equal(W1$CI_high, 1, tolerance = 0.01)
  })
}
