if (require("testthat") && require("effectsize")) {
  test_that("cohens_d", {

    # Direction ---------------------------------------------------------------
    rez_t <- t.test(iris$Sepal.Length, iris$Sepal.Width)
    rez_d <- cohens_d(iris$Sepal.Length, iris$Sepal.Width)
    testthat::expect_true(sign(rez_t$statistic) == sign(rez_d$Cohens_d))


    # Errors and warnings -----------------------------------------------------
    df <- data.frame(
      a = 1:10,
      b = 2:11,
      c = rep(letters[1:2], each = 5),
      d = sample(letters[1:3], 10, replace = T),
      e = rep(0:1, each = 5)
    )
    a2 <- 1:11

    testthat::expect_true({cohens_d(a ~ c, data = df); TRUE})
    testthat::expect_true({cohens_d("a", "c", data = df); TRUE})
    testthat::expect_true({cohens_d("a", "b", data = df); TRUE})
    testthat::expect_true({cohens_d(a2, df$b); TRUE})

    testthat::expect_error(cohens_d(a ~ b, data = df))
    testthat::expect_error(cohens_d(a ~ d, data = df))
    testthat::expect_error(cohens_d("a", "d", data = df))
    testthat::expect_error(cohens_d("c", "c", data = df))
    testthat::expect_error(cohens_d(a2, df$c))

    testthat::expect_warning(cohens_d("b", "e", data = df))
  })
}