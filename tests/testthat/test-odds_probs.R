if (require("testthat") && require("effectsize")) {
  test_that("odds_to_probs", {
    expect_equal(odds_to_probs(3), 0.75, tolerance = 0.01)
    expect_equal(probs_to_odds(0.75), 3, tolerance = 0.01)
    expect_equal(probs_to_odds(0.75, log = TRUE), 1.098, tolerance = 0.01)
    expect_equal(odds_to_probs(1.098, log = TRUE), 0.75, tolerance = 0.01)



    expect_equal(
      ncol(df <- odds_to_probs(
        iris,
        select = c("Sepal.Length"),
        exclude = c("Petal.Length"),
        log = TRUE
      )), 5
    )

    expect_equal(
      ncol(probs_to_odds(
        df,
        select = c("Sepal.Length"),
        exclude = c("Petal.Length"),
        log = TRUE
      )), 5
    )
  })
}
