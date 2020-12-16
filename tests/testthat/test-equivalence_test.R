if (require("testthat") && require("effectsize")) {
  test_that("equivalence_test", {
    ds <- t_to_d(
      t = c(0.45, -0.65, 7, -2.2, 2.25),
      df_error = c(675, 525, 2000, 900, 1875),
      ci = 0.9
    ) # TOST approach


    testthat::expect_equal(
      equivalence_test(ds, range = 0.2)$ROPE_Equivalence,
      c("Accepted", "Undecided", "Rejected", "Rejected", "Accepted")
    )

    testthat::expect_equal(
      equivalence_test(ds, range = 0.2, rule = "cet")$ROPE_Equivalence,
      c("Accepted", "Undecided", "Rejected", "Rejected", "Rejected")
    )

    testthat::expect_equal(
      equivalence_test(ds, range = 0.2, rule = "bayes")$ROPE_Equivalence,
      c("Accepted", "Undecided", "Rejected", "Undecided", "Accepted")
    )
  })
}
