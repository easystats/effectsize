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


  M <- matrix(
    c(
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
