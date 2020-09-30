if (require("testthat") && require("effectsize")) {

  set.seed(1234)
  data(iris)

  # data setup
  iris$weight_me <- rnorm(nrow(iris), 1, .3)
  iris$Sepal.Length[sample(1:nrow(iris), size = 10)] <- NA
  iris$weight_me[sample(1:nrow(iris), size = 10)] <- NA
  iris$Species[sample(1:nrow(iris), size = 8)] <- NA

  # standardize 2nd data set
  iris2 <- iris
  iris2[c("Sepal.Length", "Petal.Width")] <- standardize(iris2[c("Sepal.Length", "Petal.Width")])
  iris3 <- iris
  iris3[c("Sepal.Length", "Petal.Width")] <- standardize(iris3[c("Sepal.Length", "Petal.Width")], weights = iris3$weight_me)

  model1 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me)
  model2 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris2, weights = weight_me)
  model3 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris2)
  model4 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris3, weights = weight_me)
  model5 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris3)

  sm1 <- standardize(model1, weights = TRUE)
  sm2 <- standardize(model1, weights = FALSE)

  test_that("standardize-weight", {
    expect_equal(unname(coef(sm1)), c(-0.01782, -0.091, 0.14961, 0.79504), tolerance = 1e-3)
    expect_equal(unname(coef(sm2)), c(-0.01847, -0.09095, 0.14953, 0.80802), tolerance = 1e-3)
    expect_equal(unname(coef(model2)), c(-0.01782, -0.091, 0.14961, 0.79504), tolerance = 1e-3)
    expect_equal(unname(coef(model3)), c(0.00767, -0.11275, 0.0216, 0.81173), tolerance = 1e-3)
    expect_equal(unname(coef(model4)), c(-0.04349, -0.0914, 0.15028, 0.77812), tolerance = 1e-3)
    expect_equal(unname(coef(model5)), c(0.00506, -0.12284, 0.04546, 0.81432), tolerance = 1e-3)
  })

}
