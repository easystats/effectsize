if (require("testthat") && require("effectsize")) {
  # standardize.lm ----------------------------------------------------------
  test_that("standardize.lm", {
    iris2 <- na.omit(iris)
    iris_z <- standardize(iris2)

    m0 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris_z)
    m1 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris2)
    model <- standardize(m1)
    testthat::expect_equal(coef(m0), coef(model))

    # deal with log / sqrt terms
    testthat::expect_message(standardize(lm(mpg ~ sqrt(cyl) + log(hp), mtcars)))
    testthat::expect_message(standardize(lm(mpg ~ sqrt(cyl), mtcars)))
    testthat::expect_message(standardize(lm(mpg ~ log(hp), mtcars)))

    # difference between stand-methods:
    mt <- mtcars
    mt$hp_100 <- mt$hp/100
    fit_exp <- lm(mpg ~ exp(hp_100), mt)
    fit_scale1 <- lm(scale(mpg) ~ exp(scale(hp_100)), mt)
    fit_scale2 <- lm(scale(mpg) ~ scale(exp(hp_100)), mt)
    testthat::expect_equal(standardize_parameters(fit_exp, method = "refit")[2,2],
                           unname(coef(fit_scale1)[2]))

    testthat::expect_equal(standardize_parameters(fit_exp, method = "basic")[2,2],
                           unname(coef(fit_scale2)[2]))
  })


  # W/ weights --------------------------------------------------------------
  test_that("weights", {
    expect_warning(standardize(mtcars, weights = "xx"))

    m <- lm(mpg ~ am + hp, weights = cyl, mtcars)

    sm <- standardize(m, weights = TRUE)
    sm_data <- insight::get_data(sm)
    sm_data2 <- standardize(mtcars, select = c("mpg", "am", "hp"), weights = "cyl")
    expect_equal(sm_data[,c("mpg", "am", "hp")], sm_data2[,c("mpg", "am", "hp")])


    # no weights in stding
    sm_xw <- standardize(m, weights = FALSE)
    sm_data_xw <- insight::get_data(sm_xw)
    expect_false(isTRUE(all.equal(coef(sm)[-1], coef(sm_xw)[-1])))

    # refit and posthoc should give same results
    stdREFIT <- standardize_parameters(m, method = "refit")
    expect_equal(stdREFIT[[2]],
                 standardize_parameters(m, method = "posthoc")[[2]])

    expect_equal(stdREFIT[[2]],
                 standardize_parameters(m, method = "basic")[[2]])
  })


  # weights + missing data --------------------------------------------------
  test_that("weights + NA", {
    set.seed(1234)
    data(iris)

    # data setup
    iris$weight_me <- runif(nrow(iris))
    iris$Sepal.Length[sample(nrow(iris), size = 10)] <- NA
    iris$weight_me[sample(nrow(iris), size = 10)] <- NA

    # standardize 2nd data set
    iris2 <- standardize(iris, select = c("Sepal.Length", "Petal.Width"),
                         na_action = "all")
    iris3 <- standardize(iris, select = c("Sepal.Length", "Petal.Width"),
                         weights = "weight_me",
                         na_action = "select")



    m1 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me)


    # weights, missing data, but data isn't weight-stdized
    m2 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris2, weights = weight_me)
    sm2 <- standardize(m1, weights = FALSE)
    testthat::expect_equal(coef(m2), coef(sm2))

    # weights, missing data, and data is weight-stdized
    m3 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris3, weights = weight_me)
    sm3 <- standardize(m1, weights = TRUE)
    testthat::expect_equal(coef(m3), coef(sm3))
  })
}
