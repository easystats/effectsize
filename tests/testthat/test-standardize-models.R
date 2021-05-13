if (require("testthat") && require("effectsize")) {
  # standardize.lm ----------------------------------------------------------
  test_that("standardize.lm", {
    iris2 <- na.omit(iris)
    iris_z <- standardize(iris2)

    m0 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris_z)
    m1 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris2)
    model <- standardize(m1)
    expect_equal(coef(m0), coef(model))
  })


  # Transformations ---------------------------------------------------------
  test_that("transformations", {
    # deal with log / sqrt terms
    expect_message(standardize(lm(mpg ~ sqrt(cyl) + log(hp), mtcars)))
    expect_message(standardize(lm(mpg ~ sqrt(cyl), mtcars)))
    expect_message(standardize(lm(mpg ~ log(hp), mtcars)))

    # difference between stand-methods:
    mt <- mtcars
    mt$hp_100 <- mt$hp / 100
    fit_exp <- lm(mpg ~ exp(hp_100), mt)
    fit_scale1 <- lm(scale(mpg) ~ exp(scale(hp_100)), mt)
    fit_scale2 <- lm(scale(mpg) ~ scale(exp(hp_100)), mt)
    expect_equal(
      standardize_parameters(fit_exp, method = "refit")[2, 2],
      unname(coef(fit_scale1)[2])
    )

    expect_equal(
      standardize_parameters(fit_exp, method = "basic")[2, 2],
      unname(coef(fit_scale2)[2])
    )

    skip_if_not_installed("insight", minimum_version = "0.10.0")
    d <- data.frame(
      time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
      group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
      sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50)
    )
    m <- lm(log(sum + 1) ~ as.numeric(time) * group, data = d)


    expect_message(out <- standardize(m))
    expect_equal(coef(m), c(
      `(Intercept)` = -0.4575, `as.numeric(time)` = 0.5492, group = 0.3379,
      `as.numeric(time):group` = 0.15779
    ), tolerance = 0.01)
  })





  # W/ weights --------------------------------------------------------------
  test_that("weights", {
    expect_warning(standardize(mtcars, weights = "xx"))

    m <- lm(mpg ~ am + hp, weights = cyl, mtcars)

    sm <- standardize(m, weights = TRUE)
    sm_data <- insight::get_data(sm)
    sm_data2 <- standardize(mtcars, select = c("mpg", "am", "hp"), weights = "cyl")
    expect_equal(sm_data[, c("mpg", "am", "hp")], sm_data2[, c("mpg", "am", "hp")])


    # no weights in stding
    sm_xw <- standardize(m, weights = FALSE)
    sm_data_xw <- insight::get_data(sm_xw)
    expect_false(isTRUE(all.equal(coef(sm)[-1], coef(sm_xw)[-1])))

    # refit and posthoc should give same results
    stdREFIT <- standardize_parameters(m, method = "refit")
    expect_equal(
      stdREFIT[[2]],
      standardize_parameters(m, method = "posthoc")[[2]]
    )

    expect_equal(
      stdREFIT[[2]],
      standardize_parameters(m, method = "basic")[[2]]
    )
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
    iris2 <- standardize(iris,
      select = c("Sepal.Length", "Petal.Width"),
      remove_na = "all"
    )
    iris3 <- standardize(iris,
      select = c("Sepal.Length", "Petal.Width"),
      weights = "weight_me",
      remove_na = "selected"
    )



    m1 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me)


    # weights, missing data, but data isn't weight-stdized
    m2 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris2, weights = weight_me)
    sm2 <- standardize(m1, weights = FALSE)
    expect_equal(coef(m2), coef(sm2))

    # weights, missing data, and data is weight-stdized
    m3 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris3, weights = weight_me)
    sm3 <- standardize(m1, weights = TRUE)
    expect_equal(coef(m3), coef(sm3))
  })


  # weights + missing data ´+ na.action = na.exclude --------------------------------------------------
  test_that("weights + NA + na.exclude", {
    set.seed(1234)
    data(iris)

    # data setup
    iris$weight_me <- runif(nrow(iris))
    iris$Sepal.Length[sample(nrow(iris), size = 25)] <- NA
    iris$weight_me[sample(nrow(iris), size = 15)] <- NA

    m1 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me, na.action = na.exclude)
    m2 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me)

    expect_equal(coef(standardize(m2)), coef(standardize(m1)), tolerance = 1e-3)
    expect_equal(standardize_parameters(m1, method = "basic")[[2]],
      standardize_parameters(m2, method = "basic")[[2]],
      tolerance = 1e-3
    )
  })


  # don't standardize non-Gaussian response ------------------------------------
  test_that("standardize non-Gaussian response", {
    skip_if_not_installed("lme4")
    set.seed(1234)
    data(sleepstudy, package = "lme4")

    m1 <- glm(Reaction ~ Days, family = Gamma(), data = sleepstudy)
    m2 <- glm(Reaction ~ Days, family = Gamma(link = "identity"), data = sleepstudy)
    m3 <- glm(Reaction ~ Days, family = inverse.gaussian(), data = sleepstudy)

    expect_equal(coef(standardize(m1)), c(`(Intercept)` = 0.00338, Days = -0.00034), tolerance = 1e-2)
    expect_equal(coef(standardize(m2)), c(`(Intercept)` = 298.48571, Days = 29.70754), tolerance = 1e-3)
    expect_equal(coef(standardize(m3)), c(`(Intercept)` = 1e-05, Days = 0), tolerance = 1e-3)
  })


  # variables evaluated in the environment $$$ ------------------------------
  test_that("variables evaluated in the environment", {
    m <- lm(mtcars$mpg ~ mtcars$cyl + am, data = mtcars)
    w <- testthat::capture_warnings(standardize(m))
    expect_match(w[1], "mtcars$mpg", fixed = TRUE)

    skip_if(packageVersion("base") == package_version(3.4))
    ## Note:
    # No idea why this is suddenly not giving a warning on older R versions.
    m <- lm(mtcars$mpg ~ mtcars$cyl + mtcars$am, data = mtcars)
    warns <- capture_warnings(standardize(m))
    expect_true(grepl("mtcars$mpg", warns[1], fixed = TRUE))
    expect_true(grepl("No variables", warns[2], fixed = TRUE))
  })


  # mediation models --------------------------------------------------------
  test_that("standardize non-Gaussian response", {
    skip_if_not_installed("mediation")
    set.seed(444)
    data(jobs, package = "mediation")
    jobs$econ_hard <- jobs$econ_hard * 20
    b.int <- lm(job_seek ~ treat * age + econ_hard + sex, data = jobs)
    d.int <- lm(depress2 ~ treat * job_seek * age + econ_hard + sex, data = jobs)

    med1 <- mediation::mediate(b.int, d.int, sims = 200, treat = "treat", mediator = "job_seek")
    med2 <- mediation::mediate(b.int, d.int,
      sims = 200, treat = "treat", mediator = "job_seek",
      covariates = list(age = mean(jobs$age))
    )

    out1 <- summary(standardize(med1))
    expect_message(out2 <- summary(standardize(med2)))
    expect_equal(unlist(out1[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
      unlist(out2[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
      tolerance = 0.1
    )

    med0 <- mediation::mediate(standardize(b.int), standardize(d.int), sims = 200, treat = "treat", mediator = "job_seek")
    out0 <- summary(med0)
    medz <- standardize(mediation::mediate(b.int, d.int, sims = 200, treat = "treat", mediator = "job_seek"))
    outz <- summary(medz)
    expect_equal(unlist(out0[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
      unlist(outz[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
      tolerance = 0.1
    )
  })
}
