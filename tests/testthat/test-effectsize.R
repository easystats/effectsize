# htest -------------------------------------------------------------------
test_that("t-test", {
  x <<- 1:10
  y <<- c(1, 1:9)
  model <- t.test(x, y)
  expect_equal(effectsize(model), d <- cohens_d(x, y, pooled_sd = FALSE), ignore_attr = TRUE)
  expect_equal(cohens_d(model), d, ignore_attr = TRUE)
  expect_equal(effectsize(model, type = "g"), hedges_g(x, y, pooled_sd = FALSE), ignore_attr = TRUE)
  expect_error(effectsize(model, type = "u1"), "applicable")

  model <- t.test(x, y, var.equal = TRUE)
  expect_equal(effectsize(model, type = "u1"), cohens_u1(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(model, type = "u2"), cohens_u2(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(model, type = "u3"), cohens_u3(x, y), ignore_attr = TRUE)

  model <- t.test(x, y, alternative = "less", conf.level = 0.8)
  expect_equal(effectsize(model), cohens_d(x, y, pooled_sd = FALSE, alternative = "less", ci = 0.8), ignore_attr = TRUE)

  model <- t.test(x, y, paired = TRUE)
  expect_equal(effectsize(model), cohens_d(x, y, paired = TRUE), ignore_attr = TRUE)

  model <- t.test(x, y, var.equal = TRUE)
  expect_equal(effectsize(model), cohens_d(x, y), ignore_attr = TRUE)

  model <- t.test(x, y, var.equal = TRUE, mu = 3)
  expect_equal(effectsize(model), cohens_d(x, y, mu = 3), ignore_attr = TRUE)

  df <- data.frame(DV = c(x, y), g = rep(1:2, each = 10))
  model <- t.test(DV ~ g, data = df, var.equal = TRUE, mu = 3)
  expect_warning(effectsize(model), "data")

  ## Auto convert y to factor
  Ts <- t.test(mtcars$mpg ~ mtcars$vs)
  expect_equal(effectsize(Ts, verbose = FALSE),
    cohens_d(mtcars$mpg, factor(mtcars$vs), pooled_sd = FALSE),
    ignore_attr = TRUE
  )


  # one sample
  z <<- mtcars$wt
  model <- t.test(z, mu = 3, var.equal = TRUE)
  expect_equal(effectsize(model),
    cohens_d(z, mu = 3),
    ignore_attr = TRUE
  )

  ## Missing
  y <<- rnorm(12)
  g <<- c(rep(letters[1:2], each = 5), NA, NA)
  tt <- t.test(y ~ g, var.equal = TRUE)

  expect_equal(effectsize(tt), cohens_d(y ~ g), ignore_attr = TRUE)
})

test_that("t-test | CLES", {
  x <<- 1:4
  y <<- c(1, 1:3)
  Tt <- t.test(x, y, var.equal = TRUE)

  expect_equal(e <- p_superiority(Tt), p_superiority(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(Tt, type = "p_superiority"), e)

  expect_equal(e <- cohens_u1(Tt), cohens_u1(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(Tt, type = "u1"), e)

  expect_equal(e <- cohens_u2(Tt), cohens_u2(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(Tt, type = "u2"), e)

  expect_equal(e <- cohens_u3(Tt), cohens_u3(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(Tt, type = "u3"), e)

  expect_equal(e <- p_overlap(Tt), p_overlap(x, y), ignore_attr = TRUE)
  expect_equal(effectsize(Tt, type = "overlap"), e)
})

test_that("Wilcox | CLES", {
  x <<- 1:4
  y <<- c(1, 1:3)
  Wt <- suppressWarnings(wilcox.test(x, y))

  expect_equal(e <- p_superiority(Wt), p_superiority(x, y, parametric = FALSE), ignore_attr = TRUE)
  expect_equal(effectsize(Wt, type = "p_superiority"), e)

  expect_error(effectsize(Wt, type = "u1"), "parametric")

  expect_equal(e <- cohens_u3(Wt), cohens_u3(x, y, parametric = FALSE), ignore_attr = TRUE)
  expect_equal(effectsize(Wt, type = "u3"), e)
})

test_that("Chisq-test", {
  contingency_table <-
    as.table(rbind(c(760, 330, 470), c(480, 240, 480), c(480, 240, 480)))

  Xsq1 <- chisq.test(contingency_table)
  Xsq2 <- chisq.test(contingency_table / 10)

  expect_equal(effectsize(Xsq1, adjust = FALSE)[[1]], 0.073, tolerance = 0.01)
  expect_equal(
    effectsize(Xsq1, adjust = FALSE)[[1]],
    effectsize(Xsq2, adjust = FALSE)[[1]]
  )

  # types
  expect_error(effectsize(Xsq1, type = "phi"), "appropriate")
  expect_equal(effectsize(Xsq1), cramers_v(contingency_table))
  expect_equal(effectsize(Xsq1, type = "w"), w <- cohens_w(contingency_table))
  expect_equal(cohens_w(Xsq1), w)

  expect_error(effectsize(Xsq1, type = "riskratio"), "only")
  expect_error(riskratio(Xsq1), "only")

  contingency_table22 <- contingency_table[1:2, 1:2]
  Xsq4 <- chisq.test(contingency_table22)
  expect_equal(effectsize(Xsq4, type = "phi", adjust = FALSE), ph <- phi(contingency_table22, adjust = FALSE))
  expect_equal(phi(Xsq4, adjust = FALSE), ph)

  expect_equal(effectsize(Xsq4, type = "oddsratio"), or <- oddsratio(contingency_table22))
  expect_equal(oddsratio(Xsq4), or)

  expect_equal(effectsize(Xsq4, type = "riskratio"), rr <- riskratio(contingency_table22))
  expect_equal(riskratio(Xsq4), rr)

  expect_equal(effectsize(Xsq4, type = "pearsons_c"), pc <- pearsons_c(contingency_table22))
  expect_equal(pearsons_c(Xsq4), pc)

  expect_equal(effectsize(Xsq4, type = "h"), h <- cohens_h(contingency_table22))
  expect_equal(cohens_h(Xsq4), h)

  # goodness of fit
  observed.dfc <<- c(119, 61)
  expected.dfc <<- c(0.165, 0.835)

  x <- chisq.test(x = observed.dfc, p = expected.dfc)
  expect_error(effectsize(x, type = "v"), "goodness")
  expect_equal(effectsize(x), effectsize(x, type = "fei"))
  expect_equal(effectsize(x, type = "fei"), Fei <- fei(observed.dfc, p = expected.dfc))
  expect_equal(fei(x), Fei)
})

test_that("cor.test / other", {
  data("RCT_table")
  fish <- fisher.test(RCT_table)
  Xsq <- chisq.test(RCT_table)

  expect_equal(
    effectsize(fish),
    effectsize(Xsq, alternative = "two")
  )
})


test_that("cor.test / other", {
  r_ <- cor.test(iris$Sepal.Width, iris$Sepal.Length)
  expect_warning(effectsize(r_), "parameters")
})

test_that("one way", {
  onew <- oneway.test(mpg ~ cyl, mtcars)
  expect_message(effectsize(onew), "var")


  onew <- oneway.test(mpg ~ cyl, mtcars, var.equal = TRUE)
  m <- aov(mpg ~ cyl, mtcars)

  expect_equal(eta_squared(m, partial = FALSE)[, -1], effectsize(onew),
    tolerance = 0.03, ignore_attr = TRUE
  )
  expect_equal(eta_squared(m, partial = FALSE)[, -1], eta_squared(onew, verbose = FALSE),
    tolerance = 0.03, ignore_attr = TRUE
  )
  expect_equal(omega_squared(m, partial = FALSE)[, -1], effectsize(onew, type = "omega"),
    tolerance = 0.03, ignore_attr = TRUE
  )
  expect_equal(cohens_f(m, partial = FALSE)[, -1], effectsize(onew, type = "f"),
    tolerance = 0.03, ignore_attr = TRUE
  )
})

test_that("McNemar", {
  Performance <<- rbind(
    c(794, 86),
    c(150, 570)
  )

  model <- mcnemar.test(Performance)
  expect_equal(effectsize(model), g <- cohens_g(Performance), ignore_attr = TRUE)
  expect_equal(cohens_g(model), g, ignore_attr = TRUE)

  model <- mcnemar.test(mtcars$cyl, mtcars$gear)
  expect_equal(effectsize(model), cohens_g(mtcars$cyl, mtcars$gear), ignore_attr = TRUE)
})

test_that("htest | rank", {
  suppressWarnings(ww <- wilcox.test(mtcars$hp, mtcars$mpg + 80))
  expect_equal(effectsize(ww), rbs <- rank_biserial(mtcars$hp, mtcars$mpg + 80), ignore_attr = TRUE)
  expect_equal(rank_biserial(ww), rbs, ignore_attr = TRUE)
  expect_equal(effectsize(ww, type = "u2", ci = NULL)[[1]],
    cohens_u2(mtcars$hp, mtcars$mpg + 80, parametric = FALSE, ci = NULL)[[1]],
    tolerance = 0.001
  )
  expect_equal(effectsize(ww, type = "overlap")[[1]],
    p_overlap(mtcars$hp, mtcars$mpg + 80, parametric = FALSE, ci = NULL)[[1]],
    tolerance = 0.001
  )


  RoundingTimes <-
    matrix(
      c(
        5.40, 5.50, 5.55,
        5.85, 5.70, 5.75,
        5.20, 5.60, 5.50,
        5.55, 5.50, 5.40,
        5.90, 5.85, 5.70,
        5.45, 5.55, 5.60,
        5.40, 5.40, 5.35,
        5.45, 5.50, 5.35,
        5.25, 5.15, 5.00,
        5.85, 5.80, 5.70,
        5.25, 5.20, 5.10,
        5.65, 5.55, 5.45,
        5.60, 5.35, 5.45,
        5.05, 5.00, 4.95,
        5.50, 5.50, 5.40,
        5.45, 5.55, 5.50,
        5.55, 5.55, 5.35,
        5.45, 5.50, 5.55,
        5.50, 5.45, 5.25,
        5.65, 5.60, 5.40,
        5.70, 5.65, 5.55,
        6.30, 6.30, 6.25
      ),
      nrow = 22,
      byrow = TRUE,
      dimnames = list(
        1:22,
        c("Round Out", "Narrow Angle", "Wide Angle")
      )
    )
  ft <- friedman.test(RoundingTimes)
  W <- kendalls_w(RoundingTimes, verbose = FALSE, ci = NULL)
  expect_equal(effectsize(ft, verbose = FALSE, ci = NULL), W, ignore_attr = TRUE)
  expect_equal(kendalls_w(ft, verbose = FALSE, ci = NULL), W, ignore_attr = TRUE)

  X <<- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
  Y <<- c(3.8, 2.7, 4.0, 2.4) # with obstructive airway disease
  Z <<- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
  kt <- kruskal.test(list(X, Y, Z))
  expect_equal(effectsize(kt)[[1]], E <- rank_epsilon_squared(list(X, Y, Z))[[1]], ignore_attr = TRUE)
  expect_equal(rank_epsilon_squared(kt)[[1]], E, ignore_attr = TRUE)
})

test_that("htest | Get args from htest", {
  tt <- t.test(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, conf.level = 0.8, var.equal = TRUE)
  expect_equal(cohens_d(tt), cohens_d(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, ci = 0.8), ignore_attr = TRUE)

  suppressWarnings(ww1 <- wilcox.test(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3))
  expect_equal(rank_biserial(ww1), rank_biserial(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3), ignore_attr = TRUE)

  suppressWarnings(ww2 <- wilcox.test(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, conf.int = TRUE, conf.level = 0.8))
  expect_equal(rank_biserial(ww2), rank_biserial(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, ci = 0.8), ignore_attr = TRUE)
})


# aov ---------------------------------------------------------------------
test_that("aov", {
  data <- iris
  data$Cat1 <- rep(c("A", "B"), length.out = nrow(data))
  model <- aov(Sepal.Length ~ Species * Cat1, data = data)
  expect_equal(effectsize(model), eta_squared(model))
  expect_equal(effectsize(model, type = "omega"), omega_squared(model))
})


# BayesFactor -------------------------------------------------------------
test_that("BayesFactor", {
  skip_if_not_installed("BayesFactor")
  skip_on_cran()
  set.seed(6)
  data(raceDolls, package = "BayesFactor")
  bf1 <- BayesFactor::contingencyTableBF(raceDolls, sampleType = "poisson", fixedMargin = "cols")
  expect_equal(effectsize(bf1)[[1]], 0.143, tolerance = 0.01)
  expect_equal(effectsize(bf1, type = "OR")[[1]], 1 / 0.503, tolerance = 0.03)

  bf2 <- BayesFactor::ttestBF(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0])
  expect_equal(effectsize(bf2)[[1]], 1.30, tolerance = 0.03)
  expect_equal(effectsize(bf2, type = "u1")[[1]], 0.65, tolerance = 0.05)
  expect_equal(effectsize(bf2, type = "u2")[[1]], 0.74, tolerance = 0.05)
  expect_equal(effectsize(bf2, type = "u3")[[1]], 0.9, tolerance = 0.05)
  expect_equal(effectsize(bf2, type = "overlap")[[1]], 0.52, tolerance = 0.05)
  expect_equal(effectsize(bf2, type = "p_superiority")[[1]], 0.8, tolerance = 0.05)

  bf3 <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width)
  expect_equal(effectsize(bf3)[[1]], -0.116, tolerance = 0.03)

  bf4 <- BayesFactor::proportionBF(4, 12, 0.5)
  expect_equal(effectsize(bf4)[[1]], 0.3911, tolerance = 0.03)
})

test_that("effectsize | easycorrelation", {
  skip_if_not_installed("correlation")
  r <- correlation::correlation(mtcars)
  expect_error(effectsize(r), regexp = NA)
})

test_that("effectsize | other", {
  m <- lm(mpg ~ ., mtcars)

  expect_equal(effectsize(m),
    parameters::standardize_parameters(m),
    ignore_attr = TRUE
  )
})
