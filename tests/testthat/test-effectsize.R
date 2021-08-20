if (require("testthat") && require("effectsize")) {

  # htest -------------------------------------------------------------------
  test_that("t-test", {
    x <<- 1:10
    y <<- c(1, 1:9)
    model <- t.test(x, y)
    expect_equal(effectsize(model), cohens_d(x, y, pooled_sd = FALSE), ignore_attr = TRUE)
    expect_equal(effectsize(model, type = "g"), hedges_g(x, y, pooled_sd = FALSE), ignore_attr = TRUE)

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
    expect_warning(effectsize(model))

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
  })

  test_that("Chisq-test", {
    contingency_table <-
      as.table(rbind(c(760, 330, 470), c(480, 240, 480), c(480, 240, 480)))

    Xsq1 <- chisq.test(contingency_table)
    Xsq2 <- chisq.test(contingency_table / 10)

    expect_equal(effectsize(Xsq1)$Cramers_v, 0.073, tolerance = 0.01)
    expect_equal(
      effectsize(Xsq1)$Cramers_v,
      effectsize(Xsq2)$Cramers_v
    )

    Xsq3 <- chisq.test(table(mtcars$cyl))
    expect_equal(effectsize(Xsq3)$Cramers_v, 0.19, tolerance = 0.01)
    expect_equal(
      effectsize(Xsq3)$Cramers_v,
      cramers_v(table(mtcars$cyl))$Cramers_v
    )

    # types
    expect_equal(
      effectsize(Xsq1, type = "phi"),
      phi(contingency_table)
    )
    expect_equal(
      effectsize(Xsq1, type = "w"),
      cohens_w(contingency_table)
    )

    expect_error(effectsize(Xsq1, type = "riskratio"))
    contingency_table22 <- contingency_table[1:2, 1:2]
    Xsq4 <- chisq.test(contingency_table22)
    expect_equal(
      effectsize(Xsq4, type = "oddsratio"),
      oddsratio(contingency_table22)
    )
    expect_equal(
      effectsize(Xsq4, type = "riskratio"),
      riskratio(contingency_table22)
    )

    # goodness of fit
    observed.dfc <<- c(119, 61)
    expected.dfc <<- c(0.165, 0.835)

    x <- chisq.test(x = observed.dfc, p = expected.dfc)
    ref <- cramers_v(x = observed.dfc, p = expected.dfc)

    expect_equal(effectsize(x, type = "v"), ref)
    expect_equal(cramers_v(x), ref)
  })

  test_that("cor.test", {
    r_ <- cor.test(iris$Sepal.Width, iris$Sepal.Length)
    s_ <- suppressWarnings(cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "spearman"))
    t_ <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "kendall")

    expect_equal(effectsize(r_)[[1]], -0.118, tolerance = 0.01)
    expect_equal(effectsize(s_)[[1]], -0.167, tolerance = 0.01)
    expect_equal(effectsize(t_)[[1]], -0.077, tolerance = 0.01)

    # no CI for tau or sr
    expect_equal(ncol(effectsize(r_)), 4L)
    expect_equal(ncol(effectsize(s_)), 1L)
    expect_equal(ncol(effectsize(t_)), 1L)
  })

  test_that("one way", {
    onew <- oneway.test(mpg ~ cyl, mtcars)
    expect_warning(effectsize(onew))


    onew <- oneway.test(mpg ~ cyl, mtcars, var.equal = TRUE)
    m <- aov(mpg ~ cyl, mtcars)

    expect_equal(eta_squared(m, partial = FALSE)[, -1], effectsize(onew),
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
    expect_equal(effectsize(model), cohens_g(Performance), ignore_attr = TRUE)

    model <- mcnemar.test(mtcars$cyl, mtcars$gear)
    expect_equal(effectsize(model), cohens_g(mtcars$cyl, mtcars$gear), ignore_attr = TRUE)
  })

  test_that("htest | wrappers", {
    x <<- 1:10
    y <<- c(1, 1:9)
    Ts <- t.test(x, y)
    expect_equal(cohens_d(Ts), cohens_d(x, y, pooled_sd = FALSE),
      ignore_attr = TRUE, tolerance = 0.01
    )
    expect_equal(hedges_g(Ts), hedges_g(x, y, pooled_sd = FALSE),
      ignore_attr = TRUE, tolerance = 0.01
    )

    M <<- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
    Xsq <- chisq.test(M)
    expect_equal(phi(Xsq), phi(M), tolerance = 0.01)
    expect_equal(cramers_v(Xsq), cramers_v(M), tolerance = 0.01)

    M <<- as.table(rbind(c(762, 327), c(484, 239)))
    Xsq <- chisq.test(M)
    expect_equal(oddsratio(Xsq), oddsratio(M), tolerance = 0.01)
    expect_equal(riskratio(Xsq), riskratio(M), tolerance = 0.01)


    OWA <- oneway.test(mpg ~ factor(cyl), data = mtcars, var.equal = TRUE)
    m <- lm(mpg ~ factor(cyl), data = mtcars)
    expect_equal(eta_squared(OWA), eta_squared(m, verbose = FALSE)[, -1],
      ignore_attr = TRUE, tolerance = 0.01
    )
    expect_equal(omega_squared(OWA), omega_squared(m, verbose = FALSE)[, -1],
      ignore_attr = TRUE, tolerance = 0.01
    )
    expect_equal(epsilon_squared(OWA), epsilon_squared(m, verbose = FALSE)[, -1],
      ignore_attr = TRUE, tolerance = 0.01
    )
    expect_equal(cohens_f(OWA), cohens_f(m, verbose = FALSE)[, -1],
      ignore_attr = TRUE, tolerance = 0.01
    )
    expect_equal(cohens_f_squared(OWA), cohens_f_squared(m, verbose = FALSE)[, -1],
      ignore_attr = TRUE, tolerance = 0.01
    )

    Performance <<- rbind(
      c(794, 86),
      c(150, 570)
    )
    Mc <- mcnemar.test(Performance)
    expect_equal(cohens_g(Mc), cohens_g(Performance), tolerance = 0.01)

    H <- effectsize(chisq.test(Performance), type = "h")
    expect_equal(H, cohens_h(Performance), tolerance = 0.01)
    expect_equal(H[[1]], 1.580585, tolerance = 0.01)
    expect_equal(H$CI_low, 1.480959, tolerance = 0.01)
    expect_equal(H$CI_high, 1.68021, tolerance = 0.01)
  })

  test_that("htest | Get args from htest", {
    tt <- t.test(mtcars$hp, mtcars$mpg, alternative = "l", mu=-3, conf.level = 0.8, var.equal = TRUE)
    expect_equal(cohens_d(tt), cohens_d(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, ci = 0.8), ignore_attr = TRUE)
    expect_equal(cohens_d(tt, mu = -4, ci = 0.99, alternative = "t"),
                 cohens_d(mtcars$hp, mtcars$mpg, mu = -4, ci = 0.99, alternative = "t"),
                 ignore_attr = TRUE)

    suppressWarnings(ww1 <- wilcox.test(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3))
    expect_equal(rank_biserial(ww1), rank_biserial(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3), ignore_attr = TRUE)
    expect_equal(rank_biserial(ww1, mu = -4, alternative = "t"),
                 rank_biserial(mtcars$hp, mtcars$mpg, mu = -4, alternative = "t"),
                 ignore_attr = TRUE)

    suppressWarnings(ww2 <- wilcox.test(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3,  conf.int = TRUE, conf.level = 0.8))
    expect_equal(rank_biserial(ww2), rank_biserial(mtcars$hp, mtcars$mpg, alternative = "l", mu = -3, ci = 0.8), ignore_attr = TRUE)
    expect_equal(rank_biserial(ww2, mu = -4, alternative = "t", ci = 0.99),
                 rank_biserial(mtcars$hp, mtcars$mpg, mu = -4, alternative = "t", ci = 0.99),
                 ignore_attr = TRUE)
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
    expect_equal(effectsize(bf1, test = NULL)[[2]], 0.164, tolerance = 0.01)
    expect_equal(effectsize(bf1, test = NULL, type = "OR")[[2]], 1 / 0.503, tolerance = 0.03)

    bf2 <- BayesFactor::ttestBF(mtcars$mpg[mtcars$am == 1], mtcars$mpg[mtcars$am == 0])
    expect_equal(effectsize(bf2, test = NULL)[[2]], 1.30, tolerance = 0.03)

    bf3 <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width)
    expect_equal(effectsize(bf3, test = NULL)[[2]], -0.116, tolerance = 0.03)

    bf4 <- BayesFactor::proportionBF(4, 12, 0.5)
    expect_equal(effectsize(bf4, test = NULL)[[2]], 0.3911, tolerance = 0.03)
  })
}
