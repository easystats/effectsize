if (require("testthat") &&
    require("effectsize") &&
    require("rstanarm", quietly = TRUE) &&
    require("bayestestR") &&
    require("car")) {

  fit_bayes <- stan_glm(mpg ~ factor(cyl) * wt + qsec,
                        data = mtcars,
                        family = gaussian(),
                        refresh = 0)

  test_that("eta_squared_posterior (PARTIAL, type = 3)", {
    testthat::skip_on_cran()

    es_tab <- eta_squared(
      Anova(
        lm(mpg ~ factor(cyl) * wt + qsec,
           data = mtcars),
        type = 3
      ),
      partial = TRUE
    )

    testthat::expect_warning(es_post <- eta_squared_posterior(fit_bayes, verbose = FALSE))
    testthat::expect_equal(colnames(es_post), es_tab$Parameter)

    # this is a very soft test...
    es_tab_bayes <- describe_posterior(es_post)
    testthat::expect_equal(order(es_tab_bayes$Median), order(es_tab$Eta2))
  })


  test_that("eta_squared_posterior (non-PARTIAL, type = 2)", {
    testthat::skip_on_cran()

    es_tab <- eta_squared(
      Anova(
        lm(mpg ~ factor(cyl) * wt + qsec,
           data = mtcars),
        type = 2
      ),
      partial = FALSE
    )

    testthat::expect_warning(es_post <- eta_squared_posterior(fit_bayes, verbose = FALSE, partial = FALSE, type = 2))
    testthat::expect_equal(colnames(es_post), es_tab$Parameter)

    # this is a very soft test...
    es_tab_bayes <- describe_posterior(es_post)
    # testthat::expect_equal(order(es_tab_bayes$Median), order(es_tab$Eta2))
  })
}



