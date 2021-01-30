if (require("testthat") && require("effectsize")) {
  test_that("eta_squared_posterior", {
    skip_on_cran()
    skip_if_not_installed("rstanarm")
    skip_if_not_installed("bayestestR")
    skip_if_not_installed("car")

    fit_bayes <- rstanarm::stan_glm(mpg ~ factor(cyl) * wt + qsec,
      data = mtcars,
      family = gaussian(),
      refresh = 0
    )


    # PARTIAL, type = 3 -------------------------------------------------------
    es_tab <- eta_squared(
      car::Anova(
        lm(mpg ~ factor(cyl) * wt + qsec,
          data = mtcars
        ),
        type = 3
      ),
      partial = TRUE
    )

    expect_warning(
      es_post <- eta_squared_posterior(fit_bayes,
        ss_function = car::Anova, type = 3
      ), regexp = "bogus"
    )
    expect_equal(colnames(es_post), es_tab$Parameter)

    # this is a very soft test...
    es_tab_bayes <- bayestestR::describe_posterior(es_post)
    expect_equal(order(es_tab_bayes$Median), order(es_tab$Eta2))



    # non-PARTIAL, type = 3 ---------------------------------------------------
    es_tab <- eta_squared(
      car::Anova(
        lm(mpg ~ factor(cyl) * wt + qsec,
          data = mtcars
        ),
        type = 3
      ),
      partial = FALSE
    )

    expect_warning(
      es_post <- eta_squared_posterior(fit_bayes,
        partial = FALSE,
        ss_function = car::Anova, type = 3
      ), regexp = "bogus"
    )
    expect_equal(colnames(es_post), es_tab$Parameter)

    # this is a very soft test...
    es_tab_bayes <- bayestestR::describe_posterior(es_post)
    expect_equal(order(es_tab_bayes$Median), order(es_tab$Eta2))
  })
}
