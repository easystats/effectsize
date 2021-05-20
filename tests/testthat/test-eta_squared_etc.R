if (require("testthat") && require("effectsize")) {

  # anova() -----------------------------------------------------------------
  test_that("anova()", {
    m <- matrix(c(3, 1, 1),
      nrow = 1,
      dimnames = list(
        "Term",
        c("F value", "NumDF", "DenDF")
      )
    )
    class(m) <- "anova"

    expect_error(eta_squared(m), regexp = NA)
    expect_equal(
      eta_squared(m)[, -1],
      F_to_eta2(3, 1, 1),
      ignore_attr = TRUE
    )
  })

  # aov ---------------------------------------------------------------------
  test_that("aov", {
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    fit <- aov(Sepal.Length ~ Species * Sepal.Big, df)

    # eta
    expect_equal(eta_squared(fit, partial = FALSE)$Eta2,
      c(0.618, 0.046, 0.000),
      tolerance = 0.01
    )
    expect_equal(eta_squared(fit, partial = TRUE)$Eta2_partial,
      c(0.649, 0.121, 0.001),
      tolerance = 0.01
    )

    # omega
    expect_equal(omega_squared(fit, partial = FALSE)$Omega2,
      c(0.612, 0.043, -0.004),
      tolerance = 0.01
    )
    expect_equal(omega_squared(fit, partial = TRUE)$Omega2_partial,
      c(0.638, 0.112, -0.012),
      tolerance = 0.01
    )

    # epsilon
    expect_equal(epsilon_squared(fit, partial = FALSE)$Epsilon2,
      c(0.614, 0.044, -0.004),
      tolerance = 0.001
    )
    expect_equal(epsilon_squared(fit, partial = TRUE)$Epsilon2_partial,
      c(0.644, 0.115, -0.012),
      tolerance = 0.01
    )

    # Cohen's f/f2
    expect_equal(cohens_f_squared(fit, partial = FALSE)$Cohens_f2,
      c(1.623, 0.049, 0.000),
      tolerance = 0.001
    )
    expect_equal(cohens_f_squared(fit, partial = TRUE)$Cohens_f2_partial,
      c(1.850, 0.139, 0.001),
      tolerance = 0.001
    )
    expect_equal(cohens_f(fit, partial = FALSE)$Cohens_f,
      c(1.273, 0.220, 0.021),
      tolerance = 0.01
    )
    expect_equal(cohens_f(fit, partial = TRUE)$Cohens_f_partial,
      c(1.360, 0.373, 0.036),
      tolerance = 0.001
    )
    expect_equal(cohens_f(fit, squared = TRUE), cohens_f_squared(fit))
    expect_equal(cohens_f_squared(fit, squared = FALSE), cohens_f(fit))



    #### One way-between
    expect_message(eta_squared(aov(mpg ~ factor(gear), mtcars)))
    expect_message(eta_squared(aov(mpg ~ factor(gear) + am, mtcars)), regexp = NA)
  })


  # aovlist -----------------------------------------------------------------
  test_that("aovlist", {
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    model <- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)

    res <- eta_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    res <- omega_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    res <- epsilon_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))



    skip_if_not_installed("afex")
    # non-partial Eta2 should be the same for aov and aovlist
    data(obk.long, package = "afex")
    model <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long, observed = "gender",
      include_aov = TRUE
    )

    model2 <- aov(value ~ treatment * gender * phase * hour,
      data = model$data$long,
      contrasts = list(
        treatment = contr.sum,
        gender = contr.sum,
        phase = contr.sum,
        hour = contr.sum
      )
    )

    a1 <- eta_squared(model2, partial = F)
    a2 <- eta_squared(model$aov, partial = F)

    rownames(a1) <- a1$Parameter
    rownames(a2) <- a2$Parameter

    expect_equal(
      a1[a1$Parameter, "Eta2"],
      a2[a1$Parameter, "Eta2"]
    )
  })

  # mlm / anova table -------------------------------------------------------
  test_that("mlm / anova table", {
    data("mtcars")
    mtcars$am_f <- factor(mtcars$am)
    mtcars$cyl_f <- factor(mtcars$cyl)

    mod <- lm(cbind(mpg, qsec) ~ am_f * cyl_f, data = mtcars)
    m1 <- lm(mpg ~ am_f * cyl_f, data = mtcars)
    m2 <- lm(qsec ~ am_f * cyl_f, data = mtcars)

    expect_equal(
      eta_squared(mod)$Eta2_partial[1:3],
      eta_squared(m1)$Eta2_partial
    )

    expect_equal(
      eta_squared(mod)$Eta2_partial[4:6],
      eta_squared(m2)$Eta2_partial
    )

    expect_equal(
      eta_squared(mod, partial = FALSE)$Eta2[1:3],
      eta_squared(m1, partial = FALSE)$Eta2
    )

    expect_equal(
      eta_squared(mod, partial = FALSE)$Eta2[4:6],
      eta_squared(m2, partial = FALSE)$Eta2
    )

    # MANOVA table
    mod <- manova(cbind(mpg, qsec) ~ am_f * cyl_f, data = mtcars)
    expect_equal(nrow(eta_squared(mod)), 3L)
  })


  # Cohen's f - R2 change ---------------------------------------------------
  test_that("Cohen's f - R2 change", {
    data(hardlyworking)
    m1 <- lm(salary ~ xtra_hours, data = hardlyworking)
    m2 <- lm(salary ~ xtra_hours + n_comps, data = hardlyworking)

    fsD <- cohens_f_squared(m1, model2 = m2)[, 1:4]
    fs <- cohens_f_squared(m2)[-1, -1] # this ONLY works because of the default type-I errors!!!!
    rownames(fsD) <- rownames(fs) <- 1
    expect_equal(fsD, fs, tolerance = 0.01, ignore_attr = TRUE)


    skip_if_not_installed("performance")
    fsD <- cohens_f_squared(m1, model2 = m2)
    R2_1 <- performance::r2(m1)[[1]]
    R2_2 <- performance::r2(m2)[[1]]
    expect_equal(
      fsD$Cohens_f2_partial,
      unname((R2_2 - R2_1) / (1 - R2_2))
    )
  })

  # generalized Eta -------------------------------------------------------------
  test_that("generalized | between", {
    skip_if_not_installed("afex")
    skip_if_not_installed("car")

    data(obk.long, package = "afex")
    m <- suppressWarnings(
      afex::aov_car(value ~ treatment * gender + Error(id),
        data = obk.long, observed = "gender",
        include_aov = TRUE
      )
    )

    Aov <- car::Anova(m$aov, type = 3)

    expect_equal(
      anova(m, es = "ges", observed = NULL)$ges,
      eta_squared(Aov, generalized = TRUE, verbose = FALSE)$Eta2_generalized
    )


    expect_equal(
      anova(m, es = "ges", observed = "gender")$ges,
      eta_squared(Aov, generalized = "gender", verbose = FALSE)$Eta2_generalized
    )

    # in a completely between design, with all measured,
    # all are equal to total
    expect_equal(
      eta_squared(Aov, generalized = c("gender", "treatment"), verbose = FALSE)[[2]],
      eta_squared(Aov, partial = FALSE, verbose = FALSE)[[2]]
    )
  })


  test_that("generalized | within-mixed", {
    skip_if_not_installed("afex")
    data(obk.long, package = "afex")

    # estimate mixed ANOVA on the full design:
    m <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long, observed = "gender",
      include_aov = TRUE
    )


    ef <- eta_squared(m$aov, generalized = "gender")
    af <- anova(m, es = "ges", observed = "gender")
    expect_equal(ef$Eta2_generalized,
      c(
        0.211, 0.083, 0.186, 0.193, 0.099,
        0.002, 0.015, 0.132, 0.001, 0.004,
        0.011, 0.016, 0.008, 0.01, 0.02
      ),
      tolerance = 0.05
    )
    expect_equal(ef$Eta2_generalized,
      af$ges,
      tolerance = 0.1
    )


    ef <- eta_squared(m$aov, generalized = TRUE)
    af <- anova(m, es = "ges", observed = NULL)
    expect_equal(ef$Eta2_generalized,
      c(
        0.286, 0.111, 0.218, 0.264, 0.142,
        0.004, 0.021, 0.185, 0.002, 0.005,
        0.016, 0.023, 0.013, 0.014, 0.029
      ),
      tolerance = 0.05
    )
    expect_equal(ef$Eta2_generalized,
      af$ges,
      tolerance = 0.1
    )
  })



  # rm-omega ----------------------------------------------------------------
  test_that("omega", {
    skip_if_not_installed("afex")
    # cross validated with MOTE
    data(obk.long, package = "afex")

    m <- suppressWarnings(
      afex::aov_car(value ~ treatment * gender + Error(id / (phase)),
        data = obk.long, observed = "gender",
        include_aov = TRUE
      )
    )


    ef <- omega_squared(m, partial = TRUE)
    expect_equal(ef$Omega2_partial,
                 c(0.3115, 0.1814, 0.2221, 0.2637, 0.1512, -0.0173, -0.0171),
                 tolerance = 0.01)
    expect_equal(ef$CI_low,
                 c(0, 0, 0, 0, 0, 0, 0),
                 tolerance = 0.01)
    # TODO Why are we getting diferent results on different systems and R releases?
    # expect_equal(ef$CI_high,
    #              c(0.5814, 0.5036, 0.5052, 0.4589, 0.3023, 0, 0),
    #              tolerance = 0.01)
  })



  # afex --------------------------------------------------------------------
  test_that("afex | within-mixed", {
    skip_if_not_installed("afex")

    data(obk.long, package = "afex")

    mod <- afex::aov_ez("id", "value", obk.long, between = c("treatment", "gender"),
                        within = c("phase", "hour"),
                        observed = "gender",
                        anova_table = list(correction = "none"))

    x <- eta_squared(mod, generalized = TRUE)
    a <- anova(mod)
    r <- cor(a$ges[order(rownames(a))], x$Eta2_generalized[order(x$Parameter)])
    expect_equal(r, 1, tolerance = 0.005)

    x <- eta_squared(mod)
    a <- anova(mod, es = "pes")
    r <- cor(a$pes[order(rownames(a))], x$Eta2_partial[order(x$Parameter)])
    expect_equal(r, 1, tolerance = 0.005)
  })


  # car ---------------------------------------------------------------------
  test_that("car MLM", {
    skip_if_not_installed("afex")
    skip_if_not_installed("car")
    data(obk.long, package = "afex")
    model1 <- afex::aov_car(value ~ treatment * gender + Error(id / (phase * hour)),
      data = obk.long, observed = "gender",
      include_aov = FALSE
    )

    ws <- capture_warnings(eta_squared(model1$Anova, partial = FALSE))
    expect_match(tail(ws, 1), "Currently only supports partial eta", fixed = TRUE)

    expect_equal(
      eta_squared(model1$Anova, verbose = FALSE)[1:3, ][[2]],
      c(0.4407468, 0.2678884, 0.3635011),
      tolerance = 0.01
    )
  })


  # Include intercept -------------------------------------------------------
  test_that("include_intercept | car", {
    skip_if_not_installed("car")

    m <- lm(mpg ~ factor(cyl) * factor(am), data = mtcars)
    AOV <- car::Anova(m, type = 3)

    res0 <- eta_squared(AOV, verbose = FALSE)
    res1 <- eta_squared(AOV, include_intercept = TRUE, verbose = FALSE)
    expect_equal(nrow(res0), 3)
    expect_equal(nrow(res1), nrow(res0) + 1)
    expect_equal(res1[[1]][1], "(Intercept)")
    expect_equal(res1[[2]][1], 0.8680899, tolerance = 0.01)

    res0 <- epsilon_squared(AOV, verbose = FALSE)
    res1 <- epsilon_squared(AOV, include_intercept = TRUE, verbose = FALSE)
    expect_equal(nrow(res0), 3)
    expect_equal(nrow(res1), nrow(res0) + 1)
    expect_equal(res1[[1]][1], "(Intercept)")


    res0 <- omega_squared(AOV, verbose = FALSE)
    res1 <- omega_squared(AOV, include_intercept = TRUE, verbose = FALSE)
    expect_equal(nrow(res0), 3)
    expect_equal(nrow(res1), nrow(res0) + 1)
    expect_equal(res1[[1]][1], "(Intercept)")

    # generalized
    res1 <- eta_squared(AOV, generalized = "cyl", include_intercept = TRUE, verbose = FALSE)
    expect_equal(res1[[1]][1], "(Intercept)")
    expect_equal(res1[[2]][1], 0.784483, tolerance = 0.01)
  })


  test_that("include_intercept | afex", {
    skip_if_not_installed("afex")
    data(obk.long, package = "afex")

    suppressWarnings(suppressMessages(
      a <- afex::aov_car(value ~ treatment * gender + Error(id),
        include_aov = TRUE,
        data = obk.long
      )
    ))

    resE0 <- eta_squared(a, verbose = FALSE)
    resA0 <- anova(a, es = "pes")
    expect_equal(nrow(resE0), 3)
    expect_equal(nrow(resE0), nrow(resA0))


    resE1 <- eta_squared(a, include_intercept = TRUE, verbose = FALSE)
    resA1 <- anova(a, es = "pes", intercept = TRUE)
    expect_equal(nrow(resE1), nrow(resE0) + 1)
    expect_equal(nrow(resE1), nrow(resA1))

    skip_if_not_installed("car")
    resE1 <- eta_squared(car::Anova(a$aov, type = 3), include_intercept = TRUE, generalized = "gender", verbose = FALSE)
    resA1 <- anova(a, es = "ges", intercept = TRUE, observed = "gender")
    expect_equal(resE1[[2]][1], 0.9386555, tolerance = 0.01)
    expect_equal(resE1[[2]][1], resA1[[5]][1], tolerance = 0.01)
  })


  # tidymodels --------------------------------------------------------------
  test_that("ets_squared | tidymodels", {
    skip_on_cran()
    skip_if_not_installed("tidymodels")
    require("tidymodels", quietly = TRUE)

    set.seed(123)
    mod_lm <-
      linear_reg() %>%
      set_engine("lm") %>%
      set_mode("regression") %>%
      fit(mpg ~ am + vs, data = mtcars)

    set.seed(123)
    tidy_lm <- eta_squared(mod_lm)
    lm_lm <- eta_squared(lm(mpg ~ am + vs, data = mtcars))

    expect_equal(tidy_lm, lm_lm, tolerance = 0.001)
  })
}
