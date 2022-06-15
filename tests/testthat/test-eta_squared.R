if (require("testthat") && require("effectsize")) {

  # anova() -----------------------------------------------------------------
  test_that("anova()", {
    # Make minimal ANOVA table
    mod <- anova(lm(mpg ~ cyl + hp, mtcars))

    mod1 <- mod
    mod1$DenDF <- mod1$Df[nrow(mod1)]
    mod1 <- mod1[-nrow(mod1),]

    expect_error(eta_squared(mod1), regexp = NA)
    expect_equal(
      eta_squared(mod1)[, -1],
      F_to_eta2(mod1[["F value"]], mod1$Df, mod1$DenDF),
      ignore_attr = TRUE
    )
    expect_warning(eta_squared(mod1, partial = FALSE))
    expect_warning(eta_squared(mod1, generalized = TRUE))

    mod2 <- mod1
    mod2$`F value` <- NULL
    expect_error(eta_squared(mod2))
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

    #### Alternative
    m <<- aov(mpg ~ factor(gear) + am, mtcars)
    et1 <- eta_squared(m)
    et2 <- eta_squared(m, ci = 0.9, alternative = "two.sided")
    expect_equal(et1$CI_low, et2$CI_low)

    ### parameters:
    expect_equal(eta_squared(parameters::model_parameters(m)),
                 eta_squared(m))
  })


  # aovlist -----------------------------------------------------------------
  test_that("aovlist", {
    skip_on_cran()
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    model <<- aov(Sepal.Length ~ Sepal.Big + Error(Species), data = df)

    res <- eta_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    expect_equal(res$Eta2_partial, c(0.4472423, 0.1217329), tolerance = 0.001)
    expect_equal(eta_squared(model, partial = FALSE)$Eta2,
                 c(0.27671136, 0.04641607), tolerance = 0.001)

    res <- omega_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    expect_equal(res$Omega2_partial, c(-0.06795358, 0.04141846), tolerance = 0.001)
    expect_equal(omega_squared(model, partial = FALSE)$Omega2,
                 c(-0.04864626, 0.03287821), tolerance = 0.001)

    res <- epsilon_squared(model, partial = TRUE)
    expect_true(all(c("Group", "Parameter") %in% colnames(res)))
    expect_equal(res$Epsilon2_partial, c(-0.1055154, 0.1157174), tolerance = 0.001)
    expect_equal(epsilon_squared(model, partial = FALSE)$Epsilon2,
                 c(-0.06528301, 0.04412238), tolerance = 0.001)


    expect_equal(eta_squared(parameters::model_parameters(model)),
                 eta_squared(model))


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

    a1 <- eta_squared(model2, partial = FALSE)
    a2 <- eta_squared(model$aov, partial = FALSE)

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

    # Row order
    fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
    out <- eta_squared(fit, partial = FALSE, ci = NULL)
    expect_equal(as.character(out$Response), c("mpg", "disp", "hp"))

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


    ef <- omega_squared(m, partial = TRUE, alternative = "two")
    expect_equal(ef$Omega2_partial,
      c(0.3115, 0.1814, 0.2221, 0.2637, 0.1512, -0.0173, -0.0171),
      tolerance = 0.01
    )
    expect_equal(ef$CI_low,
      c(0, 0, 0, 0, 0, 0, 0),
      tolerance = 0.01
    )

    expect_equal(ef$CI_high,
                 c(0.626, 0.553, 0.557, 0.518, 0.355, 0, 0),
                 tolerance = 0.01)
  })


  # failed CIs --------------------------------------------------------------

  test_that("failed CIs", {
    library(testthat)

    model <- aov(wt ~ cyl + Error(gear), data = mtcars)

    expect_warning(eta_squared(model), regexp = "CIs")
    expect_warning(eta <- eta_squared(model, verbose = FALSE), regexp = NA)
    expect_equal(nrow(eta), 2L)
    expect_equal(eta[1, "Eta2_partial"], 1)

    expect_warning(eta_squared(model, partial = FALSE), regexp = "CIs")
    expect_warning(eta <- eta_squared(model, partial = FALSE, verbose = FALSE), regexp = NA)
    expect_equal(nrow(eta), 2L)
    expect_equal(eta[1, "Eta2"], 0.34, tolerance = 0.01)
  })


  # Include intercept -------------------------------------------------------
  test_that("include_intercept | car", {
    skip_on_cran()
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

  # Special cases --------------------------------------------------------------

  ## afex --------------------------------------------------------------------
  test_that("afex | within-mixed", {
    skip_if_not_installed("afex")

    data(obk.long, package = "afex")

    mod <- afex::aov_ez("id", "value", obk.long,
                        between = c("treatment", "gender"),
                        within = c("phase", "hour"),
                        observed = "gender"
    )

    x <- eta_squared(mod, generalized = TRUE)
    a <- anova(mod, observed = "gender")
    expect_equal(a$ges, x$Eta2_generalized)

    x <- eta_squared(mod)
    a <- anova(mod, es = "pes")
    expect_equal(a$pes, x$Eta2_partial)


    x <- eta_squared(mod, include_intercept = TRUE)
    a <- anova(mod, es = "pes", intercept = TRUE)
    expect_equal(a$pes, x$Eta2_partial)

    # see issue #389
    data <- data.frame(subject = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L,
                                   2L, 1L, 2L, 1L, 2L, 1L, 2L),
                       y = c(0.0586978983148275, -0.159870038198774, 0.0125690871484012,
                             -0.0152529928817782, 0.092433880558952, 0.0359796249184537,
                             -0.00786545388312909, 0.0340005375703463, 0.165294695432772,
                             0.0201040753050847, 0.0741924965491503, -0.0345053066539826,
                             0.0108194665250311, -0.163941830205729, 0.310344189786906,
                             -0.106627229564326),
                       A = c("A1", "A1", "A1", "A1", "A1", "A1", "A1", "A1", "A2",
                             "A2", "A2", "A2", "A2", "A2", "A2", "A2"),
                       B = c("B1", "B1", "B1", "B1", "B2", "B2", "B2", "B2", "B1",
                             "B1", "B1", "B1", "B2", "B2", "B2", "B2"),
                       C = c("C1", "C1", "C2", "C2", "C1", "C1", "C2", "C2", "C1",
                             "C1", "C2", "C2", "C1", "C1", "C2", "C2"))
    mod <- afex::aov_ez("subject", "y", data, within = c("A", "B", "C"))
    tab <- as.data.frame(anova(mod, es = "pes"))
    res <- eta_squared(mod)

    tab <- tab[order(rownames(tab)),]
    res <- res[order(res$Parameter),]

    expect_equal(res$Eta2_partial,tab$pes, tolerance = 0.001)
  })


  test_that("afex | mixed()", {
    skip_if_not_installed("afex")
    skip_if_not_installed("lmerTest")
    skip_if(getRversion() <= "3.6")

    data(md_15.1, package = "afex")
    # random intercept plus random slope
    t15.4a <- afex::mixed(iq ~ timecat + (1 + time | id), data = md_15.1)
    expect_equal(
      eta_squared(t15.4a),
      eta_squared(t15.4a$full_model)
    )
  })


  ## car ---------------------------------------------------------------------
  test_that("car MVM", {
    skip_if_not_installed("afex")
    skip_if_not_installed("car")

    # Simple ---
    ds <- data.frame(I = c(116, 96, 120, 110, 116, 126, 86, 80),
                     II = c(76, 93, 112, 113, 75, 120, 90, 105),
                     III = c(85, 63, 89, 60, 115, 101, 129, 67),
                     IV = c(50, 87, 100, 60, 79, 70, 65, 65),
                     id = 1:8)

    ds_long <- data.frame(
      id = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
             1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
             1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L,
             1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
      ind_var = c("I", "I", "I", "I", "I", "I", "I", "I",
                  "II", "II", "II", "II", "II", "II", "II", "II",
                  "III", "III", "III", "III", "III", "III", "III", "III",
                  "IV", "IV", "IV", "IV", "IV", "IV", "IV", "IV"),
      score = c(116, 96, 120, 110, 116, 126, 86, 80,
                76, 93, 112, 113, 75, 120, 90, 105,
                85, 63, 89, 60, 115, 101, 129, 67,
                50, 87, 100, 60, 79, 70, 65, 65)
    )



    fit <- lm(cbind(I, II, III, IV) ~ 1, data = ds)
    in_rep <- data.frame(ind_var = gl(4, 1))
    A_car <- car::Anova(fit, idata = in_rep, idesign =  ~ ind_var)

    eta_car <- effectsize::eta_squared(A_car, ci = NULL)[[2]]

    eta_afex <- afex::aov_ez('id', 'score', ds_long,
                             within = 'ind_var',
                             anova_table = list(es = "pes"))$anova_table$pes

    expect_equal(eta_car, eta_afex)

    # Complex ---
    data(obk.long, package = "afex")

    mod <- afex::aov_ez("id", "value", obk.long,
                        between = c("treatment", "gender"),
                        within = c("phase", "hour"),
                        observed = "gender")
    expect_equal(
      sort(eta_squared(mod$Anova, generalized = "gender")[[2]]),
      sort(mod$anova_table$ges)
    )
  })


  test_that("Anova.mlm Manova", {
    skip_if_not_installed("car")

    data("mtcars")
    mtcars$am_f <- factor(mtcars$am)
    mtcars$cyl_f <- factor(mtcars$cyl)

    mod <- lm(cbind(mpg, qsec) ~ am_f * cyl_f, data = mtcars)

    Manova <- car::Manova(mod)

    expect_true(is.null(summary(Manova, univariate = TRUE)[["univariate.tests"]]))
    expect_error(eta_squared(Manova), regexp = NA)
    expect_equal(
      eta_squared(manova(mod))[[2]][2:3],
      eta_squared(Manova)[[2]][2:3]
    )

  })

  ## merMod --------------------

  test_that("merMod and lmerModLmerTest", {
    skip_if_not_installed("lmerTest")
    skip_if_not_installed("lme4")

    data("sleepstudy", package = "lme4")

    m <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
    mtest <- lmerTest::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

    t15.4a <- afex::mixed(iq ~ timecat + (1 + time | id), data = md_15.1)
    expect_equal(
      eta_squared(m),
      eta_squared(mtest)
    )
  })


  ## tidymodels -------------------
  test_that("ets_squared | tidymodels", {
    skip_on_cran()
    skip_if_not_installed("parsnip")

    set.seed(123)
    mod_lm <- parsnip::linear_reg(engine = "lm", mode = "regression")
    mod_lm <- parsnip::fit(mod_lm, mpg ~ am + vs, data = mtcars)

    set.seed(123)
    tidy_lm <- eta_squared(mod_lm)
    lm_lm <- eta_squared(lm(mpg ~ am + vs, data = mtcars))

    expect_equal(tidy_lm, lm_lm, tolerance = 0.001)
  })


  ## GAMs -------------------
  test_that("ets_squared | gam", {
    skip_on_cran()
    skip_if_not_installed("mgcv")

    set.seed(2) ## simulate some data...
    dat <- mgcv::gamSim(1, n = 400, dist = "normal", scale = 2)
    b <- mgcv::gam(y ~ x0 + s(x1) + s(x2) + t2(x1, x2) + s(x3), data = dat)

    expect_error(out <- eta_squared(b), regexp = NA)
    expect_output(print(out), "Type III")
  })

  ## rms -------------------
  test_that("ets_squared | rms", {
    skip_on_cran()
    skip_if_not_installed("rms")

    b <- rms::ols(mpg ~ cyl + am, data = mtcars)
    expect_error(out <- eta_squared(b), regexp = NA)
    expect_output(print(out), "Type II")

    skip_if_not_installed("car")
    skip_if(getRversion() <= "3.6" &&
              Sys.info()["sysname"] == "Darwin")
    b_lm <- car::Anova(lm(mpg ~ cyl + am, data = mtcars), type = 2)
    out_lm <- eta_squared(b_lm)
    expect_equal(out[1:2, ], out_lm, ignore_attr = TRUE)
  })
}
