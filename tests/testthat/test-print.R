if (require("testthat") && require("effectsize")) {
  test_that("print", {
    set.seed(123)
    expect_snapshot(print(effectsize(t.test(1:10, y = c(7:20)))))

    set.seed(123)
    expect_snapshot(print(effectsize(chisq.test(as.table(rbind(
      c(762, 327, 468),
      c(484, 239, 477),
      c(484, 239, 477)
    ))))))

    set.seed(123)
    expect_snapshot(print(effectsize(oneway.test(extra ~ group, data = sleep, var.equal = TRUE))))

    set.seed(123)
    model <- aov(mpg ~ factor(am) * factor(cyl), data = mtcars)
    es <- eta_squared(model)
    expect_snapshot(print(equivalence_test(es, range = 0.15)))

    expect_snapshot(print(rules(c("small" = 0.2, "medium" = 0.5), name = "Cohen's Rules")))
  })
}
