if (require("testthat") && require("effectsize")) {
  test_that("interpret_fit functions", {
    expect_snapshot(
      list(
        interpret_gfi(c(.5, .99)),
        interpret_agfi(c(.5, .99)),
        interpret_nfi(c(.5, .99)),
        interpret_nnfi(c(.5, .99)),
        interpret_cfi(c(.5, .99)),
        interpret_rmsea(c(.5, .99)),
        interpret_srmr(c(.5, .99)),
        interpret_rfi(c(.5, .99)),
        interpret_ifi(c(.5, .99)),
        interpret_pnfi(c(.5, .99))
      )
    )
  })
}
