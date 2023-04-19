test_that("d/rbs_to_cles | numeric", {
  # Null ----------------------
  expect_equal(d_to_overlap(0), 1)
  expect_equal(d_to_u1(0), 0)

  expect_equal(d_to_p_superiority(0), 0.5)
  expect_equal(rb_to_p_superiority(0), 0.5)
  expect_equal(d_to_u3(0), 0.5)
  expect_equal(d_to_u2(0), 0.5)


  # +ive -----------------------
  expect_true(d_to_overlap(1) < 1)
  expect_true(d_to_u1(1) > 0)

  expect_true(d_to_p_superiority(1) > 0.5)
  expect_true(rb_to_p_superiority(1) > 0.5)
  expect_true(d_to_u3(1) > 0.5)
  expect_true(d_to_u2(1) > 0.5)


  # -ive -----------------------
  expect_true(d_to_overlap(-1) < 1)
  expect_true(d_to_u1(-1) > 0)

  expect_true(d_to_p_superiority(-1) < 0.5)
  expect_true(rb_to_p_superiority(-1) < 0.5)
  expect_true(d_to_u3(-1) < 0.5)
  expect_true(d_to_u2(-1) > 0.5)
})

test_that("d_to_cles | from Cohens d", {
  d1 <- cohens_d(1:3, 1:4)
  d2 <- cohens_d(1:4, 1:3)

  expect_equal(d_to_p_superiority(d1)[[1]], 1 - d_to_p_superiority(d2)[[1]])
  expect_equal(d_to_u3(d1)[[1]], 1 - d_to_u3(d2)[[1]])

  expect_equal(d_to_u1(d1), d_to_u1(d2), tolerance = 0.001)
  expect_equal(d_to_u2(d1), d_to_u2(d2), tolerance = 0.001)
  expect_equal(d_to_overlap(d1), d_to_overlap(d2), tolerance = 0.001)
})

test_that("d_to_cles | from Cohens d | one sided | negative d", {
  d0 <- cohens_d(mpg ~ am, data = mtcars, ci = 0.9)
  d1 <- cohens_d(mpg ~ am, data = mtcars, alternative = "l")
  d2 <- cohens_d(mpg ~ am, data = mtcars, alternative = "g")

  # P -----------
  p0 <- d_to_p_superiority(d0)
  p1 <- d_to_p_superiority(d1)
  p2 <- d_to_p_superiority(d2)
  expect_equal(p1$CI_high, p0$CI_high, tolerance = 0.001)
  expect_equal(p2$CI_low, p0$CI_low, tolerance = 0.001)
  expect_output(print(p1), "lower")
  expect_output(print(p2), "upper")

  # U3 -----------
  u30 <- d_to_u3(d0)
  u31 <- d_to_u3(d1)
  u32 <- d_to_u3(d2)
  expect_equal(u31$CI_high, u30$CI_high, tolerance = 0.001)
  expect_equal(u32$CI_low, u30$CI_low, tolerance = 0.001)
  expect_output(print(p1), "lower")
  expect_output(print(p2), "upper")



  # U1 -----------
  u10 <- d_to_u1(d0)
  u11 <- d_to_u1(d1)
  u12 <- d_to_u1(d2)
  expect_equal(u31$CI_high, u30$CI_high, tolerance = 0.001)
  expect_equal(u32$CI_low, u30$CI_low, tolerance = 0.001)
  expect_output(print(p1), "lower")
  expect_output(print(p2), "upper")


  # U2 -----------
  u20 <- d_to_u2(d0)
  u21 <- d_to_u2(d1)
  u22 <- d_to_u2(d2)
  expect_equal(u22$CI_high, u20$CI_high, tolerance = 0.001)
  expect_equal(u21$CI_low, u20$CI_low, tolerance = 0.001)
  expect_output(print(u21), "upper")
  expect_output(print(u22), "lower")

  # OVL -----------
  OVL0 <- d_to_overlap(d0)
  OVL1 <- d_to_overlap(d1)
  OVL2 <- d_to_overlap(d2)
  expect_equal(OVL1$CI_high, OVL0$CI_high, tolerance = 0.001)
  expect_equal(OVL2$CI_low, OVL0$CI_low, tolerance = 0.001)
  expect_output(print(OVL1), "lower")
  expect_output(print(OVL2), "upper")
})
