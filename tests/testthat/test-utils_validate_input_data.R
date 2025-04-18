test_that(".get_data_2_samples", {
  df <- data.frame(
    a = 1:10,
    b = 2:11,
    c = rep(letters[1:2], each = 5),
    d = c("a", "b", "b", "c", "c", "b", "c", "a", "a", "b"),
    e = rep(0:1, each = 5)
  )
  df$exp_a <- exp(df$a)
  a2 <- 1:11

  expect_error(d1 <- cohens_d(a ~ c, data = df), regexp = NA)
  expect_error(d2 <- cohens_d("a", "c", data = df), regexp = NA)
  expect_error(d3 <- cohens_d(df$a ~ df$c), regexp = NA)
  expect_error(d4 <- cohens_d(df$a, df$c), regexp = NA)
  expect_error(d5 <- cohens_d(df$a[df$c == "a"], df$a[df$c == "b"]), regexp = NA)
  expect_equal(d1, d2)
  expect_equal(d1, d3)
  expect_equal(d1, d4)
  expect_equal(d1, d5)

  expect_error(cohens_d("a", "b", data = df), regexp = NA)
  expect_error(cohens_d(a2, df$b), regexp = NA)
  expect_error(cohens_d(b ~ e, data = df), regexp = NA)

  expect_equal(
    cohens_d(exp(a) ~ c, data = df),
    cohens_d("exp_a", "c", data = df)
  )


  expect_error(cohens_d(a ~ b, data = df), "exactly")
  expect_error(cohens_d(a ~ d, data = df), "exactly")
  expect_error(cohens_d("a", "d", data = df), "exactly")
  expect_error(cohens_d("c", "c", data = df), "non-numeric")
  expect_error(cohens_d(a2, df$c), "length")
  expect_error(cohens_d("a", "aa", data = df), "missing")

  expect_warning(cohens_d("b", "e", data = df), "convert")

  x <- c(1, NA, 2, 3, 4)
  y <- c(1, 2, 3, 4, 5)

  expect_warning(d1 <- cohens_d(x, y), "dropped")
  expect_warning(d2 <- cohens_d(x, y, paired = TRUE), "dropped")
  expect_equal(d1, cohens_d(1:4, 1:5), tolerance = 0.01) # indep
  expect_equal(d2, cohens_d(1:4, c(1, 3:5), paired = TRUE), tolerance = 0.01) # paired

  # no length problems
  expect_error(cohens_d(mtcars$mpg - 23), regexp = NA)

  # Missing factor levels: the actual levels in the data are 3rd and 4th
  f <- factor(letters[1:2], levels = c("d", "e", "a", "b"))
  f <- rep(f, each = 5)
  y <- c(2, 4, 3, 5, 1, 7, 9, 8, 6, 1)
  expect_error(d <- cohens_d(y, f), regexp = NA)
  expect_true(attr(d, "pooled_sd"))
})

test_that(".get_data_2_samples | na.action", {
  data("mtcars")
  mtcars$mpg[1] <- NA
  expect_warning(d1 <- cohens_d(mpg ~ am, data = mtcars), "dropped")
  expect_warning(d2 <- cohens_d(mpg ~ am, data = mtcars, na.action = na.omit), NA)
})

test_that(".get_data_2_samples | subset", {
  expect_error(cohens_d(mpg ~ cyl, data = mtcars), "exactly")
  expect_error(cohens_d(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

  expect_error(rank_biserial(mpg ~ cyl, data = mtcars), "exactly")
  expect_error(rank_biserial(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

  expect_error(sd_pooled(mpg ~ cyl, data = mtcars), "exactly")
  expect_error(sd_pooled(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

  expect_error(cohens_u1(mpg ~ cyl, data = mtcars), "exactly")
  expect_error(cohens_u1(mpg ~ cyl, data = mtcars, subset = cyl %in% c(4, 6)), regexp = NA)

  d1 <- cohens_d(mpg ~ cyl,
    data = mtcars,
    subset = cyl < 8
  )

  x <- mtcars$cyl < 8
  d2 <- cohens_d(mpg ~ cyl,
    data = mtcars,
    subset = x
  )

  x <- mtcars$cyl
  d3 <- cohens_d(mpg ~ cyl,
    data = mtcars,
    subset = x < 8
  )

  d4 <- cohens_d(mpg ~ cyl,
    data = mtcars,
    subset =
      c(
        TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
        TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,
        FALSE, TRUE
      )
  )

  expect_equal(d1, d2)
  expect_equal(d1, d3)
  expect_equal(d1, d4)
})

test_that(".get_data_2_samples | reference", {
  # create data
  my_tib <- data.frame(
    group = gl(2, 12, labels = c("No treatment", "Treatment")),
    outcome = c(3, 1, 5, 4, 6, 4, 6, 2, 0, 5, 4, 5, 4, 3, 6, 6, 8, 5, 5, 4, 2, 5, 7, 5)
  )
  my_tib$group_chr <- as.character(my_tib$group)

  # fomula input w/ factor
  expect_identical(
    cohens_d(outcome ~ group, data = my_tib)[[1]],
    -cohens_d(outcome ~ group, data = my_tib, reference = "No treatment")[[1]]
  )

  # fomula input w/ character
  expect_identical(
    cohens_d(outcome ~ group_chr, data = my_tib)[[1]],
    -cohens_d(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  )

  # vector input w/ factor
  expect_identical(
    cohens_d(my_tib$outcome, my_tib$group)[[1]],
    -cohens_d(my_tib$outcome, my_tib$group, reference = "No treatment")[[1]]
  )

  # vector input w/ character
  expect_identical(
    cohens_d(my_tib$outcome, my_tib$group_chr)[[1]],
    -cohens_d(my_tib$outcome, my_tib$group_chr, reference = "No treatment")[[1]]
  )

  # name input w/ factor
  expect_identical(
    cohens_d("outcome", "group", data = my_tib)[[1]],
    -cohens_d("outcome", "group", data = my_tib, reference = "No treatment")[[1]]
  )

  # name input w/ character
  expect_identical(
    cohens_d("outcome", "group_chr", data = my_tib)[[1]],
    -cohens_d("outcome", "group_chr", data = my_tib, reference = "No treatment")[[1]]
  )

  # sign is opposite, same value
  expect_identical(
    rank_biserial(outcome ~ group_chr, data = my_tib)[[1]],
    -rank_biserial(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  )

  # inverse
  expect_equal(
    means_ratio(outcome ~ group_chr, data = my_tib)[[1]],
    1/means_ratio(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  )

  # sum to 1
  expect_equal(
    cohens_u3(outcome ~ group_chr, data = my_tib)[[1]],
    1 - cohens_u3(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  )

  # sum to 1
  expect_equal(
    p_superiority(outcome ~ group_chr, data = my_tib)[[1]],
    1 - p_superiority(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  )

  # sign is opposite but so is value
  delta1 <- glass_delta(outcome ~ group_chr, data = my_tib)[[1]]
  delta2 <- glass_delta(outcome ~ group_chr, data = my_tib, reference = "No treatment")[[1]]
  expect_true(sign(delta1) == -sign(delta2))
  expect_true(abs(delta1) != abs(delta2))




  data("sleep")
  sleep2 <- reshape(sleep,
                    direction = "wide",
                    idvar = "ID", timevar = "group"
  )

  # formula w/ Pair()
  expect_identical(
    hedges_g(Pair(extra.1, extra.2) ~ 1, data = sleep2, verbose = FALSE)[[1]],
    -hedges_g(Pair(extra.1, extra.2) ~ 1, data = sleep2, , verbose = FALSE, reference = "extra.1")[[1]]
  )


  # formula w/ arbitrary Pair()
  expect_identical(
    cohens_d(Pair(extra[group == 1] + pi, extra[group == 2]) ~ 1, data = sleep, verbose = FALSE)[[1]],
    -cohens_d(Pair(extra[group == 1] + pi, extra[group == 2]) ~ 1, data = sleep, verbose = FALSE, reference = "extra[group == 1] + pi")[[1]]
  )
})

test_that(".get_data_multi_group", {
  df <- data.frame(
    a = 1:15,
    b = 2:16,
    c = rep(letters[1:3], each = 5),
    e = rep(0:1, length = 15)
  )
  df$exp_a <- exp(df$a)

  expect_error(d1 <- rank_epsilon_squared(a ~ c, data = df, ci = NULL), regexp = NA)
  expect_error(d2 <- rank_epsilon_squared("a", "c", data = df, ci = NULL), regexp = NA)
  expect_error(d3 <- rank_epsilon_squared(df$a ~ df$c, ci = NULL), regexp = NA)
  expect_error(d4 <- rank_epsilon_squared(df$a, df$c, ci = NULL), regexp = NA)
  L <- split(df$a, df$c)
  expect_error(d5 <- rank_epsilon_squared(L, ci = NULL), regexp = NA)
  expect_equal(d1, d2, tolerance = 0.01)
  expect_equal(d1, d3, tolerance = 0.01)
  expect_equal(d1, d4, tolerance = 0.01)
  expect_equal(d1, d5, tolerance = 0.01)

  expect_error(rank_epsilon_squared(b ~ e, data = df), regexp = NA)

  expect_equal(
    rank_epsilon_squared(exp(a) ~ c, data = df, ci = NULL),
    rank_epsilon_squared("exp_a", "c", data = df, ci = NULL)
  )

  expect_error(rank_epsilon_squared("c", "c", data = df), "non-numeric")
  expect_error(rank_epsilon_squared("a", "aa", data = df), "missing")

  df[1, ] <- NA
  expect_warning(E1 <- rank_epsilon_squared(a ~ c, data = df, ci = NULL), "dropped")
  expect_equal(E1, rank_epsilon_squared(df$a[-1], df$c[-1], ci = NULL))
})


test_that(".get_data_multi_group | subset", {
  d <- expand.grid(id = 1:30, g = 1:4)
  d$y <- rnorm(nrow(d)) + d$g
  expect_equal(
    rank_epsilon_squared(y ~ g, data = d, subset = g < 4, ci = NULL),
    rank_epsilon_squared(y ~ g, data = subset(d, g < 4), ci = NULL)
  )
})

test_that(".get_data_nested_groups", {
  skip_if_not_installed("base", minimum_version = "3.6.1")
  M1 <- cbind(
    "Round Out" = c(5.4, 5.85, 5.2),
    "Narrow Angle" = c(5.5, 5.7, 5.6),
    "Wide Angle" = c(5.55, 5.75, 5.5)
  )

  M2 <- data.frame(
    id = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    name = c(
      "Round Out", "Narrow Angle", "Wide Angle",
      "Round Out", "Narrow Angle", "Wide Angle",
      "Round Out", "Narrow Angle", "Wide Angle"
    ),
    value = c(5.4, 5.5, 5.55, 5.85, 5.7, 5.75, 5.2, 5.6, 5.5)
  )

  set.seed(1)
  W1 <- kendalls_w(M1, ci = NULL)
  W2 <- kendalls_w(value ~ name | id, data = M2, ci = NULL)
  W3 <- kendalls_w(M2$value, M2$name, M2$id, ci = NULL)
  W4 <- kendalls_w(M2$value ~ M2$name | M2$id, ci = NULL)

  expect_equal(W1, W2)
  expect_equal(W1, W3)
  expect_equal(W1, W4)
})

test_that(".get_data_nested_groups | subset", {
  d <- expand.grid(id = 1:30, g = 1:4)
  d$y <- rnorm(nrow(d)) + d$g

  expect_equal(
    kendalls_w(y ~ g | id, data = d, subset = g < 4, ci = NULL),
    kendalls_w(y ~ g | id, data = subset(d, g < 4), ci = NULL)
  )
})


test_that(".get_data_multivariate", {
  data("mtcars")
  D <- mahalanobis_d(mtcars[, c("mpg", "hp")])
  expect_equal(mahalanobis_d(cbind(mpg, hp) ~ 1, data = mtcars), D)
  expect_equal(mahalanobis_d(mpg + hp ~ 1, data = mtcars), D)

  D <- mahalanobis_d(
    mtcars[mtcars$am == 0, c("mpg", "hp")],
    mtcars[mtcars$am == 1, c("mpg", "hp")]
  )
  expect_equal(mahalanobis_d(cbind(mpg, hp) ~ am, data = mtcars), D)
  expect_equal(mahalanobis_d(mpg + hp ~ am, data = mtcars), D)
})

test_that(".get_data_multivariate | subset", {
  data("mtcars")
  D1 <- mahalanobis_d(mpg + hp ~ am, data = mtcars, subset = hp > 100)
  D2 <- mahalanobis_d(mpg + hp ~ am, data = subset(mtcars, hp > 100))
  expect_equal(D1, D2)
})

test_that(".get_data_multivariate | na.action", {
  data("mtcars")
  mtcars$mpg[1] <- NA
  expect_warning(mahalanobis_d(mtcars[, c("mpg", "hp")]), regexp = "dropped")
  expect_warning(mahalanobis_d(mpg + hp ~ 1, data = mtcars, na.action = na.omit), regexp = NA)
  expect_warning(D1 <- mahalanobis_d(mpg + hp ~ 1, data = mtcars), regexp = "dropped")
  expect_equal(D1, mahalanobis_d(mpg + hp ~ 1, data = mtcars[-1, ]))
})


test_that(".get_data_paired | reference", {
  data("sleep")
  sleep2 <- reshape(sleep,
                    direction = "wide",
                    idvar = "ID", timevar = "group"
  )

  # formual w/ Pair()
  expect_identical(
    repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2)[[1]],
    -repeated_measures_d(Pair(extra.1, extra.2) ~ 1, data = sleep2, reference = "extra.1")[[1]]
  )


  # 3 part formula (+aggragate)
  data("rouder2016")
  rouder2016$cond_num <- as.numeric(rouder2016$cond)

  # with factor
  expect_identical(
    repeated_measures_d(rt ~ cond | id, data = rouder2016, verbose = FALSE)[[1]],
    -repeated_measures_d(rt ~ cond | id, data = rouder2016, verbose = FALSE, reference = "2")[[1]]
  )

  expect_identical(
    repeated_measures_d(rt ~ cond_num | id, data = rouder2016, verbose = FALSE)[[1]],
    -repeated_measures_d(rt ~ cond_num | id, data = rouder2016, verbose = FALSE, reference = "2")[[1]]
  )
})
