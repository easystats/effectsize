# TODO
# document
# - references for types in docs + references in comments for ses.
# - link to `lmeInfo::g_mlm()` and `emmeans::effsize()`
# - examples
# Fix cohens d
# - to give warning with paired?
# - link back here
# add to vignette
# add proper names
# news
# tests

#' Standardized Mean Differences for Repeated Measures
#'
#' Short description... Pair with any reported `stats::t.test(paired = TRUE)`.
#'
#' @param x,y paired numeric vectors. `x` can also be a formula:
#' - `Pair(x,y) ~ 1` for wide data.
#' - `y ~ condition | id` for long data, possibly with repetitions.
#' @param type Type of repeated measures standardized differences. See details.
#' @param adjust Apply Hedges' small-sample bias correction?
#' @inheritParams cohens_d
#'
#' @inheritSection effectsize_CIs Confidence (Compatibility) Intervals (CIs)
#' @inheritSection effectsize_CIs CIs and Significance Tests
#' @inheritSection print.effectsize_table Plotting with `see`
#'
#' @note
#' `rm_d()` is an alias for `repeated_measures_d()`.
#'
#' @return A data frame with the effect size ( `Cohens_d`, `Hedges_g`,
#'   `Glass_delta`) and their CIs (`CI_low` and `CI_high`).
#'
#' @family standardized differences
#' @seealso [cohens_d()]
#'
#'
#' @export
repeated_measures_d <- function(x, y,
                                data = NULL,
                                mu = 0, type = c("rm", "av", "z", "b", "d", "r"),
                                adjust = TRUE,
                                ci = 0.95, alternative = "two.sided",
                                verbose = TRUE, ...) {

  alternative <- .match.alt(alternative)
  type <- match.arg(type)
  data <- .get_data_paired(x, y, data = data, type = type, verbose = verbose, ...)

  if (type %in% c("d", "r")) {
    values <- .replication_d(data, mu = mu, type = type)
  } else {
    values <- .paired_d(data, mu = mu, type = type)
  }

  out <- data.frame(d = values[["d"]])

  if (.test_ci(ci)) {
    # Add cis
    out$CI <- ci
    ci.level <- .adjust_ci(ci, alternative)

    alpha <- 1 - ci.level
    probs <- c(alpha / 2, 1 - alpha / 2)
    qs <- stats::qnorm(probs)

    confint <- out[["d"]] + qs * values[["se"]]
    out$CI_low <- confint[1]
    out$CI_high <- confint[2]

    ci_method <- list(method = "normal")
    out <- .limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }


  if (adjust) {
    df <- values[["df"]]
    J <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) # exact method

    out[, colnames(out) %in% c("d", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("d", "CI_low", "CI_high")] * J
  }

  # rename column to type
  colnames(out)[1] <- paste0("d_", type)

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  .someattributes(out) <- .nlist(
    mu, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}

#' @rdname repeated_measures_d
#' @export
rm_d <- repeated_measures_d

#' @keywords internal
.paired_d <- function(data, mu, type) {
  x <- data[["x"]]
  y <- data[["y"]]

  m <- mean(x - y)
  n <- length(x)
  df <- n - 1
  r <- cor(x, y)

  if (type == "rm") {
    f <- 2 * (1 - r)

    s <- sd(x - y) / sqrt(f)
    d <- (m - mu) / s

    se <- sqrt(((1 / n) + (d ^ 2) / (2 * n)) * f)
  } else if (type == "av") {
    s <- sqrt((var(x) + var(y)) / 2)
    d <- (m - mu) / s

    se <- sqrt((2 / n) + (d ^ 2) / (4 * n))
  } else if (type == "z") {
    s <- sd(x - y)
    d <- (m - mu) / s

    se <- sqrt((1 / n) + (d ^ 2) / (2 * n))
  } else if (type == "b") {
    s <- sd(x)
    d <- (m - mu) / s

    se <- sqrt((2 * (1 - r) / n) + (d ^ 2) / (2 * n))
  }

  .nlist(d, se, df)
}

#' @keywords internal
.replication_d <- function(data, mu, type) {
  if (type == "r") {
    # for r - need to make sure there are replications!
    cell_ns <- tapply(data[[1]], data[3:2], function(v) length(stats::na.omit(v)))
    all(cell_ns > 1L)
  }

  mod <- stats::aov(y ~ condition + Error(id / condition), data = data,
                    contrasts = list(condition = contr.treatment))

  m <- -unname(coef(mod[["id:condition"]]))
  m_V <- unname(vcov(mod[["id:condition"]])[1])

  pars <- parameters::model_parameters(mod)

  if (type == "d") {
    e <- as.data.frame(pars[pars$Parameter == "Residuals",])
  } else if (type == "r") {
    e <- as.data.frame(pars[pars$Group == "Within",])
  }

  s <- sqrt(sum(e[["Sum_Squares"]]) / sum(e[["df"]]))
  df <- sum(e[["df"]])

  d <- (m - mu) / s
  se <- sqrt((df / (df - 2)) * (m_V / (s^2)) + (d ^ 2) * (8 * df ^2 - df + 2) / (16*(df-2)*((df-1)^2)))

  .nlist(d, se, df)
}

# # examples ----------------------------------------
#
# # Paired data
# data(sleep)
#
# repeated_measures_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "rm")
# #> d_rm  |         95% CI
# #> ----------------------
# #> -0.75 | [-1.17, -0.33]
# repeated_measures_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "av")
# #> d_av  |        95% CI
# #> ---------------------
# #> -0.76 | [-1.60, 0.07]
# repeated_measures_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "z")
# #> d_z   |         95% CI
# #> ----------------------
# #> -1.17 | [-1.94, -0.41]
# repeated_measures_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "b")
# #> d_b   |         95% CI
# #> ----------------------
# #> -0.81 | [-1.31, -0.30]
#
# repeated_measures_d(Pair(extra[group == 1], extra[group == 2]) ~ 1, data = sleep, type = "d")
# #> d_d   |         95% CI
# #> ----------------------
# #> -0.80 | [-1.30, -0.30]
#
#
# dat <- read.table("effectSizePuzzler.txt", header = TRUE)
#
# repeated_measures_d(rt ~ cond | id, data = dat, type = "rm")
# #> d_rm  |         95% CI
# #> ----------------------
# #> -0.80 | [-1.06, -0.53]
# repeated_measures_d(rt ~ cond | id, data = dat, type = "av", adjust = FALSE) # jakewestfall.org/blog: 0.84
# #> d_av  |         95% CI
# #> ----------------------
# #> -0.84 | [-1.41, -0.26]
# repeated_measures_d(rt ~ cond | id, data = dat, type = "z", adjust = FALSE) # jakewestfall.org/blog: 1.35
# #> d_z   |         95% CI
# #> ----------------------
# #> -1.35 | [-1.90, -0.81]
# repeated_measures_d(rt ~ cond | id, data = dat, type = "b")
# #> d_b   |         95% CI
# #> ----------------------
# #> -0.86 | [-1.19, -0.53]
#
# repeated_measures_d(rt ~ cond | id, data = dat, type = "d") # jakewestfall.org/blog: 0.25
# #> d_d   |         95% CI
# #> ----------------------
# #> -0.25 | [-0.32, -0.18]
#
# repeated_measures_d(rt ~ cond | id, data = dat, type = "r") # jakewestfall.org/blog: 0.26
# #> d_r   |         95% CI
# #> ----------------------
# #> -0.26 | [-0.33, -0.18]
