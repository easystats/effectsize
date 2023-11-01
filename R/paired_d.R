paired_d <- function(x, y,
                     data = NULL,
                     mu = 0, type = c("z", "rm", "av", "b"),
                     adjust = TRUE,
                     ci = 0.95, alternative = "two.sided",
                     verbose = TRUE, ...) {

  alternative <- effectsize:::.match.alt(alternative)
  type <- match.arg(type)

  m <- mean(x - y)
  n <- length(x)
  df <- n - 1
  r <- cor(x, y)

  if (type == "z") {
    s <- sd(x - y)
    d <- (m - mu) / s

    se <- sqrt((1 / n) + (d ^ 2) / (2 * n))
  } else if (type == "rm") {
    f <- 2 * (1 - r)

    s <- sd(x - y) / sqrt(f)
    d <- (m - mu) / s

    se <- sqrt(((1 / n) + (d ^ 2) / (2 * n)) * f)
  } else if (type == "av") {
    s <- sqrt((var(x) + var(y)) / 2)
    d <- (m - mu) / s

    se <- sqrt((2 / n) + (d ^ 2) / (4 * n))
  } else if (type == "b") {
    s <- sd(x)
    d <- (m - mu) / s

    se <- sqrt((2 * (1 - r) / n) + (d ^ 2) / (2 * n))
  }

  out <- data.frame(d = d)

  if (effectsize:::.test_ci(ci)) {
    browser()
    # Add cis
    out$CI <- ci
    ci.level <- effectsize:::.adjust_ci(ci, alternative)

    out$CI_low <- NA
    out$CI_high <- NA

    ci_method <- list(method = "normal")
    out <- effectsize:::.limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }


  if (adjust) {
    J <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) # exact method

    out[, colnames(out) %in% c("d", "CI_low", "CI_high")] <-
      out[, colnames(out) %in% c("d", "CI_low", "CI_high")] * J
  }

  # rename column to type
  colnames(out) <- paste0("d_", type)

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  effectsize:::.someattributes(out) <- effectsize:::.nlist(
    mu, ci, ci_method, alternative,
    approximate = FALSE
  )
  return(out)
}


paired_d(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], ci = NULL)
