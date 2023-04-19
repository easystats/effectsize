cohens_d_rm <- function(x, group, block, data = NULL,
                        type = c("d", "a", "r", "z", "t", "av", "rm"),
                        mu = 0,
                        ci = 0.95, alternative = "two.sided",
                        verbose = TRUE, ...) {
  type <- match.arg(type)
  alternative <- effectsize:::.match.alt(alternative)

  data <- effectsize:::.get_data_nested_groups(x, group, block, data, wide = FALSE)
  if (!is.factor(data$groups)) data$groups <- factor(data$groups)
  if (!is.factor(data$blocks)) data$blocks <- factor(data$blocks)
  contrasts(data$groups) <- contr.treatment
  contrasts(data$blocks) <- contr.treatment

  stopifnot(nlevels(data$groups) == 2L)


  if (type %in% c("a", "z", "t", "av", "rm")) {
    browser()
    data <- aggregate(data$x, data[-1], mean)
    xtab <- table(data[-3])
    data <- data[data$blocks %in% colnames(xtab)[colSums(xtab) %in% 2], ]
    colnames(data)[3] <- "x"
  }

  if (type %in% c("z", "t")) {
    out <- .paired_d_z_t(data, type, mu = mu, ci = ci, alternative = alternative)
    return(out)
  }

  insight::check_if_installed("emmeans")
  insight::check_if_installed("nlme")

  if (type %in% c("av", "rm")) {
    out <- .paired_d_av_rm(data, type, mu = mu, ci = ci, alternative = alternative)
  } else if (type %in% c("d", "a", "r")) {
    out <- .paired_d_d_a_r(data, type, mu = mu, ci = ci, alternative = alternative)
  }

  attr(out, "paired_type") <- type
  out
}

.paired_d_z_t <- function(data, type,
                          mu = 0, ci = 0.95, alternative = "two.sided") {
  data <- data[order(data$groups), ]
  data <- aggregate(data$x, data[2], diff)

  out <- cohens_d(-data$x, mu = mu, ci = ci, alternative = alternative)
  colnames(out)[1] <- "d_z"

  if (type == "t") {
    colnames(out)[1] <- "d_t"
    out[colnames(out) %in% c("d_t", "CI_low", "CI_high")] <-
      sqrt(2) * out[colnames(out) %in% c("d_t", "CI_low", "CI_high")]
  }

  out
}

.paired_d_d_a_r <- function(data, type,
                            mu = 0, ci = 0.95, alternative = "two.sided") {
  # Use nlme decomp
  contrasts(data$groups) <- stats::contr.sum
  contrasts(data$blocks) <- stats::contr.sum

  mod <- nlme::lme(x ~ groups,
                    random = ~ groups | blocks,
                    data = data)

  if (type == "r") {
    s <- sqrt(insight::get_variance_residual(mod))
    df <- insight::get_df(mod)[1]
  } else if (type %in% c("d", "a")) {
    var <- insight::get_variance(mod)
    s <- sqrt(var[["var.random"]] + var[["var.residual"]])
    df <- insight::n_obs(mod) - 2
  }

  Means <- emmeans::emmeans(mod, ~ groups)
  Diff <- emmeans::contrast(Means, method = "pairwise", offset = -mu)
  Effect <- emmeans::eff_size(Diff, sigma = s, edf = df, method = "identity")

  effectsize:::.test_ci(ci)

  em_side <- c("two.sided" = 0,
               "less" = -1,
               "greater" = +1)[alternative]

  out <- summary(Effect, infer = !is.null(ci), level = ci, side = em_side)
  out <- data.frame(out)
  out$CI <- ci
  out <- out[intersect(c("effect.size", "CI", "lower.CL", "upper.CL"), colnames(out))]
  colnames(out) <- c(paste0("d_", type), "CI", "CI_low", "CI_high")[seq_len(length(out))]

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  # attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

.paired_d_av_rm <- function(data, type,
                            mu = 0, ci = 0.95, alternative = "two.sided") {
  data <- data[order(data[[1]], data[[2]]), ]

  d <- -diff(tapply(data$x, data$groups, mean))
  ss <- tapply(data$x, data$groups, sd)

  r <- cor(
    data$x[data$groups == data$groups[1]],
    data$x[data$groups != data$groups[1]]
  )
  co <- cov(
    data$x[data$groups == data$groups[1]],
    data$x[data$groups != data$groups[1]]
  )
  n <- nrow(data) / 2

  Sdiff <- sqrt(sum(ss^2) - 2 * co)

  if (type == "av") {
    out <- data.frame(d_av = (d - mu) / mean(ss))
  } else if (type == "rm") {
    out <- data.frame(d_rm = sqrt(2 * (1 - r)) * (d - mu) / Sdiff)
  }


  if (effectsize:::.test_ci(ci)) {
    out$CI <- ci
    ci.level <- effectsize:::.adjust_ci(ci, alternative)

    tc <- qt(0.5 + ci.level / 2, df = n - 1, lower.tail = TRUE)
    se <- sqrt((2 * Sdiff^2) / (n * sum(ss)))

    out$CI_low <- out[[1]] - tc * se
    out$CI_high <- out[[1]] + tc * se
    ci_method <- list(method = "normal")
    out <- effectsize:::.limit_ci(out, alternative, -Inf, Inf)
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

# .sample_within <- function(.data) {
#   .data |>
#     tidyr::nest(data = -id) |>
#     dplyr::slice_sample(prop = 1, replace = TRUE) |>
#     dplyr::mutate(id = seq_along(id)) |>
#     tidyr::unnest(cols = data)
# }


#' @keywords internal
.deltamethod <- function(g, mean, cov, ses = TRUE) {
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g)) {
    g <- list(g)
  }
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) {
    insight::format_error(
      "Covariances should be a ", n, " by ", n,
      " matrix."
    )
  }
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(stats::deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  } else {
    new.covar
  }
}
