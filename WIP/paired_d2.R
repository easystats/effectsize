paired_d <- function(x, group, block, data = NULL,
                     mu = 0, ci = 0.95, alternative = "two.sided",
                     type = c("d", "a", "r", "z", "t", "av", "rm")) {
  type <- match.arg(type)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  data <- effectsize:::.kendalls_w_data(x, group, block, data, wide = FALSE)
  if (!is.factor(data$groups)) data$groups <- factor(data$groups)
  if (!is.factor(data$blocks)) data$blocks <- factor(data$blocks)
  data <- na.omit(data)

  stopifnot(nlevels(data$groups) == 2L)


  if (type %in% c("a", "z", "t", "av", "rm")) {
    data <- aggregate(data$x, data[-1], mean)
    xtab <- table(data[-3])
    data <- data[data$blocks %in% colnames(xtab)[colSums(xtab) %in% 2], ]
  }

  if (type %in% c("z", "t")) {
    out <- paired_d_z_t(data, type, mu = mu, ci = ci, alternative = alternative)
  } else if (type %in% c("av", "rm")) {
    out <- paired_d_av_rm(data, type, mu = mu, ci = ci, alternative = alternative)
  } else if (type %in% c("d", "a", "r")) {
    out <- paired_d_d_a_r(data, type, mu = mu, ci = ci, alternative = alternative)
  }

  attr(out, "paired_type") <- type
  out
}

paired_d_z_t <- function(data, type,
                         mu = 0, ci = 0.95, alternative = "two.sided") {
  data <- data[order(data$groups),]
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

paired_d_d_a_r <- function(data, type,
                           mu = 0, ci = 0.95, alternative = "two.sided") {
  # Use ANOVA decomp
  mod <- stats::aov(x ~ groups + Error(blocks/groups), data = data)
  pars <- parameters::model_parameters(mod)

  # Look in:
  # d/r = groups
  # a = blocks:groups
  is_d <-
    pars$Paramete == "groups" &
    pars$df == 1 &
    pars$Group == "blocks:groups"

  d <- -unname(coef(mod[[pars$Group[is_d]]]))

  if (type %in% c("d", "a")) {
    ss <- sum(pars$Sum_Squares[!is_d])
    df <- sum(pars$df[!is_d])
    s <- sqrt(ss/df)
  } else if (type == "r") {
    is_r <- pars$Group == "Within" & pars$Parameter == "Residuals"
    ss <- pars$Sum_Squares[is_r]
    df <- pars$df[is_r]
    s <- sqrt(ss/df)
  }

  out <- data.frame(d = (d - mu)/s)
  if (type != "d") names(out) <- paste0("d_", type)

  if (!is.null(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    ssd <- ((d-mu)/2)^2*insight::n_obs(mod)
    t <- sign(d) * sqrt(ssd / (ss / df))
    ts <- effectsize:::.get_ncp_t(t, df, ci.level)

    out$CI_low <- 2 * ts[1] / sqrt(df)
    out$CI_high <- 2 * ts[2] / sqrt(df)
    ci_method <- list(method = "ncp", distribution = "t")
    if (alternative == "less") {
      out$CI_low <- -Inf
    } else if (alternative == "greater") {
      out$CI_high <- Inf
    }
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

paired_d_av_rm <- function(data, type,
                           mu = 0, ci = 0.95, alternative = "two.sided") {
  data <- data[order(data[[1]], data[[2]]),]

  d <- -diff(tapply(data$x, data$groups, mean))
  ss <- tapply(data$x, data$groups, sd)

  r <- cor(data$x[data$groups==data$groups[1]],
           data$x[data$groups!=data$groups[1]])
  co <- cov(data$x[data$groups==data$groups[1]],
            data$x[data$groups!=data$groups[1]])
  n <- nrow(data)/2

  Sdiff <- sqrt(sum(ss^2) -2*co)

  if (type == "av") {
    out <- data.frame(d_av = (d - mu) / mean(ss))
  } else if (type == "rm") {
    out <- data.frame(d_rm = sqrt(2 * (1 - r)) * (d - mu) / Sdiff)
  }


  if (!is.null(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    tc <- qt(0.5+ci.level/2, df = n-1, lower.tail = TRUE)
    se <- sqrt((2*Sdiff^2)/(n * sum(ss)))

    out$CI_low <- out[[1]] - tc * se
    out$CI_high <- out[[1]] + tc * se
    ci_method <- list(method = "normal")
    if (alternative == "less") {
      out$CI_low <- -Inf
    } else if (alternative == "greater") {
      out$CI_high <- Inf
    }
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