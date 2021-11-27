paired_d <- function(x, group, block, data = NULL,
                     mu = 0, ci = 0.95, alternative = "two.sided",
                     iterations = 200,
                     type = c("d", "a", "r", "z")) {
  type <- match.arg(type)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"))

  data <- effectsize:::.kendalls_w_data(x, group, block, data, wide = FALSE)
  data$groups <- factor(data$groups)
  data$blocks <- factor(data$blocks)

  stopifnot(nlevels(data$groups) == 2L)

  orig_type <- type

  if (type %in% c("a", "z", "t")) {
    data <- aggregate(data$x, data[-1], mean)

    if (type == "a") type <- "d"
  }

  if (type == "z") {
    data <- data[order(data$groups),]
    data <- aggregate(data$x, data[2], function(.x) {
      if (length(.x) != 2L){
        NA
      } else {
        diff(.x)
      }
    })
    return(cohens_d(data$x, mu = mu, ci = ci, alternative = alternative))
  }

  insight::check_if_installed("lme4")
  mod <- tryCatch({
    lme4::lmer(x ~ groups + (groups | blocks), data = data)
  }, error = function(...) {
    lme4::lmer(x ~ groups + (1 | blocks), data = data)
  })

  # # Use ANOVA decomp instead?
  # mod <- aov(x ~ groups + Error(blocks), data = data) # blocks/groups? TryCatch?
  # summ <- summary(mod)
  # d <- coef(mod[["Within"]])
  # s <- sqrt(summ[["Error: Within"]][[1]][2,"Mean Sq"])

  out <- data.frame(d = .d_from_lmer(mod, type = type, mu = mu))
  if (orig_type != "d") names(out) <- paste0("d_", orig_type)

  if (!is.null(ci) &&
      insight::check_if_installed("boot", stop = FALSE, reason = "for CI estimation")) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)


    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    sims <- lme4::bootMer(
      mod, nsim = iterations, use.u = TRUE,
      FUN = function(.m) .d_from_lmer(.m, type = type, mu = mu)
    )

    CIs <- boot::boot.ci(sims, type = "perc", conf = ci.level)$percent[,4:5]

    out$CI_low <- CIs[1]
    out$CI_high <- CIs[2]

    ci_method <- list(method = "percentile bootstrap", iterations = iterations)
    if (alternative == "less") {
      out$CI_low <- -Inf
    } else if (alternative == "greater") {
      out$CI_high <- Inf
    }
  } else {
    ci_method <- alternative <- NULL
  }

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired_type") <- orig_type
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}


.d_from_lmer <- function(mod, type, mu) {
  d <- unname(lme4::fixef(mod)[2])

  if (type == "d") {
    s <- sqrt(insight::get_variance_intercept(mod) +
                insight::get_variance_residual(mod, verbose = FALSE))
  } else if (type == "r") {
    s <- sigma(mod)
  }

  unname((d - mu) / s)
}
