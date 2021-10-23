paired_d <- function(x, group, block, data = NULL,
                     mu = 0, ci = 0.95, alternative = "two.sided",
                     type = c("d", "z", "a", "r")) {
  type <- match.arg(type)
  data <- effectsize:::.kendalls_w_data(x, group, block, data, wide = FALSE)
  data$groups <- factor(data$groups)
  data$blocks <- factor(data$blocks)

  stopifnot(nlevels(data$groups) == 2L)

  orig_type <- type

  if (type %in% c("a", "z", "t")) {
    data <- aggregate(data$x, data[-1], mean)

    if (type == "a") type <- "d"
  }

  if (type == "d") {
    x <- split(data$x, data$groups)
    names(x) <- c("x", "y")
    d <- unname(diff(sapply(x, mean)))
    s <- do.call(sd_pooled, x)
  } else if (type == "z") {
    data <- data[order(data$groups),]
    data <- aggregate(data$x, data[2], function(.x) {
      if (length(.x) != 2L){
        NA
      } else {
        diff(.x)
      }
    })
    return(cohens_d(data$x, mu = mu, ci = ci, alternative = alternative))
    # d <- mean(data$x, na.rm = TRUE)
    # s <- sd(data$x, na.rm = TRUE)
  } else if (type == "r") {
    # insight::check_if_installed("lme4")
    # mod <- tryCatch({
    #   lme4::lmer(x ~ groups + (groups | blocks), data = data)
    # }, error = function(...) {
    #   lme4::lmer(x ~ groups + (1 | blocks), data = data)
    # })
    # d <- unname(lme4::fixef(mod)[2])
    # s <- sigma(mod)

    # Use ANOVA decomp instead
    mod <- aov(x ~ groups + Error(blocks), data = data) # blocks/groups? TryCatch?
    summ <- summary(mod)
    s <- sqrt(summ[["Error: Within"]][[1]][2,"Mean Sq"])
    browser()
  }


  out <- data.frame(d = (d - mu) / s)
  if (orig_type != "d") names(out) <- paste0("d_", orig_type)

  if (!is.null(ci)) {
    stopifnot(length(ci) == 1, ci < 1, ci > 0)

    # Add cis
    # USE emmeans::eff_size???? (w/ qdrg?)
    out$CI <- ci
    ci.level <- if (alternative == "two.sided") ci else 2 * ci - 1

    out$CI_low <- -Inf
    out$CI_high <- Inf

    ci_method <- list()
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



