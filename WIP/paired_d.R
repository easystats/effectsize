paired_d <- function(x, group, block, data = NULL,
                     mu = 0, ci = 0.95, alternative = "two.sided",
                     type = c("d", "z", "t", "a", "r")) {
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
  } else if (type %in% c("z", "t")) {
    data <- data[order(data$groups),]
    data <- aggregate(data$x, data[2], function(.x) {
      if (length(.x) != 2L){
        NA
      } else {
        diff(.x)
      }
    })
    if (type == "z") {
      d <- mean(data$x, na.rm = TRUE)
      s <- sd(data$x, na.rm = TRUE)
    } else {
      tt <- t.test(data$x)
      d <- unname(tt$statistic / sqrt(tt$parameter + 1))
      s <- 1
    }
  } else if (type == "r") {
    insight::check_if_installed("lme4")

    mod <- tryCatch({
      lme4::lmer(x ~ groups + (groups | blocks), data = data)
    }, error = function(...) {
      lme4::lmer(x ~ groups + (1 | blocks), data = data)
    })
    d <- unname(lme4::fixef(mod)[2])
    s <- sigma(mod)
  }

  ci_method <- ci <- alternative <- NULL

  out <- data.frame(d = (d - mu) / s)
  if (orig_type != "d") names(out) <- paste0("d_", orig_type)

  class(out) <- c("effectsize_difference", "effectsize_table", "see_effectsize_table", class(out))
  attr(out, "paired_type") <- orig_type
  attr(out, "mu") <- mu
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- ci_method
  attr(out, "approximate") <- FALSE
  attr(out, "alternative") <- alternative
  return(out)
}

library(effectsize)

dat <- read.table("http://pcl.missouri.edu/exp/effectSizePuzzler.txt", header=TRUE)

paired_d(rt ~ cond | id, data = dat, type = "d") # 0.2497971
paired_d(rt ~ cond | id, data = dat, type = "a") # 0.8357347
paired_d(rt ~ cond | id, data = dat, type = "z") # 1.353713
paired_d(rt ~ cond | id, data = dat, type = "t") # withOUT sqrt(2) bias, is the same as d_z
paired_d(rt ~ cond | id, data = dat, type = "r") # 0.259


