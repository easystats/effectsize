library(effectsize)

source("WIP/paired_d2.R")

#' dat <- read.table("http://pcl.missouri.edu/exp/effectSizePuzzler.txt", header=TRUE)
#'
#' # From http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
#' # More types:
#' # https://doi.org/10.3389/fpsyg.2013.00863
#' # https://journals.sagepub.com/doi/pdf/10.1177/0013164403256358 (CIs)
#'
#' # Are CIs correct?
#' # Need to simulate data with a big subject:condition interaction.
#'
#' #' Single measurement:
#' #' d - classic definition
#' #' r - standardize by the residual sd (sigma)
#' paired_d(rt ~ cond | id, data = dat, type = "d") # 0.2497971
#' paired_d(rt ~ cond | id, data = dat, type = "r") # 0.2587388
#'
#' #' Aggregate:
#' #' a - aggregate
#' #' z - standardized scores
#' #' t - (incorrect) conversion from t statistic (d_z * sqrt(2))
#' #' rm - d, but accounts for correlation between repeated measures (not a good name)
#' #' av - d, but just use average sd (isn't this type a?)
#' cohens_d_rm(rt ~ cond | id, data = dat, type = "a") # 0.8357347
#' cohens_d_rm(rt ~ cond | id, data = dat, type = "z") # 1.353713
#' cohens_d_rm(rt ~ cond | id, data = dat, type = "t") # 1.914439
#' cohens_d_rm(rt ~ cond | id, data = dat, type = "rm")
#' cohens_d_rm(rt ~ cond | id, data = dat, type = "av")


# Test on other data:
library(emmeans)
data(stroop, package = "afex")
stroop <- subset(stroop, study == "1" & condition == "control")

m <- lme4::lmer(rt ~ congruency + (congruency | pno), data = stroop)

cohens_d(rt ~ congruency, data = stroop)
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "d")
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "r")

emmeans(m, ~ congruency) |>
  eff_size(sigma(m), df.residual(m))

emmeans(m, ~ congruency) |>
  eff_size(sd(predict(m, re.form = NA) - na.omit(stroop$rt)), df.residual(m))

cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "a")
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "z")
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "t")
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "rm")
cohens_d_rm(rt ~ congruency | pno, data = stroop, type = "av")


set.seed(1)
dat <- expand.grid(
  t = 1:100,
  id = letters[1:10],
  cond = LETTERS[1:2]
) |>
  as.data.frame() |>
  dplyr::select(-t) |>
  dplyr::mutate(
    dplyr::across(.fns = factor),
    dplyr::across(.fns = `contrasts<-`, value = contr.sum),
    rt = model.matrix(~ id * cond) %*%
      c(0, c(scale(seq(0, 1, length = 9))), 0.9, c(scale(seq(0, 1, length = 9)))) +
      rnorm(2000, sd = 0.85)
  )


cohens_d(rt ~ cond, data = dat)
paired_d(rt ~ cond | id, data = dat, type = "d")
paired_d(rt ~ cond | id, data = dat, type = "r")

paired_d(rt ~ cond | id, data = dat, type = "a")
paired_d(rt ~ cond | id, data = dat, type = "z")
paired_d(rt ~ cond | id, data = dat, type = "t")
paired_d(rt ~ cond | id, data = dat, type = "rm")
paired_d(rt ~ cond | id, data = dat, type = "av")
