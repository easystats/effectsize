library(dplyr)
library(effectsize)

source("WIP/paired_d2.R")

set.seed(1)
dat <- expand.grid(t = 1:100,
                   id = letters[1:10],
                   cond = LETTERS[1:2]) |>
  as.data.frame() |>
  select(-t) |>
  mutate(
    across(.fns = factor),
    across(.fns = `contrasts<-`, value = contr.sum),
    y = model.matrix(~ id * cond) %*%
      c(0, c(scale(seq(0, 1, length = 9))), 0.9, c(scale(seq(0, 1, length = 9)))) +
      rnorm(2000, sd = 0.85)
  )

# aov(y ~ cond + Error(id/cond), data = dat) |>
#   parameters::model_parameters() |>
#   as.data.frame()

cohens_d(y ~ cond, data = d)
paired_d(y ~ cond | id, data = d, type = "d") # 0.2497971
paired_d(y ~ cond | id, data = d, type = "r") # 0.2587388


paired_d(y ~ cond | id, data = d, type = "a") # 0.8357347
paired_d(y ~ cond | id, data = d, type = "z") # 1.353713
paired_d(y ~ cond | id, data = d, type = "rm")
paired_d(y ~ cond | id, data = d, type = "av")

B <- 200
d <- numeric(B)

for (b in seq_along(d)) {
  d[b] <- paired_d(y ~ cond | id,
                   data = .sample_within(dat),
                   ci = NULL,
                   type = "r")[[1]]
}

bayestestR::describe_posterior(d, test = NULL)

paired_d(y ~ cond | id, data = dat,
         type = "r")

