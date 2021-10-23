library(effectsize)

dat <- read.table("http://pcl.missouri.edu/exp/effectSizePuzzler.txt", header=TRUE)

paired_d(rt ~ cond | id, data = dat, type = "z") # 1.353713 DONE

paired_d(rt ~ cond | id, data = dat, type = "d") # 0.2497971
paired_d(rt ~ cond | id, data = dat, type = "a") # 0.8357347
# paired_d(rt ~ cond | id, data = dat, type = "t") # withOUT sqrt(2) bias, is the same as d_z
paired_d(rt ~ cond | id, data = dat, type = "r") # 0.259

head(dat)

# https://doi.org/10.3389/fpsyg.2013.00863
dat_m <- tapply(dat$rt, list(dat$id, dat$cond), mean)
rr <- cor(dat_m)[2]
MM <- apply(dat_m, 2, mean)
SS <- apply(dat_m, 2, sd)

# Cohen's d RM 0.267
sqrt(2 * (1-rr)) * diff(MM) /
  sqrt(sum(SS ^ 2) + 2 * rr * prod(SS))

#Cohen's d AV 0.209
diff(MM) / sqrt(sum(SS) / 2)


# For bootstarp?
data <- data.frame(
  x = dat$rt,
  groups = dat$cond |> factor(),
  blocks = dat$id |> factor()
)

sample_rm_data <- function(data) {
  tmp_data <- split(data, data$blocks)
  tmp_data <- sample(tmp_data, replace = TRUE)
  tmp_data <- mapply(tmp_data, seq_along(tmp_data), FUN = function(d, id) {
    d$blocks <- id
    split(d, d$groups) <- lapply(split(d, d$groups), function(.d) .d[sample(nrow(.d), replace = TRUE), ])
    d
  }, SIMPLIFY = FALSE)
  do.call("rbind", tmp_data)
}


sample_rm_data(data) |> head()


