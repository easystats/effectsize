library(effectsize)

source("WIP/paired_d2.R")

dat <- read.table("http://pcl.missouri.edu/exp/effectSizePuzzler.txt", header=TRUE)

# From http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/
paired_d(rt ~ cond | id, data = dat, type = "d") # 0.2497971
paired_d(rt ~ cond | id, data = dat, type = "a") # 0.8357347
paired_d(rt ~ cond | id, data = dat, type = "z") # 1.353713
paired_d(rt ~ cond | id, data = dat, type = "t") # 1.914439
paired_d(rt ~ cond | id, data = dat, type = "r") # 0.259

# More types:
# https://doi.org/10.3389/fpsyg.2013.00863
# https://journals.sagepub.com/doi/pdf/10.1177/0013164403256358 (CIs)
paired_d(rt ~ cond | id, data = dat, type = "rm")
paired_d(rt ~ cond | id, data = dat, type = "av")


# Test ----
paired_d(rt ~ cond | id, data = dat, type = "d", mu = 0.05)
paired_d(rt ~ cond | id, data = dat, type = "d", alternative = "l")
