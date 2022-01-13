library(effectsize)

dat <- read.table("http://pcl.missouri.edu/exp/effectSizePuzzler.txt", header=TRUE)

paired_d(rt ~ cond | id, data = dat, type = "z") # 1.353713
paired_d(rt ~ cond | id, data = dat, type = "d") # 0.2497971
paired_d(rt ~ cond | id, data = dat, type = "a") # 0.8357347
# paired_d(rt ~ cond | id, data = dat, type = "t") # withOUT sqrt(2) bias, is the same as d_z
paired_d(rt ~ cond | id, data = dat, type = "r") # 0.259


# Test
paired_d(rt ~ cond | id, data = dat, type = "d", mu = 0.05)
paired_d(rt ~ cond | id, data = dat, type = "d", alternative = "l")


# More types ----
data(stroop, package = "afex")
stroop <- subset(stroop, study == 1 & condition == "control" & acc == 1)
stroop <- subset(stroop, pno %in% levels(pno)[1:10])

paired_d(rt ~ congruency | pno, data = stroop, type = "z", ci = NULL)
paired_d(rt ~ congruency | pno, data = stroop, type = "d", ci = NULL)
paired_d(rt ~ congruency | pno, data = stroop, type = "a", ci = NULL)
paired_d(rt ~ congruency | pno, data = stroop, type = "r", ci = NULL)

data_agg <- aggregate(stroop$rt, stroop[, c("pno", "congruency")], mean)
d <- diff(tapply(data_agg$x, data_agg$congruency, mean))
ss <- tapply(data_agg$x, data_agg$congruency, sd)

# type av (close to type a)
s <- mean(ss)
d / s

# type rm (close to type r?) NOPE
data_agg <- data_agg[order(data_agg$pno),]
data_agg <- split(data_agg, data_agg$congruency)
r <- cor(sapply(data_agg, "[[", "x"))[1,2]
s <- sqrt(sum(ss^2) - 2 * r * prod(ss))
(d / s) * sqrt(2 * (1-r))

effsize::cohen.d(data_agg[[1]]$x, data_agg[[2]]$x, paired = TRUE)
