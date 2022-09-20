library(simstudy)
library(tidyverse)
library(effectsize)
library(tictoc)
nsim = 2000
source(here::here("papers","wmw_odds", "R_simulation_funcs.R"))



# 5-item Likert
# baseprobs = c(.2,.2,.3,.2,.1)

tic()
sim_2ord_5l_v1 = sim_loop_2ord(nsim = 2000)
toc()
sum_2ord_5l_v1 = cov_2ord(sim_2ord_5l_v1, nsim = nsim)

res_2ord_5l_v1 = list(baseprobs = baseprobs,
                      sim_2ord_5l_v1 = sim_2ord_5l_v1,
                      sum_2ord_5l_v1 = sum_2ord_5l_v1)

saveRDS(res_2ord_5l_v1, here::here("papers","wmw_odds","res_2ord_5l_v1.rds"))

ggplot(sum_2ord_5l_v1,
       aes(x=n,ymin=lci,ymax=uci,y=coverage))+
  facet_wrap(~theta) +
  geom_ribbon(alpha = .2, color = "green") +
  geom_line() +
  geom_hline(yintercept = .95) +
  theme_bw() +
  scale_y_continuous(limits = c(0.6,1),
                     breaks = seq(0.6,1,.05))
