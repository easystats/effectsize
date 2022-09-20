ci_binom = function(centx,n,conf_level=.95){
  p <- centx
  x <- p*n
  alpha <- 1 - conf_level
  alpha2 <- 0.5 * alpha
  z <- qnorm(1 - alpha2)
  z2 <- z * z
  # Wilson, E. B. (1927). Probable Inference, the Law of Succession, and Statistical Inference. Journal of the American Statistical Association, 22(158), 209–212. https://doi.org/10.1080/01621459.1927.10502953
  p1 <- p + 0.5 * z2/n
  p2 <- z * sqrt((p * (1 - p) + 0.25 * z2/n)/n)
  p3 <- 1 + z2/n
  lcl <- (p1 - p2)/p3
  ucl <- (p1 + p2)/p3
  res = c(lcl,ucl)
  return(res)
}


sim_2ord = function(baseprobs = c(.2,.2, .3, .2, .1),
                       n = 30,
                       ci = .95,
                       odds = 1.5){
  # odds ratio to log odds scaling
  treat_effect = log(odds)
  # Assign treatment at a 1:1 ratio
  defA <- defData(varname = "treat",
                  formula = "1;1",
                  dist = "trtAssign")
  # Add Treatment effect
  defA <-  defDataAdd(defA,
                      varname = "z",
                      formula = paste0(treat_effect,"*treat"),
                      #variance = 1,
                      dist = "nonrandom")
  dt = genData(n, defA)
  dX <- genOrdCat(dt, adjVar = "z", baseprobs, catVar = "r")
  dX$r = as.ordered(dX$r)
  #set.seed(130)

  res = wmw_odds(x = subset(dX, treat == 1)$r,
                 y = subset(dX, treat == 0)$r,
                 ci = ci)

  return(janitor::clean_names(as.data.frame(res)))
  #return(res)
}



sim_rep_2ord = function(baseprobs = c(.2,.2, .3, .2, .1),
                        n = 30,
                        ci = .95,
                        odds = 1.5,
                        nsim = 200){
  res= replicate(nsim, sim_2ord(baseprobs = baseprobs,
                                n = n,
                                ci = ci,
                                odds = odds), simplify = FALSE)
  res_tab = dplyr::bind_rows(res)
  res_tab$n = n
  res_tab$theta = odds
  return(res_tab)
}


sim_loop_2ord = function(baseprobs = c(.2,.2, .3, .2, .1),
                         n = seq(20,50,10),
                         ci = .95,
                         odds = seq(0.25,1.75,.25),
                         nsim = 200){

  full_tab = data.frame()

  for(ns in n){
    for(odds_i in odds){
      part_tab = sim_rep_2ord(baseprobs = baseprobs,
                              n = ns,
                              ci = ci,
                              odds = odds_i,
                              nsim = nsim)
      full_tab = bind_rows(full_tab, part_tab)
    }
  }
  full_tab = full_tab |> as_tibble() |>
    mutate(coverage = ifelse(theta >= ci_low & theta <= ci_high, 1,0))
  return(full_tab)

}

cov_2ord = function(tab, nsim = 200){
  sum_tab = tab |>
    group_by(theta, n) |>
    summarize(coverage = mean(coverage, na.rm = TRUE),
              .groups = 'drop')

  sum_tab$lci =  mapply(function(x, y)
    ci_binom(x, y)[1],
    sum_tab$coverage,
    nsim)

  sum_tab$uci =  mapply(function(x, y)
    ci_binom(x, y)[2],
    sum_tab$coverage,
    nsim)
  return(sum_tab)
}
