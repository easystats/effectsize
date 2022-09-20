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
  res_tab$odds_par = odds
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

  return(full_tab)

}