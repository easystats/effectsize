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

sim_2ord()
res= replicate(10, sim_2ord(), simplify = FALSE)
dplyr::bind_rows(res)

