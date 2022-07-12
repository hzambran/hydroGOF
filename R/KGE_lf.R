References:

1) Garcia, F., Folton, N. and Oudin, L. (2017). Which objective function to 
   calibrate rainfall-runoff models for low-flow index simulations?. 
   Hydrological sciences journal, 62(7), pp.1149-1166. doi:10.1080/02626667.2017.1308511
2) Pushpalatha, R., Perrin, C., Le Moine, N. and Andréassian, V. (2012). 
   A review of efficiency criteria suitable for evaluating low-flow simulations. 
   Journal of Hydrology, 420, pp.171-182. doi: 10.1016/j.jhydrol.2011.11.055

# epsilon: By defualt it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
KGElf <- function(sim, obs, method=c("2009", "2012"), epsilon=1/100) {
  
  method <- match.arg(method)

  # 1) KGE(Q): traditioanl KGE 2012
  kge    <- KGE(sim=sim, obs=obs, method=method)
  
  # 2) KGE(1/Q): KGE low flows
  sim.lf <- 1 / ( sim + epsilon*mean(obs, na.rm = TRUE) )
  obs.lf <- 1 / ( obs + epsilon*mean(obs, na.rm = TRUE) )
  kge.lf <- KGE(sim=sim.lf, obs=obs.lf, method=method)
  
  # 3) [KGE(Q) + KGE(1/Q)] / 2 : ggregated goodness-of-fit value
  out <- (kge + kge.lf) / 2
    
  return(out)
  
}
