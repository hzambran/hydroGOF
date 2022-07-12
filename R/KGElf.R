# File KGElf.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
# Copyright 2017-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'KGElf': Kling-Gupta Efficiency with focus on low flows                      #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2017                                                                #
# Updates: 07-Jul-2022 ; 11-Jul-2022                                           #
################################################################################
# The optimal value of KGElf is 1

# Ref1:
# Garcia, F., Folton, N. and Oudin, L. (2017). 
# Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. 
# Hydrological sciences journal, 62(7), pp.1149-1166. doi:10.1080/02626667.2017.1308511

# Ref2:
# Pushpalatha, R., Perrin, C., Le Moine, N. and Andréassian, V. (2012). 
# A review of efficiency criteria suitable for evaluating low-flow simulations. 
# Journal of Hydrology, 420, pp.171-182. doi: 10.1016/j.jhydrol.2011.11.055

KGElf <- function(sim, obs, ...) UseMethod("KGElf")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
KGElf.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                          method=c("2009", "2012"), out.type=c("single", "full"), 
                          epsilon=1/100, ...) {
  
  # 1) KGE(Q): KGE (2009 or 2012)
  kge <- KGE(sim=sim, obs=obs, method=method)
  
  # 2) KGE(1/Q): KGE based on Garcia et al. (2017), with epsilon based on Pushpalatha et al. (2012)
  sim.lf <- 1 / ( sim + epsilon*mean(obs, na.rm = TRUE) )
  obs.lf <- 1 / ( obs + epsilon*mean(obs, na.rm = TRUE) )
  kge.lf <- KGE(sim=sim.lf, obs=obs.lf, method=method)
  
  # 3) [KGE(Q) + KGE(1/Q)] / 2 : ggregated goodness-of-fit value
  out <- (kge + kge.lf) / 2
    
  return(out)
  
}
