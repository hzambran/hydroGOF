# File ubRMSE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'ubRMSE': Unbiased Root Mean Square Error                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 18-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################

# The optimal value of ubRMSE is 0

# ubRMSE vaies int he followign range:
#
#            | sigma_sim - sigma_obs | <= ubRMSE <= sqrt( sigma_sim^2 + sigma_obs^2)
# where:
#
# -) sigma_sim: standard deviation of simulated values
# -) sigma_obs: standard deviation of observed values

# Ref:
# Entekhabi, D., Reichle, R. H., Koster, R. D., & Crow, W. T. (2010). 
# Performance metrics for soil moisture retrievals and application requirements. 
# Journal of Hydrometeorology, 11(3), 832-840. doi: 10.1175/2010JHM1223.1


# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Root Mean Square Error between 'sim' and 'obs', in the same units of 'sim' and 'obs'

ubRMSE <-function(sim, obs, ...) UseMethod("ubRMSE")

ubRMSE.default <- function(sim, obs, na.rm=TRUE, 
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !") 

  epsilon.type <- match.arg(epsilon.type)  

  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
   
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end     
	 
    RMSE <- sqrt( mean( (sim - obs)^2, na.rm = na.rm) )

    BIAS <- mean(sim - obs, na.rm = na.rm)

    ubRMSE <- sqrt(RMSE^2 - BIAS^2)
  
  } else {
       ubRMSE <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end
    
	            
  return(ubRMSE)
     
} # 'ubRMSE.default' end
  

ubRMSE.matrix <- function(sim, obs, na.rm=TRUE, 
                          fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA) {

   # Checking that 'sim' and 'obs' have the same dimensions
   if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   ubRMSE <- rep(NA, ncol(obs))       
          
   ubRMSE <- sapply(1:ncol(obs), function(i,x,y) { 
                  ubRMSE[i] <- rmse.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                 }, x=sim, y=obs )    
                     
   names(ubRMSE) <- colnames(obs)
            
   return(ubRMSE)
     
} # 'ubRMSE.matrix' end


ubRMSE.data.frame <- function(sim, obs, na.rm=TRUE, 
                              fun=NULL, ...,
                              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                              epsilon.value=NA) {

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)


  ubRMSE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)        
           
  return(ubRMSE)
     
} # 'ubRMSE.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 18-Jan-2024                                                         #
################################################################################
ubRMSE.zoo <- function(sim, obs, na.rm=TRUE, 
                       fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       ubRMSE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., epsilon.type=epsilon.type, 
                     epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., epsilon.type=epsilon.type, 
                      epsilon.value=epsilon.value)
     
  } # 'ubRMSE.zoo' end
