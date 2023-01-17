# File VE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2011-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'VE': Volumetric Efficiency                                                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 26-Aug-2011                                                         #
# Updates: 26-Aug-2011                                                         #
#          16-Jan-2023                                                         #
################################################################################
# Reference: Criss, R. E. and Winston, W. E. (2008),                           #
#            Do Nash values have value? Discussion and alternate proposals.    #
#            Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072      #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Absolute Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

VE <-function(sim, obs, ...) UseMethod("VE")

VE.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")    
      
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
  
  denominator <- sum(obs, na.rm=na.rm)
  
  if (denominator != 0) {      
     ve <- 1 - ( sum( abs(sim-obs) ) / denominator )     
   } else {
       ve <- NA
       warning("'sum((obs)=0' => it is not possible to compute 'VE' !")  
     } 
   } else {
       ve <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
     
   return(ve)      
     
} # 'VE.default' end
  


################################################################################
# 'VE': Volumetric Efficiency                                                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 26-Aug-2011                                                         #
# Updates: 26-Aug-2011                                                         #
#          16-Jan-2023                                                         #
################################################################################  
VE.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  ve <- rep(NA, ncol(obs))       
          
  ve <- sapply(1:ncol(obs), function(i,x,y) { 
                 ve[i] <- VE.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                      epsilon.type=epsilon.type,  
                                      epsilon.value=epsilon.value)
               }, x=sim, y=obs )    
                     
  names(ve) <- colnames(obs)
                 
  return(ve)
     
} # 've.matrix' end
  

################################################################################
# 'VE': Volumetric Efficiency                                                  #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 26-Aug-2011                                                         #
# Updates: 26-Aug-2011                                                         #
#          16-Jan-2023                                                         #
################################################################################    
VE.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  VE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
            epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'VE.data.frame' end



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
VE.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA){
    
  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
  if (is.matrix(sim) | is.data.frame(sim)) {
     VE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'VE.zoo' end
