# File rmse.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2011-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'rmse': Root Mean Square Error                                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 15-Dic-2008                                                         #
# Updates: 06-Sep-2009                                                         #
#          18-Jan-2024                                                         #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Root Mean Square Error between 'sim' and 'obs', in the same units of 'sim' and 'obs'

rmse <-function(sim, obs, ...) UseMethod("rmse")

rmse.default <- function(sim, obs, na.rm=TRUE, 
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) {

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type) 

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !")   

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
	 
    rmse <- sqrt( mean( (sim - obs)^2, na.rm = na.rm) )
  
  } else {
       rmse <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end
           
  return(rmse)
     
} # 'rmse.default' end
  

rmse.matrix <- function(sim, obs, na.rm=TRUE, 
                        fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA) {

   # Checking 'epsilon.type'
   epsilon.type <- match.arg(epsilon.type) 

   # Checking that 'sim' and 'obs' have the same dimensions
   if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   rmse <- rep(NA, ncol(obs))       
          
   rmse <- sapply(1:ncol(obs), function(i,x,y) { 
                  rmse[i] <- rmse.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                       epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                 }, x=sim, y=obs )    
                     
   names(rmse) <- colnames(obs)
            
   return(rmse)
     
} # 'rmse.matrix' end


rmse.data.frame <- function(sim, obs, na.rm=TRUE, 
                            fun=NULL, ...,
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA) {

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  rmse.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)        
           
  return(rmse)
     
} # 'rmse.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 18-Jan-2024                                                         #
################################################################################
rmse.zoo <- function(sim, obs, na.rm=TRUE, 
                     fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rmse.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., epsilon.type=epsilon.type, 
                   epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., epsilon.type=epsilon.type, 
                      epsilon.value=epsilon.value)
     
  } # 'rmse.zoo' end
