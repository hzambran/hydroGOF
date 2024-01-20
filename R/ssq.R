# File ssq.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'ssq': Sum of Squared Residuals                                              #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 04-Mar-2009                                                         #
# Updates: 06-Sep-2009                                                         #
#          20-Jan-2024                                                         #   
################################################################################

ssq <-function(sim, obs, ...) UseMethod("ssq")
 
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Sum of the Squared Residuals between 'sim' and 'obs', 
#           with squared measurement units of 'sim' and 'obs'
ssq.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    

  # Checking 'epsilon.type'
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
	 
	  ssq <- sum( (sim - obs)^2, na.rm= na.rm)   

  } else {
      ssq <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end
     
  return(ssq)
     
} # 'ssq' END
  

ssq.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  ssq <- rep(NA, ncol(obs))       
          
  ssq <- sapply(1:ncol(obs), function(i,x,y) { 
                 ssq[i] <- ssq.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                        epsilon.type=epsilon.type,  
                                        epsilon.value=epsilon.value)
                 }, x=sim, y=obs )    
                     
   names(ssq) <- colnames(obs)     
           
  return(ssq)
     
} # 'ssq.matrix' END
  
  
ssq.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA){

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  sim <- as.matrix(sim)
	obs <- as.matrix(obs)
	
	ssq.matrix(sim, obs, na.rm=na.rm, ...)        
     
} # 'ssq.data.frame' END

ssq.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       ssq.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'ssq.zoo' end
