##################################################
# 'rNSE': Relative Nash-sutcliffe Efficiency   #
##################################################
# Started: April 2010                            #
# Updates: 01-Jun-2011                           #
##################################################
# Ref:
# Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency 
#                           criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005 
# Legates and McCabe, 1999. Evaluating the use of "goodness-of-fit" measures 
#                           in hydrologic and hydroclimatic model validation. 
#                           Water Resources Research. v35 i1. 233-241.

# Nash-Sutcliffe efficiency not "inflated" by squared values
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

rNSE <-function(sim, obs, ...) UseMethod("rNSE")

rNSE.default <- function (sim, obs, na.rm=TRUE, ...){ 

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      
   
   vi <- valindex(sim, obs)
	 
   obs <- obs[vi]
   sim <- sim[vi]
	 
   # Testing for zero values in obs, which leads to -Inf as result
   zero.index <- which(obs==0)
   if (length(zero.index > 0) ) {
       warning("'rNSE' can not be computed: some elements in 'obs' are zero !", call.=FALSE)
   } # IF end
	 
   denominator <- sum( ( ( obs - mean(obs) ) / mean(obs) )^2 )
	 
   if (denominator != 0) {
	  
   rNSE <- 1 - ( sum( ( (obs - sim) / obs )^2 ) / denominator )
	 
   } else {
      rNSE <- NA
      warning("'sum( ( ( obs - mean(obs) ) / mean(obs) )^2 ) = 0', it is not possible to compute 'rNSE'")  
     } # ELSE end
	 
   return(rNSE)
     
} # 'rNSE.default' end


rNSE.matrix <- function (sim, obs, na.rm=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  rNSE <- rep(NA, ncol(obs))       
          
  rNSE <- sapply(1:ncol(obs), function(i,x,y) { 
                 rNSE[i] <- rNSE.default( x[,i], y[,i], na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(rNSE) <- colnames(obs)
  return(rNSE)
     
} # 'rNSE.matrix' end


rNSE.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rNSE.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'rNSE.data.frame' end


rNSeff <-function(sim, obs, ...) UseMethod("rNSE")



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
rNSE.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rNSE.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'rNSE.zoo' end
