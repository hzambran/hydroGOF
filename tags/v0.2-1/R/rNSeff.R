##################################################
# 'rNSeff': Relative Nash-sutcliffe Efficiency   #
##################################################
#    April 2010                                  #
##################################################
# Ref:
# Krause, P., Boyle, D. P., and Bäse, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005 
# Legates and McCabe, 1999. Evaluating the use of "goodness-of-fit" measures 
#                           in hydrologic and hydroclimatic model validation. 
#                           Water Resources Research. v35 i1. 233-241.

# Nash-Sutcliffe efficiency not "inflated" by squared values
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

rNSeff <-function(sim, obs, ...) UseMethod("rNSeff")

rNSeff.default <- function (sim, obs, na.rm=TRUE, ...){ 

	 if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      
   
     vi <- valindex(sim, obs)
	 
	 obs <- obs[vi]
	 sim <- sim[vi]
	 
	 denominator <- sum( ( ( obs - mean(obs) ) / mean(obs) )^2 )
	 
	 if (denominator != 0) {
	  
	 rNSeff <- 1 - ( sum( ( (obs - sim) / obs )^2 ) / denominator )
	 
	 } else stop("'sum( ( ( obs - mean(obs) ) / mean(obs) )^2 ) = 0', it is not possible to compute 'rNSeff'")  
	 
	 return(rNSeff)
     
} # 'rNSeff.default' end


rNSeff.matrix <- function (sim, obs, na.rm=TRUE, ...){ 

  rNSeff <- rep(NA, ncol(obs))       
          
  rNSeff <- sapply(1:ncol(obs), function(i,x,y) { 
                 rNSeff[i] <- rNSeff.default( x[,i], y[,i], na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(rNSeff) <- colnames(obs)
  return(rNSeff)
     
} # 'rNSeff.matrix' end


rNSeff.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rNSeff.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'rNSeff.data.frame' end
