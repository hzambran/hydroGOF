############################
# 'me': Mean Error         #
############################
#   15-Dic-2008; 06-Sep-09 #
############################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

me <-function(sim, obs, ...) UseMethod("me")

me.default <- function (sim, obs, na.rm=TRUE, ...){

     
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !") 
		      
  me <- mean( sim - obs, na.rm = na.rm)           
     
  return(me)
     
  } # 'me.default' end
  
  
me.matrix <- function (sim, obs, na.rm=TRUE, ...){

   me <- colMeans( sim - obs, na.rm= na.rm)  
   
   return(me)
     
  } # 'me' end
  
  
me.data.frame <- function (sim, obs, na.rm=TRUE,...){

   sim <- as.matrix(sim)
   obs <- as.matrix(obs)
	
   me.matrix(sim, obs, na.rm=na.rm, ...)
     
  } # 'me.data.frame' end
