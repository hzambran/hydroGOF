##################################
# 'mse': Mean Squared Error      #
##################################
#   03-Feb-2009;  06-Sep-09      #
##################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Squared Error between 'sim' and 'obs'
# Ref: Yapo P. O., Gupta H. V., Sorooshian S., 1996. 
#      Automatic calibration of conceptual rainfall-runoff models: 
#      sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48.

mse <-function(sim, obs, ...) UseMethod("mse")

mse.default <- function (sim, obs, na.rm=TRUE, ...){

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements 
     # that are present both in 'x' and 'y' (NON- NA values)
     obs <- obs[vi]
     sim <- sim[vi]
      
     mse <- mean( (sim - obs)^2, na.rm = na.rm)
     
     return( mse )
     
} # 'mse.default' end
  
  
mse.matrix <- function (sim, obs, na.rm=TRUE, ...){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   mse <- colMeans( (sim - obs)^2, na.rm= na.rm)        
                    
   return(mse)
     
  } # 'mse.matrix' end
  

mse.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  mse.matrix(sim, obs)
     
} # 'mse.data.frame' end  


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
mse.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       mse.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'mse.zoo' end
