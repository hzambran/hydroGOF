##################################
# 'rmse': Root Mean Square Error #
##################################
#   15-Dic-2008; 06-Sep-09       #
##################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Root Mean Square Error between 'sim' and 'obs', in the same units of 'sim' and 'obs'

rmse <-function(sim, obs, ...) UseMethod("rmse")

rmse.default <- function (sim, obs, na.rm=TRUE, ...) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !")     
	 
  rmse <- sqrt( mean( (sim - obs)^2, na.rm = na.rm) )
           
  return(rmse)
     
} # 'rmse.default' end
  

rmse.matrix <- function (sim, obs, na.rm=TRUE, ...) {

   # Checking that 'sim' and 'obs' have the same dimensions
   if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   rmse <- sqrt( colMeans( (sim - obs)^2, na.rm = na.rm, ...) )          
           
   return(rmse)
     
} # 'rmse.matrix' end


rmse.data.frame <- function (sim, obs, na.rm=TRUE, ...) {

   rmse <- sqrt( colMeans( (sim - obs)^2, na.rm = na.rm, ...) )          
           
   return(rmse)
     
} # 'rmse.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
rmse.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rmse.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'rmse.zoo' end
