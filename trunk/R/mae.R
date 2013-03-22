##############################
# 'mae': Mean Absolute Error #
##############################
#   15-Dic-2008; 06-Sep-09   #
##############################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Absolute Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

mae <-function(sim, obs, ...) UseMethod("mae")

mae.default <- function (sim, obs, na.rm=TRUE, ...){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !")        
      
  mae <- mean( abs(sim - obs), na.rm = TRUE) 
               
  return(mae)
     
} # 'mae.default' end
  
  
mae.matrix <- function (sim, obs, na.rm=TRUE, ...){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  mae <- colMeans( abs(sim - obs), na.rm= na.rm)  
                 
  return(mae)
     
  } # 'mae.matrix' end
  
  
mae.data.frame <- function (sim, obs, na.rm=TRUE,...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  mae.matrix(sim, obs, na.rm=na.rm, ...)  
     
} # 'mae.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
mae.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       mae.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'mae.zoo' end
