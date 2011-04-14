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

  mae <- colMeans( abs(sim - obs), na.rm= na.rm)  
                 
  return(mae)
     
  } # 'mae.matrix' end
  
  
mae.data.frame <- function (sim, obs, na.rm=TRUE,...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  mae.matrix(sim, obs, na.rm=na.rm, ...)  
     
} # 'mae.data.frame' end
