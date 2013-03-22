################################################
# 'rSD': Ratio of Standard Deviations          #
################################################
#   15-Dic-2008; 06-Sep-09    #
###############################
# SD(sim) / SD(obs)  
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Ratio of Standard Deviations  between 'sim' and 'obs', 
#           when multiplied by 100 its units is % 

rSD <-function(sim, obs, ...) UseMethod("rSD")

rSD.default <- function (sim, obs, na.rm=TRUE, ...){

     denominator <- sd(obs, na.rm = na.rm)
     
     if (denominator != 0) {
     
     rSD <- sd(sim, na.rm= na.rm) / sd(obs, na.rm= na.rm) 
     
     } else {
        rSD <- NA
        warning("'sd(obs)=0', it is not possible to compute 'rSD'")  
       } # ELSE end
     
     return(rSD)
     
  } # 'rSD.default' end
  
  
rSD.matrix <- function (sim, obs, na.rm=TRUE, ...){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") ) 
 
  rSD <- rep(NA, ncol(obs))       
          
  rSD <- sapply(1:ncol(obs), function(i,x,y) { 
                 rSD[i] <- rSD.default( x[,i], y[,i], na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(rSD) <- colnames(obs)
  
  return(rSD)
     
} # 'rSD.matrix' end


rSD.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rSD.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'rSD.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
rSD.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rSD.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'rSD.zoo' end
