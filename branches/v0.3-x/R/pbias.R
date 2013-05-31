##################################
# 'pbias': Percent Bias          #
##################################
#   03-Feb-2009;  06-Sep-09      #
##################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Percent Bias between 'sim' and 'obs', 
#           when multiplied by 100, its units is percentage
# Ref: Yapo P. O., Gupta H. V., Sorooshian S., 1996. 
#      Automatic calibration of conceptual rainfall-runoff models: 
#      sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48.

pbias <-function(sim, obs, ...) UseMethod("pbias")

pbias.default <- function (sim, obs, na.rm=TRUE, ...){

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements 
	 # that are present both in 'x' and 'y' (NON- NA values)
     obs <- obs[vi]
     sim <- sim[vi]
     
     # lenght of the data sets that will be ocnsidered for the ocmputations
     n <- length(obs)
      
     denominator <- sum( obs )
     
     if (denominator != 0) {
      
       pbias <- 100 * ( sum( sim - obs ) / denominator )
     
     } else {
        pbias <- NA
        warning("'sum((obs)=0', it is not possible to compute 'pbias'")  
       } # ELSE end
     
     return( round(pbias, 1) )
     
} # 'pbias.default' end
  
  
pbias.matrix <- function (sim, obs, na.rm=TRUE, ...){

   # Checking that 'sim' and 'obs' have the same dimensions
   if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   pbias <- rep(NA, ncol(obs))       
          
   pbias <- sapply(1:ncol(obs), function(i,x,y) { 
                 pbias[i] <- pbias.default( x[,i], y[,i], na.rm=na.rm, ... )
            }, x=sim, y=obs )        
                    
   return(pbias)
     
  } # 'pbias.matrix' end
  

pbias.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  pbias.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'pbias.data.frame' end  


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
pbias.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       pbias.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'pbias.zoo' end
