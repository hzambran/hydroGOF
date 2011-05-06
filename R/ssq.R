###################################
# 'ssq': Sum of Squared Residuals #
###################################
#   04-Mar-2009; 06-Sep-09        #
###################################
# I don't think it is very useful, but I just added because it is required 
# for doing some SWAt comparisons

ssq <-function(sim, obs, ...) UseMethod("ssq")
 
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Sum of the Squared Residuals between 'sim' and 'obs', 
#           with squared measurement units of 'sim' and 'obs'
ssq.default <- function (sim, obs, na.rm=TRUE, ...){

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
         ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
     if ( length(obs) != length(sim) ) 
	    stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !") 
	 
	 ssq <- sum( (sim - obs)^2, na.rm= na.rm)   
     
     return(ssq)
     
  } # 'ssq' END
  

ssq.matrix <- function (sim, obs, na.rm=TRUE, ...){

    # Checking that 'sim' and 'obs' have the same dimensions
    if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

    ssq <- colSums( (sim - obs)^2, na.rm = na.rm)          
           
    return(ssq)
     
  } # 'ssq.matrix' END
  
  
ssq.data.frame <- function (sim, obs, na.rm=TRUE, ...){

    sim <- as.matrix(sim)
	obs <- as.matrix(obs)
	
	ssq.matrix(sim, obs, na.rm=na.rm, ...)        
     
  } # 'ssq.data.frame' END
