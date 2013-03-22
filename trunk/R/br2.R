#########################################################################
# 'br2': Weighted R2                                                    #
# Coef. of  determination multiplied by the coef. of the regression line#
#########################################################################
#   27-Oct-2009                                                         #
#########################################################################

# This index allows accounting for the discrepancy in the magnitude of two signals
# under or overpredictions, (depicted by 'b') as well as their dynamics (depicted by R2).

# Krause, P., Boyle, D. P., and Bäse, F.: Comparison of different efficiency 
# criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005

br2 <-function(sim, obs, ...) UseMethod("br2")
 
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': weighted R2 between 'sim' and 'obs'

br2.default <- function (sim, obs, na.rm=TRUE, ...){

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements 
     # that are present both in 'x' and 'y' (NON- NA values)
     obs <- obs[vi]
     sim <- sim[vi]
     
     # the next two lines are required for avoiding an strange behaviour 
     # of the difference function when sim and obs are time series.
     if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
     if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)
     
     # Computing the linear regression between 'sim' and 'obs', 
     # forcing a zero intercept.
     x.lm <- lm(sim ~ obs - 1)
     
     # Getting the slope of the previous linear regression
     b <- as.numeric( coefficients(x.lm)["obs"]   )
     
     # computing the r2
     r2 <- (.rPearson(sim, obs))^2
     
     br2 <- ifelse(b <= 1, r2*abs(b), r2/abs(b))
     
     return(br2)
     
  } # 'br2' END
  

br2.matrix <- function (sim, obs, na.rm=TRUE, ...){

    # Checking that 'sim' and 'obs' have the same dimensions
    if ( all.equal(dim(sim), dim(obs)) != TRUE )
      stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
            paste(dim(sim), collapse=" "), "] != [", 
            paste(dim(obs), collapse=" "), "] )", sep="") )

    br2 <- rep(NA, ncol(obs))       
          
    br2 <- sapply(1:ncol(obs), function(i,x,y) { 
                 br2[i] <- br2.default( x[,i], y[,i], na.rm=na.rm, ... )
            }, x=sim, y=obs )            
           
    return(br2)
     
  } # 'br2.matrix' END
  
  
br2.data.frame <- function (sim, obs, na.rm=TRUE, ...){

    sim <- as.matrix(sim)
    obs <- as.matrix(obs)

    br2.matrix(sim, obs, na.rm=na.rm, ...)        
     
  } # 'br2.data.frame' END
  
  
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
br2.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       br2.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'br2.zoo' end
