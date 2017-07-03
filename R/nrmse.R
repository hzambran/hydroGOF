# File nrmse.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2008-2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'nrmse': Normalized Root Mean Square Error                                   #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008                                                         #
# Updates: 06-Sep-2009                                                         #
#          03-Jul-2017                                                         #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'norm'  : character, indicating the value to be used to normalise the RMS. Valid values are:
#           -) 'sdobs' : standard deviation of observations.
#           -) 'maxmin': difference between maximum and minimum observed values

# 'Result': Normalized Root Mean Square Error between 'sim' and 'obs', 
#           when multiplied by 100 its units is %

nrmse <-function(sim, obs, ...) UseMethod("nrmse")
 
nrmse.default <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

    # Checking that the user provied a valid argument for 'norm'       
    if (is.na(match(norm, c("sd", "maxmin") ) ) ) 
       stop("Invalid argument: 'norm' must be in c('sd', 'maxmin')")

    if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      

    vi <- valindex(sim, obs)
     
    obs <- obs[vi]
    sim <- sim[vi]
       
    if (norm=="sd") {
      cte <- sd(obs, na.rm=na.rm)
    } else if (norm=="maxmin") {
        cte <- ( max(obs, na.rm= na.rm) - min(obs, na.rm =na.rm) )
      } # ELSE end

     rmse <- rmse(sim, obs, na.rm) 
     
     if (max(obs, na.rm= na.rm) - min(obs, na.rm= na.rm) != 0) {
     
       nrmse <- rmse / cte
     
     } else {
         nrmse <- NA
         warning("'obs' is constant, it is not possible to compute 'nrmse'")  
       } # ELSE end
     
     return( round( 100*nrmse, 1) )
     
  } # 'nrmse.default' end
  
 
################################################################################
# 'nrmse': Normalized Root Mean Square Error                                   #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008                                                         #
# Updates: 06-Sep-2009 ; 05-Nov-2012                                           #
################################################################################
nrmse.matrix <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
          
  # Checking that the user provied a valid argument for 'norm'       
  if (is.na(match(norm, c("sd", "maxmin") ) ) ) 
     stop("Invalid argument: 'norm' must be in c('sd', 'maxmin')")

  nrmse <- rep(NA, ncol(obs))       
          
  nrmse <- sapply(1:ncol(obs), function(i,x,y) { 
                 nrmse[i] <- nrmse.default( x[,i], y[,i], na.rm=na.rm, norm=norm, ... )
               }, x=sim, y=obs )    
                     
  names(nrmse) <- colnames(obs)
  
  return(nrmse)
     
} # 'nrms.matrix' end


################################################################################
# 'nrmse': Normalized Root Mean Square Error                                   #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008                                                         #
# Updates: 06-Sep-2009                                                         #
################################################################################
nrmse.data.frame <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  nrmse.matrix(sim, obs, na.rm=na.rm, norm=norm, ...)
     
} # 'nrmse.data.frame' end
  
  
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
nrmse.zoo <- function(sim, obs, na.rm=TRUE, norm="sd", ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       nrmse.matrix(sim, obs, na.rm=na.rm, norm=norm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, norm=norm, ...)
     
  } # 'nrmse.zoo' end
