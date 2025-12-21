# File nrmse.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'nrmse': Normalized Root Mean Square Error                                   #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008                                                         #
# Updates: 06-Sep-2009                                                         #
#          03-Jul-2017                                                         #
#          16-Jan-2023                                                         #
#          21-Dec-2025                                                         #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'norm'  : character, indicating the value to be used to normalise the RMS. Valid values are:
#           -) 'sd'    : standard deviation of observations.
#           -) 'maxmin': difference between maximum and minimum observed values
#           -) 'mean'  : arithmetic mean of observed values
#           -) 'IQR'   : interquantile range of observed values, computed using the \code{stats::IQR} function with \code{type=7}.

# 'Result': Normalized Root Mean Square Error between 'sim' and 'obs', 
#           when multiplied by 100 its units is %

nrmse <-function(sim, obs, ...) UseMethod("nrmse")
 
nrmse.default <- function(sim, obs, na.rm=TRUE, norm=c("sd", "maxmin", "mean", "IQR"), fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")     

  # Checking the 'norm' argument
  norm <- match.arg(norm)   

  # Checking the 'epsilon.type' argument
  epsilon.type <- match.arg(epsilon.type) 
 
  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)
   
  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end     

    cte = switch(  
            norm,  
            "sd"= sd(obs, na.rm=na.rm), 
            "maxmin"= ( max(obs, na.rm= na.rm) - min(obs, na.rm =na.rm) ),  
            "mean"= mean(obs, na.rm=na.rm),  
            "IQR"= stats::IQR(obs, na.rm= na.rm, type=7),
          )  

    rmse <- rmse(sim, obs, na.rm) 
     
    if (cte != 0) {     
       nrmse <- rmse / cte     
    } else {
        nrmse <- NA
        warning("'obs' is constant -> it is not possible to compute 'nrmse' !")  
      } # ELSE end

  } else {
      nrmse <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
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
#          21-Dec-2025                                                         #
################################################################################
nrmse.matrix <- function(sim, obs, na.rm=TRUE, norm=c("sd", "maxmin", "mean", "IQR"), fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  nrmse <- rep(NA, ncol(obs))       
          
  nrmse <- sapply(1:ncol(obs), function(i,x,y) { 
                 nrmse[i] <- nrmse.default( x[,i], y[,i], na.rm=na.rm, norm=norm, 
                                            fun=fun, ..., 
                                            epsilon.type=epsilon.type,  
                                            epsilon.value=epsilon.value)
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
#          21-Dec-2025                                                         #
################################################################################
nrmse.data.frame <- function(sim, obs, na.rm=TRUE, norm=c("sd", "maxmin", "mean", "IQR"), fun=NULL, ...,
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  nrmse.matrix(sim, obs, na.rm=na.rm, norm=norm, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'nrmse.data.frame' end
  
  
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 21-Dec-2025                                                         #
################################################################################
nrmse.zoo <- function(sim, obs, na.rm=TRUE, norm=c("sd", "maxmin", "mean", "IQR"), fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       nrmse.matrix(sim, obs, na.rm=na.rm, norm=norm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, norm=norm, ...)
     
} # 'nrmse.zoo' end
