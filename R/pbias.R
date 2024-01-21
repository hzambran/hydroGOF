# File pbias.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2009-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'pbias': Percent Bias                                                        #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 03-Feb-2009;                                                        #
# Updates: 06-Sep-2009                                                         #
#          27-Apr-2020; 01-May-2020                                            #
#          16-Jan-2023                                                         #
#          20-Jan-2024                                                         #
################################################################################

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Percent Bias between 'sim' and 'obs', 
#           when multiplied by 100, its units is percentage
# Ref: Yapo P. O., Gupta H. V., Sorooshian S., 1996. 
#      Automatic calibration of conceptual rainfall-runoff models: 
#      sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48.

pbias <-function(sim, obs, ...) UseMethod("pbias")

pbias.default <- function(sim, obs, na.rm=TRUE, dec=1, fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

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
     
    # lenght of the data sets that will be ocnsidered for the ocmputations
    n <- length(obs)
      
    denominator <- sum( obs )
     
    if ( (denominator != 0) & (!is.na(denominator)) ) {      
      pbias <- 100 * ( sum( sim - obs ) / denominator )
      pbias <- round(pbias, dec)     
    } else {
        pbias <- NA
        warning("'sum((obs)=0' -> it is not possible to compute 'pbias' !")  
       } 
   } else {
       pbias <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
     
  return( pbias )
     
} # 'pbias.default' end
  

################################################################################
# 'pbias': Percent Bias                                                        #
################################################################################
# Started: 03-Feb-2009;                                                        #
# Updates: 06-Sep-2009                                                         #
#          27-Apr-2020; 01-May-2020                                            #
#          16-Jan-2023                                                         #
################################################################################  
pbias.matrix <- function(sim, obs, na.rm=TRUE, dec=1, fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){

   # Checking that 'sim' and 'obs' have the same dimensions
   if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

   pbias <- rep(NA, ncol(obs))       
          
   pbias <- sapply(1:ncol(obs), function(i,x,y) { 
                 pbias[i] <- pbias.default( x[,i], y[,i], na.rm=na.rm, dec=dec, 
                                            fun=fun, ..., 
                                            epsilon.type=epsilon.type,  
                                            epsilon.value=epsilon.value)
            }, x=sim, y=obs )        
                    
   return(pbias)
     
} # 'pbias.matrix' end
  

################################################################################
# 'pbias': Percent Bias                                                        #
################################################################################
# Started: 03-Feb-2009;                                                        #
# Updates: 06-Sep-2009                                                         #
#          27-Apr-2020; 01-May-2020                                            #
#          16-Jan-2023                                                         #
################################################################################ 
pbias.data.frame <- function(sim, obs, na.rm=TRUE, dec=1, fun=NULL, ...,
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  pbias.matrix(sim, obs, na.rm=na.rm, dec=dec, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'pbias.data.frame' end  



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 27-Apr-2020                                                         #
#          16-Jan-2023                                                         #
################################################################################
pbias.zoo <- function(sim, obs, na.rm=TRUE, dec=1, fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       pbias.matrix(sim, obs, na.rm=na.rm, dec=dec, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, dec=dec, ...)
     
} # 'pbias.zoo' end
