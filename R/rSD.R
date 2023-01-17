# File rSD.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'rSD': Ratio of Standard Deviations                                          #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          03-Jul-2017                                                         #
#          16-Jan-2023                                                         #
################################################################################

# SD(sim) / SD(obs)  
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Ratio of Standard Deviations  between 'sim' and 'obs', 
#           when multiplied by 100 its units is % 

rSD <-function(sim, obs, ...) UseMethod("rSD")

rSD.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      

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

    denominator <- sd(obs, na.rm = na.rm)
     
    if (denominator != 0) {     
     rSD <- sd(sim, na.rm= na.rm) / sd(obs, na.rm= na.rm)     
    } else {
       rSD <- NA
       warning("'sd(obs)=0' -> it is not possible to compute 'rSD' !")  
      } 
   } else {
         rSD <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
     
  return(rSD)
     
} # 'rSD.default' end
  

################################################################################
# 'rSD': Ratio of Standard Deviations                                          #
################################################################################
# Started: 15-Dic-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          03-Jul-2017                                                         #
#          16-Jan-2023                                                         #
################################################################################  
rSD.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") ) 
 
  rSD <- rep(NA, ncol(obs))       
          
  rSD <- sapply(1:ncol(obs), function(i,x,y) { 
                 rSD[i] <- rSD.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                        epsilon.type=epsilon.type,  
                                        epsilon.value=epsilon.value)
               }, x=sim, y=obs )    
                     
  names(rSD) <- colnames(obs)
  
  return(rSD)
     
} # 'rSD.matrix' end


################################################################################
# 'rSD': Ratio of Standard Deviations                                          #
################################################################################
# Started: 15-Dic-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          03-Jul-2017                                                         #
#          16-Jan-2023                                                         #
################################################################################ 
rSD.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rSD.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'rSD.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
rSD.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rSD.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'rSD.zoo' end
