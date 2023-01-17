# File me.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# 'me': Mean Error                                                             #
################################################################################
# Started:  15-Dic-2008; 06-Sep-09                                             #
# Updates:  16-Jan-2023                                                        #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Mean Error between 'sim' and 'obs', in the same units of 'sim' and 'obs' 

me <-function(sim, obs, ...) UseMethod("me")

me.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){

     
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")    
      
  if ( length(obs) != length(sim) ) 
	 stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !") 

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
		      
    me <- mean( sim - obs, na.rm = na.rm)           

  } else {
      me <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end 
     
  return(me)
     
} # 'me.default' end
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started:  15-Dic-2008; 06-Sep-09                                             #
# Updates:  16-Jan-2023                                                        #
################################################################################  
me.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  #me <- colMeans( sim - obs, na.rm= na.rm) 

  me <- rep(NA, ncol(obs))       
          
  me <- sapply(1:ncol(obs), function(i,x,y) { 
                 me[i] <- me.default( x[,i], y[,i], na.rm=na.rm, norm=norm, 
                                      fun=fun, ..., 
                                      epsilon.type=epsilon.type,  
                                      epsilon.value=epsilon.value)
               }, x=sim, y=obs ) 
   
 return(me)
     
} # 'me' end
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started:  15-Dic-2008; 06-Sep-09                                             #
# Updates:  16-Jan-2023                                                        #
################################################################################   
me.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){

   sim <- as.matrix(sim)
   obs <- as.matrix(obs)
	
   me.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'me.data.frame' end
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
me.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       me.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
} # 'me.zoo' end
