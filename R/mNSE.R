# File mNSE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2011-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

##################################################
# 'mNSE': Modified Nash-sutcliffe Efficiency     #
##################################################
# Started: July 28th, 2009;  06-Sep-09           #
# Updates: 01-Jun-2011                           #
#          29-Jul-2022                           #
##################################################
# Ref:
# Krause, P., Boyle, D. P., and Base, F. (2005). Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97
# Legates and McCabe, 1999. Evaluating the use of "goodness-of-fit" measures 
#                           in hydrologic and hydroclimatic model validation. 
#                           Water Resources Research. v35 i1. 233-241.

# Nash-Sutcliffe efficiency not "inflated" by squared values
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'j'   : numeric, with the exponent to be used in the computation of the modified Nash-Sutcliffe effciency. The default value is j=1

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

mNSE <-function(sim, obs, ...) UseMethod("mNSE")

mNSE.default <- function(sim, obs, j=1, na.rm=TRUE,
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){ 

	 if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")   

     epsilon.type <- match.arg(epsilon.type)
     
     # Checking that the provided exponent is positive
     if (j < 0 ) stop("Invalid argument: 'j' must be positive")           
   
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
	  
	   denominator <- sum( abs(obs - mean(obs, na.rm=na.rm))^j )
	 
	   if (denominator != 0) {	  
	     NS1 <- 1 - ( sum( abs(obs - sim)^j ) / denominator )	 
	   } else {
	       NS1 <- NA
	       warning("'sum(abs(obs - mean(obs))^j)=0', it is not possible to compute 'mNSE'")  
	     } # ELSE end

     } else {
         NS1 <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
       } # ELSE end
	 
	 return(NS1)
     
} # 'mNSE.default' end


mNSE.matrix <- function (sim, obs, j=1, na.rm=TRUE,
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  NS1 <- rep(NA, ncol(obs))       
          
  NS1 <- sapply(1:ncol(obs), function(i,x,y) { 
                 NS1[i] <- mNSE.default( x[,i], y[,i], j=j, na.rm=na.rm, fun=fun, ..., 
                                         epsilon.type=epsilon.type, epsilon.value=epsilon.value)
               }, x=sim, y=obs)    
                     
  names(NS1) <- colnames(obs)
  return(NS1)
     
} # 'mNSE.matrix' end


mNSE.data.frame <- function(sim, obs, j=1, na.rm=TRUE,
                            fun=NULL, ...,
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA){  
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  mNSE.matrix(sim, obs, j, na.rm=na.rm, fun=fun, ..., 
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'mNSE.data.frame' end


mNSeff <-function(sim, obs, ...) UseMethod("mNSE")


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 29-Jul-2022                                                         #
################################################################################
mNSE.zoo <- function(sim, obs, j=1, na.rm=TRUE, 
                     fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA){ 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       mNSE.matrix(sim, obs, j=j, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, j=j, na.rm=na.rm, fun=fun, ..., 
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'mNSE.zoo' end
