# File md.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2010-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################
# 'md': Modified Index of Agreement    #
########################################
# Started: 07-Apr-2010                 #
# Updates: 14-Jan-2023                 #
########################################
# Ref
# 1) Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005 

# Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0 
# and the closer to 1 the better the performance of the model 

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'j'   : numeric, with the exponent to be used in the computation of the modified index  of agreement. The default value is j=1

# 'Result': Modified Index of Agreement between 'sim' and 'obs'

md <-function(sim, obs, ...) UseMethod("md")

md.default <- function(sim, obs, j=1, na.rm=TRUE,
                       fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){ 

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     epsilon.type <- match.arg(epsilon.type)

     # the next two lines are required for avoiding an strange behaviour 
     # of the difference function when sim and obs are time series.
     if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
     if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)

     # Checking that the provided exponent is positive
     if (j < 0 ) stop("Invalid argument: 'j' must be positive")   

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
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
     
       # Mean of the observed values
       Om <- mean(obs)
      
       denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^j )
     
       if (denominator != 0) {
         d1 <- 1 - ( sum( ( abs(obs - sim) )^j ) / denominator )
       } else {
           d1 <- NA
           warning("'sum((abs(sim-Om)+abs(obs-Om))^j)=0', it is not possible to compute 'md'")  
         } # ELSE end

     } else {
         d1 <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
       } # ELSE end
     
     return(d1) 
     
} # 'md.default' end


md.matrix <- function(sim, obs, j=1, na.rm=TRUE,
                      fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){ 

 # Checking that 'sim' and 'obs' have the same dimensions
 if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

 d1 <- rep(NA, ncol(obs))       
          
 d1 <- sapply(1:ncol(obs), function(i,x,y) { 
                 d1[i] <- md.default( x[,i], y[,i], j, na.rm=na.rm, fun=fun, ..., 
                                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                 }, x=sim, y=obs )    
                     
  names(d1) <- colnames(obs)
  return(d1)
     
} # 'md.matrix' end


md.data.frame <- function(sim, obs, j=1, na.rm=TRUE,
                          fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  md.matrix(sim=sim, obs=obs, j=j, na.rm=na.rm, ...)
     
} # 'md.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 14-Jan-2023                                                         #
################################################################################
md.zoo <- function(sim, obs, j=1, na.rm=TRUE,
                   fun=NULL, ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       md.matrix(sim, obs, j=j, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, j=j, na.rm=na.rm, ...)
     
  } # 'md.zoo' end
