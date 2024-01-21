# File d.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2008-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

########################################
# 'IoA': Index of Agreement            #
########################################
# December 18th, 2008;  06-Sep-09      #
# 28-Feb-2016                          #
# Updates: 20-Jul-2022 ; 29-Jul-2022   #
########################################
# 1) Willmott, C.J. 1981. On the validation of models. Physical Geography, 2, 184-194
# 2) Willmott, C. J. (1984). "On the evaluation of model performance in physical geography." Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460.
# 3) Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241. 

# Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0 
# and the closer to 1 the better the performance of the model 

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Index of Agreement between 'sim' and 'obs'

d <-function(sim, obs, ...) UseMethod("d")

d.default <- function(sim, obs, na.rm=TRUE,
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
       Om <- mean(obs, na.rm=na.rm)
      
       denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^2 )
     
       if ( (denominator != 0) & (!is.na(denominator)) ) {      
         d <- 1 - ( sum( (obs - sim)^2 ) / denominator )     
       } else {      
           d <- NA   
           warning("'sum((abs(sim-Om)+abs(obs-Om))^2)=0', it is not possible to compute 'IoA'")           
         }
     } else {
         d <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
       } # ELSE end
     
     return(d) 
     
} # 'd.default' end


d.matrix <- function(sim, obs, na.rm=TRUE,
                     fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA){ 
 
 # Checking that 'sim' and 'obs' have the same dimensions
 if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

 d <- rep(NA, ncol(obs))       
          
 d <- sapply(1:ncol(obs), function(i,x,y) { 
             d[i] <- d.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
             }, x=sim, y=obs)    
                     
  names(d) <- colnames(obs)

  return(d)
     
} # 'd.matrix' end


d.data.frame <- function(sim, obs, na.rm=TRUE,
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){  
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  d.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
           epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
     
} # 'd.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 20-Jul-2022 ; 29-Jul-2022                                           #
################################################################################
d.zoo <- function(sim, obs, na.rm=TRUE,
                  fun=NULL, ...,
                  epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                  epsilon.value=NA){ 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       d.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
     
  } # 'd.zoo' end
