########################################
# 'NSE': Nash-sutcliffe Efficiency     #
########################################
# 15-Dic-2008   ; 06-Sep-09            #
# 29-Jun-2017                          #
########################################
# Nash-Sutcliffe efficiencies (Nash and Sutcliffe, 1970) range from -Inf to 1. 
# An efficiency of 1 (NSE = 1) corresponds to a perfect match of modeled to the observed data. 
# An efficiency of 0 (NSE = 0) indicates that the model predictions are as accurate
# as the mean of the observed data, whereas 
# an efficiency less than zero (-Inf < NSE < 0) occurs when the observed mean is a better predictor than the model.
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Nash-sutcliffe Efficiency between 'sim' and 'obs'

NSE <-function(sim, obs, ...) UseMethod("NSE")

NSE.default <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){ 

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      

   vi <- valindex(sim, obs)
     
   obs <- obs[vi]
   sim <- sim[vi]

   if (!is.null(FUN)) {
     new <- preproc(sim=sim, obs=obs, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
     sim <- new[["sim"]]
     obs <- new[["obs"]]
   } # IF end
     
   denominator <- sum( (obs - mean(obs))^2 )
     
   if (denominator != 0) {
      
     NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )
     
   } else {
       NS <- NA
       warning("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSE'")  
     } # ELSE end
     
   return(NS)
     
} # 'NSE' end


NSE.matrix <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  NS <- rep(NA, ncol(obs))       
          
  NS <- sapply(1:ncol(obs), function(i,x,y) { 
                 NS[i] <- NSE.default( x[,i], y[,i], na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
               }, x=sim, y=obs )    
                     
  names(NS) <- colnames(obs)
  
  return(NS)
     
} # 'NSE.matrix' end


NSE.data.frame <- function (sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  NSE.matrix(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
     
} # 'NSE.data.frame' end


NSeff <-function(sim, obs, ...) UseMethod("NSE")


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
NSE.zoo <- function(sim, obs, na.rm=TRUE, FUN=NULL, epsilon=c(0, "Pushpalatha2012", "other"), epsilon.value=NA, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       NSE.matrix(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, FUN=FUN, epsilon=epsilon, epsilon.value=epsilon.value, ...)
     
  } # 'NSE.zoo' end

