##################################################
# 'rNSE': Relative Nash-sutcliffe Efficiency     #
##################################################
# Started: April 2010                            #
# Updates: 01-Jun-2011                           #
#          14-Jan-2023                           #
##################################################
# Ref:
# Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency 
#                           criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005 
# Legates and McCabe, 1999. Evaluating the use of "goodness-of-fit" measures 
#                           in hydrologic and hydroclimatic model validation. 
#                           Water Resources Research. v35 i1. 233-241.

# Nash-Sutcliffe efficiency not "inflated" by squared values
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

rNSE <-function(sim, obs, ...) UseMethod("rNSE")

rNSE.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){ 

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")    

   epsilon.type <- match.arg(epsilon.type)  

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
	 
     # Testing for zero values in obs, which leads to -Inf as result
     zero.index <- which(obs==0)
     if (length(zero.index > 0) ) {
       warning("'rNSE' can not be computed: some elements in 'obs' are zero !", call.=FALSE)
     } # IF end
	 
     denominator <- sum( ( ( obs - mean(obs) ) / mean(obs) )^2 )
	 
     if (denominator != 0) {
	     rNSE <- 1 - ( sum( ( (obs - sim) / obs )^2 ) / denominator )
	   } else {
         rNSE <- NA
         warning("'sum( ( ( obs - mean(obs) ) / mean(obs) )^2 ) = 0', it is not possible to compute 'rNSE'")  
       } # ELSE end
   } else {
         rNSE <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
	 
   return(rNSE)
     
} # 'rNSE.default' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 14-Jan-2023                                                         #
################################################################################
rNSE.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  rNSE <- rep(NA, ncol(obs))       
          
  rNSE <- sapply(1:ncol(obs), function(i,x,y) { 
                 rNSE[i] <- rNSE.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                          epsilon.type=epsilon.type,  epsilon.value=epsilon.value)
               }, x=sim, y=obs )    
                     
  names(rNSE) <- colnames(obs)
  return(rNSE)
     
} # 'rNSE.matrix' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 14-Jan-2023                                                         #
################################################################################
rNSE.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rNSE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'rNSE.data.frame' end


rNSeff <-function(sim, obs, ...) UseMethod("rNSE")



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 14-Jan-2023                                                         #
################################################################################
rNSE.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rNSE.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'rNSE.zoo' end
