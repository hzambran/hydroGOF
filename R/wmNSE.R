##############################################
# Weighted Monthly Nash-Sutcliffe Efficiency #
##############################################
# Author: Mauricio Zambrano-Bigiarini        #
##############################################
# Started: 21-Jul-2011, JRC, Ispra           #
# Updates:                                   #
##############################################
# Reference: Unpublished (yet)               #
##############################################

wmNSE <-function(sim, obs, ...) UseMethod("wmNSE")

wmNSE.default <- function(sim, obs, FUN=median, j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
   
   if ( is.na(match(class(obs), c("zoo", "xts"))) ) 
     stop("Invalid argument type: 'obs' have to be of class: c('zoo', 'xts')")
     
   if (!require(hydroTSM))
     stop("Package hydroTSM is not present in your system => is not possible to compute wmNSE !")
     
   # Checking w
   if ( is.numeric(w) | is.integer(w) ) {
     if ( length(w) != 1 ) {
       if ( length(w) != length(obs) )
         stop( paste("Invalid argument: length(w) != length(obs)' (", length(w), "!=", length(obs), ")", sep="") )
     } # IF end
   } else if ( is.na(match(w, c("wl", "wh", "whl"))) )
      stop("Invalid argument: 'w' have to be in: c('wl', 'wh', 'whl')")
      
   # Checking 'j'
   if (j < 0) stop("Invalid argument: 'j' must be positive !")
   
   # Checking 'k'
   if (k < 0) stop("Invalid argument: 'k' must be positive !") 

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]
   
   # Computing the monthly medians. 12 values: Jan,..., Dec 
   monthly.med <- hydroTSM::monthlyfunction(obs, FUN=FUN)
   
   # Creating an index with the month number corresponding to each observed value
   month <- as.numeric(format(time(obs), "%m"))
   
   obs <- as.numeric(obs)
   sim <- as.numeric(sim)

   n <- length(obs)
   
   # Computing the weights
   if ( is.numeric(w) | is.integer(w) ) {
      if ( length(w) == 1 ) {
         w <- rep(w, n)
      } else w <- w[vi]
   } else if (w=="wl") {
       w <- wl.default(x=obs, k=k, pbb=pbb, ...)
     } else if (w=="wh") {
         w <- wh.default(x=obs, k=k, pbb=pbb, ...)
       } else if (w=="whl") {
           w <- whl.default(x=obs, lambda=lambda, lQ=lQ, hQ=hQ, ... )
         } # IF end
       
   # Creating a numeric vector with the seasonal average corresponding to each value of 'obs'
   mNSE.med <- rep(NA, n)
   for ( i in 1:12) {
     month.index <- which(month==i)
     mNSE.med[month.index] <- monthly.med[i]
   } # FOR end
   
   denominator <- sum( ( abs(w*(obs - mNSE.med)) )^j )
     
   if (denominator != 0) {
      
     wmNSE <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else stop("'sum((abs(w*(obs - median(obs))))^j)=0' => it is not possible to compute 'wmNSE'")  

   return(wmNSE)

} # 'wmNSE.default' end


wmNSE.matrix <- function(sim, obs, FUN=median, j=0.5, 
                        w="wl", k=0.5, pbb=0.8, 
                        lambda=0.5, 
                        lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                        hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
                        
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  n     <- ncol(obs)
  wmNSE <- rep(NA, n)

  wmNSE <- sapply(1:n, function(i,x,y) {
                 wmNSE[i] <- wmNSE.default( as.numeric(x[,i]), y[,i], FUN=FUN, j=j, w=w, k=k, pbb=pbb,
                                          lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm, ... )
               }, x=sim, y=obs )

  names(wmNSE) <- colnames(obs)

  return(wmNSE)

} # 'wmNSE.matrix' end


wmNSE.data.frame <- function(sim, obs, FUN=median, j=0.5, 
                            w="wl", k=0.5, pbb=0.8, 
                            lambda=0.5, 
                            lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                            hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wmNSE.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wmNSE.data.frame' end


wmNSE.zoo <- function (sim, obs, FUN=median, j=0.5, 
                       w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, 
                       lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){ 
 
  if ( is.matrix(sim) | is.data.frame(sim) ) sim <- coredata(sim)
  
  if ( is.matrix(sim) ) {
     wmNSE.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
  } else wmNSE.default(sim, obs, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
     
} # 'wmNSE.zoo' end
