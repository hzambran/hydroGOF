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

wmNSE2 <-function(sim, obs, ...) UseMethod("wmNSE2")

wmNSE2.default <- function(sim, obs, FUN=median, j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                         na.rm=TRUE, ...){
  
   if ( is.na(match(class(obs), c("zoo", "xts"))) ) 
     stop("Invalid argument type: 'obs' have to be of class: c('zoo', 'xts')")
     
   if (!require(hydroTSM))
     stop("Package hydroTSM is not present in your system => is not possible to compute wmNSE2 !")
     
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
   monthly.med <- hydroTSM::monthlyfunction(obs, FUN=FUN, na.rm=na.rm)
   
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
   
     wmNSE2 <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else {
      wmNSE2 <- NA
      warning("'sum((abs(w*(obs - median(obs))))^j)=0' => it is not possible to compute 'wmNSE2'")  
     } # ELSE end 

   return(wmNSE2)

} # 'wmNSE2.default' end


wmNSE2.matrix <- function(sim, obs, FUN=median, j=0.5, 
                        w="wl", k=0.5, pbb=0.8, 
                        lambda=0.5, 
                        lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                        hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                        #lQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                        #hQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                        na.rm=TRUE, ...){
  
                        
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") ) 
  
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
  
  # number of columns in 'obs'
  nsim <- ncol(obs)    
  
  # number of rows in 'obs'
  nobs <- nrow(obs)    
                                      
  # wrapper to 'monthlyfunction.default' with column index      
  .monthlyfunctioncol <- function(col, x, FUN2, na.rm=TRUE, ...) {					    
     hydroTSM::monthlyfunction.default(x[,col], FUN=FUN2, na.rm=na.rm, ...)   
  } # '.monthlyfunctioncol' END  
  
  # matrix with the index of the pairs of 'sim' and 'obs' with valid values
  valid.index <- !is.na(sim) & !is.na(obs)  
  invalid.index <- which(valid.index == FALSE)
  
  # Putting NA's in each row of 'sim' and 'obs' where a value in 'sim' OR 'obs' is missing
  sim[invalid.index] <- NA
  obs[invalid.index] <- NA
  
  #monthly.med <- apply(X=obs, MARGIN=2, FUN=.monthlyfunctioncol, FUN2=FUN, na.rm=na.rm,...)
  monthly.med <- sapply(1:nsim, FUN=.monthlyfunctioncol, x=obs, FUN2=FUN, na.rm=na.rm,...)
  
  # Creating an index with the month number corresponding to each observed value
  month <- as.numeric(format(time(obs), "%m"))
  
  # Numeric, with a numeric index ( [1,12]) indicating which monthly value has to be used for each observed value
  obs.monthly.med.index <- pmatch(month, as.numeric(time(monthly.med)), duplicates.ok=TRUE)
  
  # Matrix with the monthly values corresponding to each value of 'obs'
  obs.med <- matrix( rep( monthly.med[obs.monthly.med.index ], nsim), ncol=nsim)
  
  # Removing time attributes for faster computations
  obs <- coredata(obs)
  if (is.zoo(sim)) sim <- coredata(sim)
  
  # Putting NA's in each row of 'obs.med' where a value in 'sim' OR 'obs' is missing
  obs.med[invalid.index] <- NA
  
  # Computing the weights
  if ( is.numeric(w) | is.integer(w) ) {
      if ( length(w) == 1 ) {
         w <- matrix(rep(w, nsim*nobs), ncol=nsim)
      } else w <- matrix(rep(w, nsim), ncol=nsim)
  } else if (w=="wl") {
       w <- wl.matrix(x=obs, k=k, pbb=pbb, ...)
     } else if (w=="wh") {
         w <- wh.matrix(x=obs, k=k, pbb=pbb, ...)
       } else if (w=="whl") {
           w <- whl.matrix(x=obs, lambda=lambda, lQ=lQ, hQ=hQ, ... )
         } # IF end   
  
  #residulas in the numerator
  R <- abs(sim - obs)  
  
  # residuals in the denominator
  Rmed <- abs(obs - obs.med)
  
  # wmNSE numerator
  num <- colSums((w*R)^j, na.rm=TRUE)
  
  # wmNSE denominator
  denom <- colSums((w*Rmed)^j, na.rm=TRUE)
  
  # wmNSE computation
  wmNSE <- 1 - num/denom
   
  names(wmNSE) <- colnames(obs)

  return(wmNSE)

} # 'wmNSE2.matrix' end


wmNSE2.data.frame <- function(sim, obs, FUN=median, j=0.5, 
                            w="wl", k=0.5, pbb=0.8, 
                            lambda=0.5, 
                            #lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                            #hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                            lQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                            hQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                            na.rm=TRUE, ...){

  sim <- coredata(sim)
  #obs <- coredata(obs)

  wmNSE2.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wmNSE2.data.frame' end


wmNSE2.zoo <- function (sim, obs, FUN=median, j=0.5, 
                       w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, 
                       lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){ 
 
  if ( is.matrix(sim) | is.data.frame(sim) ) sim <- coredata(sim)
  
  if ( is.matrix(sim) ) {
     wmNSE2.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
  } else wmNSE2.default(sim, obs, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
     
} # 'wmNSE2.zoo' end
