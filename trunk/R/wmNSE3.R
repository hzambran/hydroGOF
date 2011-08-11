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

wmNSE3 <-function(sim, obs, ...) UseMethod("wmNSE3")

wmNSE3.zoo <- function(sim, obs, FUN=median, j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                         na.rm=TRUE, ...){
  
   # Checking that the user provied a valid class for 'x'   
   valid.class <- c("xts", "zoo")    
   if (length(which(!is.na(match(class(obs), valid.class )))) <= 0)  
      stop("Invalid argument: 'class(obs)' must be in c('xts', 'zoo')")
   
   sim <- zoo::coredata(sim)

   wmNSE3.default(sim=sim, obs=obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wmNSE3.zoo' end


wmNSE3.default <- function(sim, obs, FUN=median, j=0.5, 
                           w="wl", k=0.5, pbb=0.8, 
                           lambda=0.5, 
                           lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                           hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                           na.rm=TRUE, ...){
     
   #############################################################################
   ###########################      Checkings     ##############################
   #############################################################################
     
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
   
   # Checking 'lambda'
   if ( (lambda < 0) | (lambda > 1) ) stop("Invalid argument: '0 <= lambda <= 1' !") 

   #############################################################################
   ###########################    Computations    ##############################
   #############################################################################
   
   # 0) Remo0ving NA's
   vi  <- valindex(sim, obs)
   obs <- obs[vi]
   sim <- sim[vi]
   
   # 1) Computing the monthly medians. 12 values: Jan,..., Dec 
   monthly.med <- hydroTSM::monthlyfunction(obs, FUN=FUN, na.rm=na.rm)
   
   # Creating an index with the month number corresponding to each observed value
   month <- as.numeric(format(time(obs), "%m"))
   
   # 2) Removing time attributes for faster computations
   obs <- as.numeric(obs)
   sim <- as.numeric(sim)

   n <- length(obs)
   
   # 3) Computing the weights
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
       
   # 4) Creating a numeric vector with the monthly median corresponding to each value of 'obs'
   obs.med <- rep(NA, n)
   for ( i in 1:12) {
     month.index <- which(month==i)
     obs.med[month.index] <- monthly.med[i]
   } # FOR end
    
    
   # 5) Computing wmNSE 
   
   # 5.1) Computing the denominator of wmNSE
   denominator <- sum( ( abs(w*(obs - obs.med)) )^j )
   
   if (denominator != 0) {   
     wmNSE3 <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )     
   } else {   
      wmNSE3 <- NA
      warning("'sum((abs(w*(obs - median(obs))))^j)=0' => it is not possible to compute 'wmNSE3'")        
     } # ELSE end 

   return(wmNSE3)

} # 'wmNSE3.default' end


wmNSE3.matrix <- function(sim, obs, FUN=median, j=0.5, 
                          w="wl", k=0.5, pbb=0.8, 
                          lambda=0.5, 
                          lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                          hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                          #lQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                          #hQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                          na.rm=TRUE, ...){
  
  #############################################################################
  ###########################      Checkings     ##############################
  #############################################################################
                       
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
  
  # Checking 'lambda'
   if ( (lambda < 0) | (lambda > 1) ) stop("Invalid argument: '0 <= lambda <= 1' !") 
  
  #############################################################################
  ###########################    Computations    ##############################
  #############################################################################
   
  # number of columns in 'obs'
  nsim <- ncol(obs)    
  
  # number of rows in 'obs'
  nobs <- nrow(obs)                                         
  
  ##################
  # 0) Removing NA's
  # Creating a matrix with the index of the pairs of 'sim' and 'obs' with valid & NA values
  valid.index   <- !is.na(sim) & !is.na(obs)  
  invalid.index <- which(valid.index == FALSE)
  
  # Putting NA's in each row of 'sim' and 'obs' where a value in 'sim' OR 'obs' is missing
  sim[invalid.index] <- NA
  obs[invalid.index] <- NA
  
  #####################
  # 1) Computing the monthly medians. 12 values: Jan,..., Dec 
  monthly.med <- hydroTSM::monthlyfunction(obs, FUN=FUN, na.rm=na.rm)
  
  # Transposition needed in order to have the monthly values in rows and each 
  # column representing a different ts of 'obs'
  monthly.med <- t(as.matrix(monthly.med))
  
  # Creating an index with the month number corresponding to each observed value
  month <- as.numeric(format(time(obs), "%m"))
  
  #####################
  # 2) Removing time attributes for faster computations
  obs <- coredata(obs)
  if (zoo::is.zoo(sim)) sim <- coredata(sim)
  
  ##########################
  # 3) Computing the weights
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
    
  ###########################
  # 4) Creating a numeric matrix with the monthly median corresponding to each value of 'obs'     
  
  # Numeric, with a numeric index ( [1,12]) indicating which monthly value has to be used for each observed value
  obs.monthly.med.index <- pmatch(month, as.numeric(time(monthly.med)), duplicates.ok=TRUE)
  
  # Matrix with the monthly values corresponding to each value of 'obs'
  obs.med <- matrix( rep( monthly.med[obs.monthly.med.index ], nsim), ncol=nsim)  
  
  # Putting NA's in each row of 'obs.med' where a value in 'sim' OR 'obs' is missing
  obs.med[invalid.index] <- NA 
  
  
  ##########################
  # 5) Computing wmNSE 
  
  # 5.1) Residulas in the numerator
  R <- abs(sim - obs)  
  
  # 5.2) Residuals in the denominator
  Rmed <- abs(obs - obs.med)
  
  # 5.3) wmNSE numerator
  num <- colSums((w*R)^j, na.rm=TRUE)
  
  # 5.4) wmNSE denominator
  denom <- colSums((w*Rmed)^j, na.rm=TRUE)
  
  # 5.5) wmNSE computation
  wmNSE <- 1 - num/denom
   
  names(wmNSE) <- colnames(obs)

  return(wmNSE)

} # 'wmNSE3.matrix' end


wmNSE3.data.frame <- function(sim, obs, FUN=median, j=0.5, 
                            w="wl", k=0.5, pbb=0.8, 
                            lambda=0.5, 
                            #lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                            #hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                            lQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                            hQ=apply(obs, 2, quantile, probs=0.3, na.rm=TRUE),
                            na.rm=TRUE, ...){

  sim <- coredata(sim)
  #obs <- coredata(obs)

  wmNSE3.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wmNSE3.data.frame' end


wmNSE3.zoo <- function (sim, obs, FUN=median, j=0.5, 
                       w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, 
                       lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){ 
 
  if ( is.matrix(sim) | is.data.frame(sim) ) sim <- coredata(sim)
  
  if ( is.matrix(sim) ) {
     wmNSE3.matrix(sim, obs, FUN=FUN, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
  } else wmNSE3.default(sim, obs, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)
     
} # 'wmNSE3.zoo' end
