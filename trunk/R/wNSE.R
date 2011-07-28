########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 22-Jan-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSE <-function(sim, obs, ...) UseMethod("wNSE")

wNSE.default <- function(sim, obs, j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
   
   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
        is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")
     
   if ( is.na(match(w, c("wl", "wh", "whl"))) )
      stop("Invalid argument: 'w' have to be in: c('wl', 'wh', 'whl')")
      
   # Checking 'j'
   if (j < 0) stop("Invalid argument: 'j' must be positive !")
   
   # Checking 'k'
   if (k < 0) stop("Invalid argument: 'k' must be positive !")   
   
   # Computing valid indexes for 'sim' and 'obs'
   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]

   n <- length(obs)
   
   if (w=="wl") {
     w <- wl.default(x=obs, k=k, pbb=pbb, ...)
   } else if (w=="wh") {
       w <- wh.default(x=obs, k=k, pbb=pbb, ...)
     } else if (w=="whl") {
       w <- whl.default(x=obs, lambda=lambda, lQ=lQ, hQ=hQ, ... )
       } # ELSE end
   
   denominator <- sum( ( abs(w*(obs - median(obs))) )^j )
     
   if (denominator != 0) {
      
     wNSE <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else {
      wNSE <- NA
      warning("'sum((abs(w*(obs - median(obs))))^j)=0' => it is not possible to compute 'wNSE'")  
     } # ELSE end 

   return(wNSE)

} # 'wNSE.default' end


wNSE.matrix <- function(sim, obs, j=0.5, 
                        w="wl", k=0.5, pbb=0.8, 
                        lambda=0.5, 
                        lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                        hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
                        
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  wNSE <- rep(NA, ncol(obs))

  wNSE <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSE[i] <- wNSE.default( x[,i], y[,i], j=j, w=w, k=k, pbb=pbb,
                                          lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm, ... )
               }, x=sim, y=obs )

  names(wNSE) <- colnames(obs)

  return(wNSE)

} # 'wNSE.matrix' end


wNSE.data.frame <- function(sim, obs, j=0.5, 
                            w="wl", k=0.5, pbb=0.8, 
                            lambda=0.5, 
                            lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                            hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSE.matrix(sim, obs, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wNSE.data.frame' end
