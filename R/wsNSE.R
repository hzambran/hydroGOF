########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 20-Jul-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wsNSE <-function(sim, obs, ...) UseMethod("wsNSE")

wsNSE.default <- function(sim, obs, j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
   
   if ( is.na(match(class(sim), c("zoo", "xts"))) |
        is.na(match(class(obs), c("zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('zoo', 'xts')")
     
   if (!require(hydroTSM))
     stop("Package hydroTSM is not present in your system => is not possible to compute wsNSE !")
     
   if ( is.na(match(w, c("wl", "wh", "whl"))) )
      stop("Invalid argument: 'w' have to be in: c('wl', 'wh', 'whl')")
      
   # Checking 'j'
   if (j < 0)
      stop("Invalid argument: 'j' must be positive !")
   
   # Checking 'k'
   if (k < 0)
      stop("Invalid argument: 'k' must be positive !")    
   

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]

   n <- length(obs)
   
   # Computing the weights
   if (w=="wl") {
     w <- wl.default(x=obs, k=k, pbb=pbb, ...)
   } else if (w=="wh") {
       w <- wh.default(x=obs, k=k, pbb=pbb, ...)
     } else if (w=="whl") {
       w <- whl.default(x=obs, lambda=lambda, lQ=lQ, hQ=hQ, ... )
       } # ELSE end
   
   # Computing the seasonal medians. 4 values:  DJF , MAM, JJA,  SON 
   seasonal.med <- hydroTSM::seasonalfunction(obs, FUN=median)
   
   # Creating a numeric vector with the seasonal average corresponding to each value of 'obs'
   season <- time2season(time(obs))
   
   DJF.index <- which(season=="DJF")
   MAM.index <- which(season=="MAM")
   JJA.index <- which(season=="JJA")
   SON.index <- which(season=="SON")
   
   sNSE.med <- rep(NA, length(obs))
   
   sNSE.med[DJF.index] <- seasonal.med[1]
   sNSE.med[MAM.index] <- seasonal.med[2]
   sNSE.med[JJA.index] <- seasonal.med[3]
   sNSE.med[SON.index] <- seasonal.med[4]
   
   denominator <- sum( ( abs(w*(obs - sNSE.med)) )^j )
     
   if (denominator != 0) {
      
     wsNSE <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else stop("'sum((abs(w*(obs - median(obs))))^j)=0' => it is not possible to compute 'wsNSE'")  

   return(wsNSE)

} # 'wsNSE.default' end


wsNSE.matrix <- function(sim, obs, j=0.5, 
                        w="wl", k=0.5, pbb=0.8, 
                        lambda=0.5, 
                        lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                        hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){
                        
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  wsNSE <- rep(NA, ncol(obs))

  wsNSE <- sapply(1:ncol(obs), function(i,x,y) {
                 wsNSE[i] <- wsNSE.default( x[,i], y[,i], j=j, w=w, k=k, pbb=pbb,
                                          lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm, ... )
               }, x=sim, y=obs )

  names(wsNSE) <- colnames(obs)

  return(wsNSE)

} # 'wsNSE.matrix' end


wsNSE.data.frame <- function(sim, obs, j=0.5, 
                            w="wl", k=0.5, pbb=0.8, 
                            lambda=0.5, 
                            lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                            hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wsNSE.matrix(sim, obs, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm,...)

} # 'wsNSE.data.frame' end
