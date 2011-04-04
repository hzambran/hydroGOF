########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 04-Feb-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSeff5 <-function(sim, obs, ...) UseMethod("wNSeff5")

wNSeff5.default <- function(sim, obs, j=2, lambda=0.5, 
                           lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                           hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                           ...){

   ##############################################
   # Weigthing function for HIGH & LOW flows II #
   ##############################################
   # lambda: wheight that will be given to the high flows #
   whl <- function(x, lambda=0.5, 
                   lQ=quantile(x, na.rm=TRUE, probs=0.2), 
                   hQ=quantile(x, na.rm=TRUE, probs=0.8) ) {

      x <- as.numeric(x)

      n <- length(x)

      if (lambda < 0 | lambda > 1)
         stop("Invalid argument: 0 <= lambda <= 1")

      index.high <- which(x >= hQ )
      index.low  <- which(x <= lQ )
      index.med  <- which(x < hQ & x > lQ)

      w <- rep(NA, n)

      w[index.high] <- lambda
      w[index.low]  <- 1 - lambda
      w[index.med]  <- 1 - lambda + (x[index.med]-lQ)*(2*lambda-1)/(hQ-lQ)

       return( w )

   } # 'whl' END


   ########################################
   #               Main body              #
   ########################################
   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]

   n <- length(obs)

   # Computing the weights
   w <- whl(x=obs, lambda=lambda, lQ=lQ, hQ=hQ)
   
   denominator <- sum( ( abs(w*(obs - median(obs))) )^j )
     
   if (denominator != 0) {
      
     wNSeff5 <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else stop("'sum((w*(obs - mean(obs)))^j)=0' => it is not possible to compute 'wNSeff5'")  

   return(wNSeff5)

} # 'wNSeff5.default' end


wNSeff5.matrix <- function(sim, obs, j=2, lambda=0.5, 
                          lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                          hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                          ...){

  wNSeff5 <- rep(NA, ncol(obs))

  wNSeff5 <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSeff5[i] <- wNSeff5.default( x[,i], y[,i], j=j, lambda=lambda, 
                                              lQ=lQ, hQ=hQ, ... )
               }, x=sim, y=obs )

  names(wNSeff5) <- colnames(obs)

  return(wNSeff5)

} # 'wNSeff5.matrix' end


wNSeff5.data.frame <- function(sim, obs, j=2, lambda=0.5, 
                              lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                              hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                              ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSeff5.matrix(sim, obs, j=j, lambda=lambda, lQ=lQ, hQ=hQ, ...)

} # 'wNSeff5.data.frame' end
