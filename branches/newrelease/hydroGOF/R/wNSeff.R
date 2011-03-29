########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 22-Jan-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSeff <-function(sim, obs, ...) UseMethod("wNSeff")

wNSeff.default <- function(sim, obs, lambda=0.5, 
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

   w <- whl(x=obs, lambda=lambda, lQ=lQ, hQ=hQ)
   
   denominator <- sum( ( w* (obs - mean(obs)) )^2 )
     
   if (denominator != 0) {
      
     wNSeff <- 1 - ( sum( ( w * (obs - sim) )^2 ) / denominator )
     
   } else stop("'sum((w*(obs - mean(obs)))^2)=0' => it is not possible to compute 'wNSeff'")  

   return(wNSeff)

} # 'wNSeff.default' end


wNSeff.matrix <- function(sim, obs, lambda=0.5, 
                          lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                          hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                          ...){

  wNSeff <- rep(NA, ncol(obs))

  wNSeff <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSeff[i] <- wNSeff.default( x[,i], y[,i], lambda=lambda, 
                                              lQ=lQ, hQ=hQ, ... )
               }, x=sim, y=obs )

  names(wNSeff) <- colnames(obs)

  return(wNSeff)

} # 'wNSeff.matrix' end


wNSeff.data.frame <- function(sim, obs, lambda=0.5, 
                              lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                              hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                              ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSeff.matrix(sim, obs, lambda=lambda, lQ=lQ, hQ=hQ, ...)

} # 'wNSeff.data.frame' end
