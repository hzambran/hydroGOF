########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 21-Jan-2011, JRC Ispra      #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wabias <-function(sim, obs, ...) UseMethod("wabias")

wabias.default <- function(sim, obs, lambda=0.5, 
                           lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                           hQ=quantile(obs, na.rm=TRUE, probs=0.95), 
                           ...){

   ##############################################
   # Weigthing function for HIGH & LOW flows II #
   ##############################################
   # lambda: wheight that will be given to the high flows #
   whl <- function(x, lambda=0.5, 
                   lQ=quantile(x, na.rm=TRUE, probs=0.05), 
                   hQ=quantile(x, na.rm=TRUE, probs=0.95) ) {

      x <- as.numeric(x)

      n <- length(x)

      if (lambda<0 | lambda> 1)
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

   wabias <-  mean( w * (  abs(sim - obs) / obs  ) )

   return(wabias)

} # 'wabias.default' end


wabias.matrix <- function(sim, obs, lambda=0.5, 
                          lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                          hQ=quantile(obs, na.rm=TRUE, probs=0.95), 
                          ...){
                          
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  wabias <- rep(NA, ncol(obs))

  wabias <- sapply(1:ncol(obs), function(i,x,y) {
                 wabias[i] <- wabias.default( x[,i], y[,i], lambda=lambda, 
                                              lQ=lQ, hQ=hQ, ... )
               }, x=sim, y=obs )

  names(wabias) <- colnames(obs)

  return(wabias)

} # 'wabias.matrix' end


wabias.data.frame <- function(sim, obs, lambda=0.5, 
                              lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                              hQ=quantile(obs, na.rm=TRUE, probs=0.95), 
                              ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wabias.matrix(sim, obs, lambda=lambda, lQ=lQ, hQ=hQ, ...)

} # 'wabias.data.frame' end
