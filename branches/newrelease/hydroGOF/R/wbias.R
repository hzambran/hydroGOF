########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: April 2010, UT, Trento      #
# Updates: 20-Jan-2011, JRC Ispra      #
########################################
# Reference: Unpublished (yet)         #
########################################

wbias <-function(sim, obs, ...) UseMethod("wbias")

wbias.default <- function(sim, obs, lambda=0.5, 
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

   wbias <-  mean( w * (  (sim - obs) / obs  ) )

   return(wbias)

} # 'wbias.default' end


wbias.matrix <- function(sim, obs, lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.95), 
                         ...){

  wbias <- rep(NA, ncol(obs))

  wbias <- sapply(1:ncol(obs), function(i,x,y) {
                 wbias[i] <- wbias.default( x[,i], y[,i], lambda=lambda, 
                                            lQ=lQ, hQ=hQ, ... )
               }, x=sim, y=obs )

  names(wbias) <- colnames(obs)

  return(wbias)

} # 'wbias.matrix' end


wbias.data.frame <- function(sim, obs, lambda=0.5, 
                              lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                              hQ=quantile(obs, na.rm=TRUE, probs=0.95), 
                              ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wbias.matrix(sim, obs, lambda=lambda, lQ=lQ, hQ=hQ, ...)

} # 'wbias.data.frame' end
