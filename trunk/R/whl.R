########################################################
# Weigthing function for HIGH & LOW flows II           #
########################################################
# lambda: wheight that will be given to the high flows #
########################################################
# Author: Mauricio Zambrano-Bigiarini                  #
########################################################
# Started: 04-Feb-2011, JRC, Ispra                     #
# Updates:                                             #
########################################################
# Reference: Unpublished (yet)                         #
########################################################

whl <-function(x, ...) UseMethod("whl")

whl.default <- function(x, lambda=0.5, 
                        lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                        hQ=quantile(x, probs=0.8, na.rm=TRUE), ... ) {

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


whl.matrix <- function(x, lambda=0.5, 
                       lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                       hQ=quantile(x, probs=0.8, na.rm=TRUE), ...) {

  whl <- apply(x, MARGIN=2, whl.default, lambda=lambda, lQ=lQ, hQ=hQ, ...)

  names(whl) <- colnames(x)

  return(whl)

} # 'whl.matrix' end


whl.data.frame <- function(x, lambda=0.5, 
                           lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                           hQ=quantile(x, probs=0.8, na.rm=TRUE), ...){

  x <- as.matrix(x)

  whl.matrix(x, lambda=lambda, lQ=lQ, hQ=hQ, ...)

} # 'whl.data.frame' end
