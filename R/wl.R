########################################
# Weigthing function for LOW flows     #
########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 26-Jan-2011, JRC, Ispra     #
# Updates: 28-Jul-2011                 #
########################################
# Reference: Unpublished (yet)         #
########################################

wl <-function(x, ...) UseMethod("wl")

wl.default <- function(x, k=0.5, pbb=0.8, ...) {
  
   x <- as.numeric(x)
  
   if (pbb < 0 | pbb > 1)
      stop("Invalid argument: 0 <= pbb <= 1")
  
   xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
   xmin <- min(x, na.rm=TRUE) 
         
   w <- ( ( xmax - (x-xmin) ) / xmax )^k
       
   # w=1 for all 'x' smaller  or equal to 'xmin'
   index <- which(x >= xmax)
      
   w[index] <- 0
       
   return( w )
       
} # 'wl' END



wl.matrix <- function(x, k=0.5, pbb=0.8, ...){

  wl        <- apply(x, MARGIN=2, wl.default, , k=k, pbb=pbb, ...)
  names(wl) <- colnames(x)
  return(wl)

} # 'wl.matrix' end


wl.data.frame <- function(x, k=0.5, pbb=0.8, ...){

  x <- as.matrix(x)
  wl.matrix(x, k=k, pbb=pbb, ...)

} # 'wl.data.frame' end


wl.zoo <- function(x, k=0.5, pbb=0.8, ...){

  x <- coredata(x)
  if ( is.matrix(x) | is.data.frame(x) ) {
    wl.matrix(x, k=k, pbb=pbb, ...)
  } else wl.default(x, k=k, pbb=pbb, ...)

} # 'wl.zoo' end
