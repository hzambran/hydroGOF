########################################
# Weigthing function for HIGH flows    #
########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 26-Jan-2011, JRC, Ispra     #
# Updates: 28-Jul-2011                 #
########################################
# Reference: Unpublished (yet)         #
########################################

wh <-function(x, ...) UseMethod("wh")

wh.default <- function(x, k=0.5, pbb=0.8, ...){

  x    <- as.numeric(x)
  xmin <- min(x, na.rm=TRUE)
  
  if (pbb < 0 | pbb > 1)
     stop("Invalid argument: 0 <= pbb <= 1")
  
  xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
         
  w <- ( (x-xmin) / xmax )^k
      
  # w=1 for all 'x' larger  or equal to 'xmax'
  index <- which(x >= xmax)
      
  w[index] <- 1 
       
  return( w )
     
} # 'wh' END


wh.matrix <- function(x, k=0.5, pbb=0.8, ...){

  wh        <- apply(x, MARGIN=2, wh.default, , k=k, pbb=pbb, ...)
  names(wh) <- colnames(x)
  return(wh)

} # 'wh.matrix' end


wh.data.frame <- function(x, k=0.5, pbb=0.8, ...){

  x <- as.matrix(x)
  wh.matrix(x, k=k, pbb=pbb, ...)

} # 'wh.data.frame' end


wh.zoo <- function(x, k=0.5, pbb=0.8, ...){

  x <- coredata(x)
  if ( is.matrix(x) | is.data.frame(x) ) {
    wh.matrix(x, k=k, pbb=pbb, ...)
  } else wh.default(x, k=k, pbb=pbb, ...)

} # 'wh.zoo' end
