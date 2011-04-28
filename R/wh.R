########################################
# Weigthing function for HIGH flows    #
########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 26-Jan-2011, JRC, Ispra     #
# Updates:                             #
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

  wh <- x*NA

  wh <- sapply(1:ncol(x), function(i,y) {
                 wh[,i] <- wh.default( y[,i], k=k, pbb=pbb, ...)
               }, y=x)

  names(wh) <- colnames(x)

  return(wh)

} # 'wh.matrix' end


wh.data.frame <- function(x, k=0.5, pbb=0.8, ...){

  x <- as.matrix(x)

  wh.matrix(x, k=k, pbb=pbb, ...)

} # 'wh.data.frame' end
