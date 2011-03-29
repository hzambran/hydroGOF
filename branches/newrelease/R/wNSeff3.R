########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 26-Jan-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSeff3 <-function(sim, obs, ...) UseMethod("wNSeff3")

wNSeff3.default <- function(sim, obs, type="low", j=2, pbb=0, na.rm=TRUE, ...){

   ########################################
   # Weigthing function for HIGH flows    #
   ########################################  
   wh <- function(x, j=2, pbb=1) { 

       x    <- as.numeric(x)
       xmin <- min(x, na.rm=TRUE)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
         
       w <- ( (x-xmin) / xmax )^j
      
       # w=1 for all 'x' larger  or equal to 'xmax'
       index <- which(x >= xmax)
      
       w[index] <- 1 
       
       return( w )
     
   } # 'wh' END
   
   ########################################
   # Weigthing function for LOW flows     #
   ########################################
   wl <- function(x, j=2, pbb=0) { 
  
       x <- as.numeric(x)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
       xmin <- min(x, na.rm=TRUE) 
         
       w <- ( ( xmax - (x-xmin) ) / xmax )^j
       
       # w=1 for all 'x' smaller  or equal to 'xmin'
       index <- which(x >= xmax)
      
       w[index] <- 0
       
       return( w )
       
   } # 'wl' END


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
   
   if ( type=="high" ) {  
     w <- wh(x=obs, j=j, pbb=pbb)     
   } else if ( type=="low" ) {         
       w <- wl(x=obs, j=j, pbb=pbb)    
     } # ELSE end
   
   denominator <- sum( ( abs(w*(obs - mean(obs))) )^j )
     
   if (denominator != 0) {
      
     wNSeff3 <- 1 - ( sum( (abs( w * (obs - sim) ))^j ) / denominator )
     
   } else stop("'sum((abs(w*(obs - mean(obs))))^j)=0' => it is not possible to compute 'wNSeff3'")  

   return(wNSeff3)

} # 'wNSeff3.default' end


wNSeff3.matrix <- function(sim, obs, type="low", j=2, pbb=1, na.rm=TRUE, ...){

  wNSeff3 <- rep(NA, ncol(obs))

  wNSeff3 <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSeff3[i] <- wNSeff3.default( x[,i], y[,i], type=type, j=j, pbb=pbb, na.rm=na.rm, ...)
               }, x=sim, y=obs )

  names(wNSeff3) <- colnames(obs)

  return(wNSeff3)

} # 'wNSeff3.matrix' end


wNSeff3.data.frame <- function(sim, obs, type="low", j=2, pbb=1, na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSeff3.matrix(sim, obs, type=type, j=j, pbb=pbb, na.rm=na.rm, ...)

} # 'wNSeff3.data.frame' end
