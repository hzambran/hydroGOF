########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 26-Jan-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSeff4 <-function(sim, obs, ...) UseMethod("wNSeff4")

wNSeff4.default <- function(sim, obs, type="low", j=0.5, wj=2, pbb=0, na.rm=TRUE, ...){

   ########################################
   # Weigthing function for HIGH flows    #
   ########################################  
   wh <- function(x, wj=2, pbb=1) { 

       x    <- as.numeric(x)
       xmin <- min(x, na.rm=TRUE)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
         
       w <- ( (x-xmin) / xmax )^wj
      
       # w=1 for all 'x' larger  or equal to 'xmax'
       index <- which(x >= xmax)
      
       w[index] <- 1 
       
       return( w )
     
   } # 'wh' END
   
   ########################################
   # Weigthing function for LOW flows     #
   ########################################
   wl <- function(x, wj=2, pbb=1) { 
  
       x <- as.numeric(x)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE) 
       xmin <- min(x, na.rm=TRUE) 
         
       w <- ( ( xmax - (x-xmin) ) / xmax )^wj
       
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
     w <- wh(x=obs, wj=wj, pbb=pbb)     
   } else if ( type=="low" ) {         
       w <- wl(x=obs, wj=wj, pbb=pbb)    
     } # ELSE end
   
   denominator <- sum( ( abs(w*(obs - median(obs))) )^j )
     
   if (denominator != 0) {
      
     wNSeff4 <- 1 - ( sum( (abs( w * (obs - sim) ) )^j ) / denominator )
     
   } else stop("'sum((abs(w*(obs - mean(obs))))^j)=0' => it is not possible to compute 'wNSeff4'")  

   return(wNSeff4)

} # 'wNSeff4.default' end


wNSeff4.matrix <- function(sim, obs, type="low", j=0.5, wj=2, pbb=1, na.rm=TRUE, ...){

  wNSeff4 <- rep(NA, ncol(obs))

  wNSeff4 <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSeff4[i] <- wNSeff4.default( x[,i], y[,i], type=type, j=j, wj=wj, pbb=pbb, na.rm=na.rm, ...)
               }, x=sim, y=obs )

  names(wNSeff4) <- colnames(obs)

  return(wNSeff4)

} # 'wNSeff4.matrix' end


wNSeff4.data.frame <- function(sim, obs, type="low", j=0.5, wj=2, pbb=1, na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSeff4.matrix(sim, obs, type=type, j=j, wj=wj, pbb=pbb, na.rm=na.rm, ...)

} # 'wNSeff4.data.frame' end
