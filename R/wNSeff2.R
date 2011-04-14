########################################
# Author: Mauricio Zambrano-Bigiarini  #
########################################
# Started: 25-Jan-2011, JRC, Ispra     #
# Updates:                             #
########################################
# Reference: Unpublished (yet)         #
########################################

wNSeff2 <-function(sim, obs, ...) UseMethod("wNSeff2")

wNSeff2.default <- function(sim, obs, type="low", j=2, pbb=0, na.rm=TRUE, ...){

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
  
       if (pbb<0 | pbb> 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmin <- quantile(x, probs=pbb, na.rm=TRUE)   
         
       if (xmin != 0) {
         w <- ( xmin / x  )^j
       } else {
           nona.index <- which(x!=xmin)
           w <- numeric(n)
           w[nona.index]      <- ( xmin / x  )^j
           w[which(x==xmin) ] <- 1
         } # ELSE end
       
       # w=1 for all 'x' smaller  or equal to 'xmin'
       index <- which(x <= xmin)
      
       w[index] <- 1 
       
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
   
   denominator <- sum( ( w* (obs - mean(obs)) )^2 )
     
   if (denominator != 0) {
      
     wNSeff2 <- 1 - ( sum( ( w * (obs - sim) )^2 ) / denominator )
     
   } else stop("'sum((w*(obs - mean(obs)))^2)=0' => it is not possible to compute 'wNSeff2'")  

   return(wNSeff2)

} # 'wNSeff2.default' end


wNSeff2.matrix <- function(sim, obs, type="low", j=2, pbb=1, na.rm=TRUE, ...){

  wNSeff2 <- rep(NA, ncol(obs))

  wNSeff2 <- sapply(1:ncol(obs), function(i,x,y) {
                 wNSeff2[i] <- wNSeff2.default( x[,i], y[,i], type=type, j=j, pbb=pbb, na.rm=na.rm, ...)
               }, x=sim, y=obs )

  names(wNSeff2) <- colnames(obs)

  return(wNSeff2)

} # 'wNSeff2.matrix' end


wNSeff2.data.frame <- function(sim, obs, type="low", j=2, pbb=1, na.rm=TRUE, ...){

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSeff2.matrix(sim, obs, type=type, j=j, pbb=pbb, na.rm=na.rm, ...)

} # 'wNSeff2.data.frame' end
