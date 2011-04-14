########################################
# Started: April 2010, Trento          #
########################################
#         Weigthed RMSE                #
########################################

wrmse2 <-function(sim, obs, ...) UseMethod("wrmse2")

wrmse2.default <- function(sim, obs, type="high", j=2, pbb=1, na.rm=TRUE, ...){       
    
   ########################################
   # Weigthing function for HIGH flows    #
   ########################################  
   wh <- function(x, j=2, pbb=1) { 

       x <- as.numeric(x)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE)
       xmin <- min(x, na.rm=TRUE)
         
       w <- ( (x-xmin) / xmax )^j
      
       # w=1 for all 'x' larger  or equal to 'xmax'
       index <- which(x > xmax)
      
       w[index] <- 1 
       
       return( w )
     
   } # 'wh' END
   
   ########################################
   # Weigthing function for LOW flows     #
   ########################################
   wl <- function(x, j=2, pbb=1) { 
  
       x <- as.numeric(x)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       xmax <- quantile(x, probs=pbb, na.rm=TRUE)
       xmin <- min(x, na.rm=TRUE)
         
       w <- ( ( xmax - (x-xmin) ) / xmax )^j
       
       # Making that all the weights larger than one be qual to one
       index <- which(x > xmax)
      
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
          
   wrmse2 <- exp( j * log( (1/n) * sum( (w*(abs(sim - obs)))^j ) ) )     
     
   return(wrmse2)
     
} # 'wrmse2.default' end



wrmse2.matrix <- function(sim, obs, type="high", j=2, pbb=1, na.rm=TRUE, ...){ 
 
  wrmse2 <- rep(NA, ncol(obs))       
          
  wrmse2 <- sapply(1:ncol(obs), function(i,x,y) { 
                 wrmse2[i] <- wrmse2.default( x[,i], y[,i], type=type, j=j, 
                                            pbb=pbb, na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(wrmse2) <- colnames(obs)
  
  return(wrmse2)
     
} # 'wrmse2.matrix' end



wrmse2.data.frame <- function(sim, obs, type="high", j=2, pbb=1, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wrmse2.matrix(sim, obs, type=type, j=j, pbb=pbb, na.rm=na.rm, ...)
     
} # 'wrmse2.data.frame' end
