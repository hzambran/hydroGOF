########################################
# Started: April 2010, Trento          #
########################################
#         Weigthed RMSE                #
########################################

wrmse <-function(sim, obs, ...) UseMethod("wrmse")

wrmse.default <- function(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...){       
    
   ########################################
   # Weigthing function for HIGH flows    #
   ########################################  
   wh <- function(x, j=2, pbb=0) { 

       x <- as.numeric(x)
       
       x.min <- min(x, na.rm=TRUE)
  
       if (pbb < 0 | pbb > 1)
         stop("Invalid argument: 0 <= pbb <= 1")
  
       if (pbb==0) {
       
         qmax <- max(x, na.rm=TRUE)
         
       } else {
       
          qmax <- quantile(x, probs=pbb, na.rm=TRUE)
          
         } # ELSE end
         
      w <- (x / qmax )^j - (x.min / qmax )^j
      
      # Making that all the weights larger than one be equal to one
      index <- which(w > 1)
      
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
  
       if (pbb==0) {
       
         qmax <- max(x, na.rm=TRUE)
         
       } else {
       
          qmax <- quantile(x, probs=pbb, na.rm=TRUE)
          
         } # ELSE end
         
       w <- ( (qmax-x) / qmax  )^j
       
       # Making that all the weights larger than one be qual to one
      index <- which(x > qmax)
      
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
          
   wrmse <- sqrt( (1/n) * sum( w * ( ( abs(sim - obs) ) )^j ) )     
     
   return(wrmse)
     
} # 'wrmse.default' end



wrmse.matrix <- function(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...){ 
 
  wrmse <- rep(NA, ncol(obs))       
          
  wrmse <- sapply(1:ncol(obs), function(i,x,y) { 
                 wrmse[i] <- wrmse.default( x[,i], y[,i], type=type, j=j, 
                                            pbb=pbb, na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(wrmse) <- colnames(obs)
  
  return(wrmse)
     
} # 'wrmse.matrix' end



wrmse.data.frame <- function(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wrmse.matrix(sim, obs, type=type, j=j, pbb=pbb, na.rm=na.rm, ...)
     
} # 'wrmse.data.frame' end
