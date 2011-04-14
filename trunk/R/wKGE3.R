##################################################
# 'wKGE3': weighted Kling-Gupta Efficiency        #
##################################################
# Started: 03-Feb-2011                           #
##################################################
# The optimal value of wKGE3 is 1

# Ref:
# Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez, 
# Decomposition of the mean squared error and NSE performance criteria: 
# Implications for improving hydrological modelling, 
# Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91, 
# DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694, 

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.

# 'Result': Kling-Gupta Efficiency between 'sim' and 'obs'

wKGE3 <- function(sim, obs, ...) UseMethod("wKGE3")

wKGE3.default <- function(sim, obs, s=c(1,1,1), type="low", j=0.5, pbb=0.8, na.rm=TRUE, ...) { 

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
   wl <- function(x, j=0.5, pbb=1) { 
  
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

     # If the user provided a value for 's'
     if (!all.equal(s, c(1,1,1)) )  {
       if ( length(s) != 3 ) stop("Invalid argument: lenght(s) must be equal to 3 !")
       if ( sum(s) != 1 )    stop("Invalid argument: sum(s) must be equal to 1.0 !")
     } # IF end

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      
   
     vi <- valindex(sim, obs)
	 
     obs <- obs[vi]
     sim <- sim[vi]

     # Computing the weights
     if ( type=="high" ) {  
       w <- wh(x=obs, j=j, pbb=pbb)     
     } else if ( type=="low" ) {         
         w <- wl(x=obs, j=j, pbb=pbb)    
       } # ELSE end

     # Mean values
     mean.sim <- mean(w*sim, na.rm=na.rm)
     mean.obs <- mean(w*obs, na.rm=na.rm)

     # Standard deviations
     sigma.sim <- sd(w*sim, na.rm=na.rm)
     sigma.obs <- sd(w*obs, na.rm=na.rm)
 
     # Pearson product-moment correlation coefficient
     r     <- .rPearson(w*sim, w*obs)

     # Alpha is a measure of relative variability in the simulated and observed values
     Alpha <- sigma.sim / sigma.obs

     # Beta is the ratio between the mean of the simulated values and the mean ob the observed ones
     Beta <- mean.sim / mean.obs

     # Computation of wKGE3
     if ( (mean.obs != 0) & (sigma.obs != 0) ) {
         wKGE3 <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(Alpha-1))^2 + (s[3]*(Beta-1))^2 )
     } else {
         if ( mean.obs != 0)  message("Warning: 'mean(obs)==0'. Beta = -Inf")
         if ( sigma.obs != 0) message("Warning: 'sd(obs)==0'. Beta = -Inf")
         wKGE3 <- -Inf
       } # ELSE end     
 
     return(wKGE3)
     
} # 'wKGE3.default' end


wKGE3.matrix <- function (sim, obs, s=c(1,1,1), type="low", j=0.5, pbb=0.8, na.rm=TRUE, ...){ 

  wKGE3 <- rep(NA, ncol(obs))       
          
  wKGE3 <- sapply(1:ncol(obs), function(i,x,y) { 
                 wKGE3[i] <- wKGE3.default( x[,i], y[,i], s=s, type=type, j=j, pbb=pbb, na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(wKGE3) <- colnames(obs)
  return(wKGE3)
     
} # 'wKGE3.matrix' end


wKGE3.data.frame <- function (sim, obs, s=c(1,1,1), type="low", j=0.5, pbb=0.8, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wKGE3.matrix(sim, obs, s=s, type=type, j=j, pbb=pbb, na.rm=na.rm, ...)
     
} # 'wKGE3.data.frame' end
