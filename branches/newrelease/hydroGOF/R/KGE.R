##################################################
# 'KGE': Kling-Gupta Efficiency                  #
##################################################
# Started: 18-Jan-2011                           #
##################################################
# The optimal value of KGE is 1

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

KGE <- function(sim, obs, ...) UseMethod("KGE")

KGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, ...) { 

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

     # Mean values
     mean.sim <- mean(sim, na.rm=na.rm)
     mean.obs <- mean(obs, na.rm=na.rm)

     # Standard deviations
     sigma.sim <- sd(sim, na.rm=na.rm)
     sigma.obs <- sd(obs, na.rm=na.rm)
 
     # Pearson product-moment correlation coefficient
     r     <- .rPearson(sim, obs)

     # Alpha is a measure of relative variability in the simulated and observed values
     Alpha <- sigma.sim / sigma.obs

     # Beta is the ratio between the mean of the simulated values and the mean ob the observed ones
     Beta <- mean.sim / mean.obs

     # Computation of KGE
     if ( (mean.obs != 0) | (sigma.obs != 0) ) {
         KGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(Alpha-1))^2 + (s[3]*(Beta-1))^2 )
     } else {
         if ( mean.obs != 0)  message("Warning: 'mean(obs)==0'. Beta = -Inf")
         if ( sigma.obs != 0) message("Warning: 'sd(obs)==0'. Beta = -Inf")
         KGE <- -Inf
       } # ELSE end     
 
     return(KGE)
     
} # 'KGE.default' end


KGE.matrix <- function (sim, obs, s=c(1,1,1), na.rm=TRUE, ...){ 

  KGE <- rep(NA, ncol(obs))       
          
  KGE <- sapply(1:ncol(obs), function(i,x,y) { 
                 KGE[i] <- KGE.default( x[,i], y[,i], s=s, na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(KGE) <- colnames(obs)
  return(KGE)
     
} # 'KGE.matrix' end


KGE.data.frame <- function (sim, obs, s=c(1,1,1), na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  KGE.matrix(sim, obs, s=s, na.rm=na.rm, ...)
     
} # 'KGE.data.frame' end
