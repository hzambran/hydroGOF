##################################################
# 'wKGE2': weighted Kling-Gupta Efficiency        #
##################################################
# Started: 03-Feb-2011                           #
##################################################
# The optimal value of wKGE2 is 1

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

wKGE2 <- function(sim, obs, ...) UseMethod("wKGE2")

wKGE2.default <- function(sim, obs, s=c(1,1,1), lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                         na.rm=TRUE, ...) { 

   ##############################################
   # Weigthing function for HIGH & LOW flows II #
   ##############################################
   # lambda: wheight that will be given to the high flows #
   whl <- function(x, lambda=0.5, 
                   lQ=quantile(x, na.rm=TRUE, probs=0.2), 
                   hQ=quantile(x, na.rm=TRUE, probs=0.8) ) {

      x <- as.numeric(x)

      n <- length(x)

      if (lambda < 0 | lambda > 1)
         stop("Invalid argument: 0 <= lambda <= 1")

      index.high <- which(x >= hQ )
      index.low  <- which(x <= lQ )
      index.med  <- which(x < hQ & x > lQ)

      w <- rep(NA, n)

      w[index.high] <- lambda
      w[index.low]  <- 1 - lambda
      w[index.med]  <- 1 - lambda + (x[index.med]-lQ)*(2*lambda-1)/(hQ-lQ)

      return( w )

   } # 'whl' END


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
     w <- whl(x=obs, lambda=lambda, lQ=lQ, hQ=hQ)

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

     # Computation of wKGE2
     if ( (mean.obs != 0) & (sigma.obs != 0) ) {
         wKGE2 <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(Alpha-1))^2 + (s[3]*(Beta-1))^2 )
     } else {
         if ( mean.obs != 0)  message("Warning: 'mean(obs)==0'. Beta = -Inf")
         if ( sigma.obs != 0) message("Warning: 'sd(obs)==0'. Beta = -Inf")
         wKGE2 <- -Inf
       } # ELSE end     
 
     return(wKGE2)
     
} # 'wKGE2.default' end


wKGE2.matrix <- function (sim, obs, s=c(1,1,1), lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                         na.rm=TRUE, ...){ 

  wKGE2 <- rep(NA, ncol(obs))       
          
  wKGE2 <- sapply(1:ncol(obs), function(i,x,y) { 
                 wKGE2[i] <- wKGE2.default( x[,i], y[,i], s=s, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(wKGE2) <- colnames(obs)
  return(wKGE2)
     
} # 'wKGE2.matrix' end


wKGE2.data.frame <- function (sim, obs, s=c(1,1,1), lambda=0.5, 
                             lQ=quantile(obs, na.rm=TRUE, probs=0.2), 
                             hQ=quantile(obs, na.rm=TRUE, probs=0.8), 
                             na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wKGE2.matrix(sim, obs, s=s, lambda=lambda, lQ=lQ, hQ=hQ, na.rm=na.rm, ... )
     
} # 'wKGE2.data.frame' end
