##################################################
# 'wKGE': weighted Kling-Gupta Efficiency        #
##################################################
# Started: 03-Feb-2011                           #
##################################################
# The optimal value of wKGE is 1

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

wKGE <- function(sim, obs, ...) UseMethod("wKGE")

wKGE.default <- function(sim, obs, s=c(1,1,1), j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...) { 

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
        is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")
     
   if ( is.na(match(w, c("wl", "wh", "whl"))) )
      stop("Invalid argument: 'w' have to be in: c('wl', 'wh', 'whl')")
      
   # Checking 'j'
   if (j < 0)
      stop("Invalid argument: 'j' must be positive !")
   
   # Checking 'k'
   if (k < 0)
      stop("Invalid argument: 'k' must be positive !")    
   

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]

   n <- length(obs)
   
   if (w=="wl") {
     w <- wl.default(x=obs, k=k, pbb=pbb, ...)
   } else if (w=="wh") {
       w <- wh.default(x=obs, k=k, pbb=pbb, ...)
     } else if (w=="whl") {
       w <- whl.default(x=obs, lambda=lambda, lQ=lQ, hQ=hQ, ... )
       } # ELSE end
    
   # Applying th weights to 'sim' and 'obs'    
   sim <- w*sim
   obs <- w*obs

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

   # Computation of wKGE
   if ( (mean.obs != 0) & (sigma.obs != 0) ) {
       wKGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(Alpha-1))^2 + (s[3]*(Beta-1))^2 )
   } else {
       if ( mean.obs != 0)  message("Warning: 'mean(obs)==0'. Beta = -Inf")
       if ( sigma.obs != 0) message("Warning: 'sd(obs)==0'. Beta = -Inf")
       wKGE <- -Inf
     } # ELSE end     
 
   return(wKGE)
     
} # 'wKGE.default' end


wKGE.matrix <- function (sim, obs, s=c(1,1,1), j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){ 
                         
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  wKGE <- rep(NA, ncol(obs))

  wKGE <- sapply(1:ncol(obs), function(i,x,y) {
                 wKGE[i] <- wKGE.default( x[,i], y[,i], s=s, j=j, w=w, k=k, pbb=pbb,
                                          lambda=lambda, lQ=lQ, hQ=hQ, ... )
               }, x=sim, y=obs )

  names(wKGE) <- colnames(obs)

  return(wKGE)
     
} # 'wKGE.matrix' end


wKGE.data.frame <- function (sim, obs, s=c(1,1,1), j=0.5, 
                         w="wl", k=0.5, pbb=0.8, 
                         lambda=0.5, 
                         lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                         hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wKGE.matrix(sim, obs, s=s, j=j, w=w, k=k, pbb=pbb, lambda=lambda, lQ=lQ, hQ=hQ, ... )
     
} # 'wKGE.data.frame' end
