# File APFB.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2024-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'APFB': Annual peak-flows bias                                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
#          02-May-2025 (EGU 2025) ; 03-May-2025                                #
################################################################################
# The optimal value of APFB is 0

# The annual peak-flow bias (APFB) objective function was proposed by 
# Mizukami et al. (2019) to drive the calibration of hydrological models focused
# in the reproduction of high-flow events.

# Ref:
# Mizukami, N., Rakovec, O., Newman, A. J., Clark, M. P., Wood, A. W., 
# Gupta, H. V., and Kumar, R.: (2019). On the choice of calibration metrics for 
# "high-flow" estimation using hydrologic models, Hydrology Earth System 
# Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019.

APFB <- function(sim, obs, ...) UseMethod("APFB")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
APFB.default <- function(sim, obs, na.rm=TRUE, 
                         start.month=1,
                         fun=NULL,
                         ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA
                         ) { 


  
   

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

  # Selecting only valid paris of values
  vi <- valindex(sim, obs)     
  if (length(vi) > 0) {	 
    obs <- obs[vi]
    sim <- sim[vi]

    # Applying 'fun' to each value in 'obs' and 'sim'
    if (!is.null(fun)) {
      fun <- match.fun(fun)
      new <- preproc(sim=sim, obs=obs, fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim <- new[["sim"]]
      obs <- new[["obs"]]
    } # IF end
  } else stop("There are no points with simultaneous values of 'sim' and 'obs' !!")


  # Getting the observed and simulated annual maximum for each year
  obs.annual.max <- hydroTSM::daily2annual(obs, FUN=max, na.rm=na.rm, start.month=start.month)
  sim.annual.max <- hydroTSM::daily2annual(sim, FUN=max, na.rm=na.rm, start.month=start.month)

  # Getting the mean of the observed annual maximum and simulated annual maximum, respectively 
  mu.obs.annual.max <- mean(obs.annual.max, na.rm=na.rm)
  mu.sim.annual.max <- mean(sim.annual.max, na.rm=na.rm)

  # Computing the APFB value
  APFB <- sqrt( ( mu.obs.annual.max / mu.sim.annual.max - 1)^2 )
    
  return(APFB)
} # 'APFB.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 03-May-2025                                                         #
################################################################################

APFB.matrix <- function(sim, obs, na.rm=TRUE, 
                        start.month=1, 
                        fun=NULL,
                        ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA) { 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
     stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
           paste(dim(sim), collapse=" "), "] != [", 
           paste(dim(obs), collapse=" "), "] )", sep="") )
           

  # Annual index for 'sim' and 'obs'
  dates.sim  <- time(sim)
  dates.obs  <- time(obs)
  years.sim  <- format( dates.sim, "%Y")
  years.obs  <- format( dates.obs, "%Y")
  if (!all.equal(years.sim, years.obs)) {
    stop("Invalid argument: 'sim' and 'obs' must have the same dates !")
  } 
  nyears <- length(unique(years.obs))


  APFB               <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=nyears, ncol=ncol(obs))
  rownames(elements) <- unique(years.obs)
  colnames(elements) <- colnames(obs)


  out.single <- sapply(1:ncol(obs), function(i,x,y) { 
                       APFB[i] <- APFB.default( x[,i], y[,i], na.rm=na.rm, 
                                                start.month=start.month, 
                                                fun=fun, 
                                                epsilon.type=epsilon.type, 
                                                epsilon.value=epsilon.value )[[1]]
                 }, x=sim, y=obs ) 
  names(out.single) <- names(obs)
 
  
  return(out.single)
     
} # 'APFB.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
#          03-May-2025                                                         #
################################################################################
APFB.data.frame <- function(sim, obs, na.rm=TRUE, 
                           start.month=1, 
                           fun=NULL,
                           ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
   
  APFB.matrix(sim, obs, na.rm=na.rm, 
             start.month=start.month, fun=fun, ...,  
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'APFB.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 03-May-2025                                                         #
################################################################################
APFB.zoo <- function(sim, obs, na.rm=TRUE, 
                    start.month=1, 
                    fun=NULL,
                    ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA) { 

  #sim <- zoo::coredata(sim)
  #if (is.zoo(obs)) obs <- zoo::coredata(obs)    

  if (is.matrix(sim) | is.data.frame(sim)) {
    APFB.matrix(sim, obs, na.rm=na.rm, 
                 start.month=start.month, fun=fun, ...,  
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm,
                    start.month=start.month, fun=fun, ...,  
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'APFB.zoo' end
