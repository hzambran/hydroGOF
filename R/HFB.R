# File HFB.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2024-2025 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'HFB': median annual High Flow Bias                                          #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################

# The high flow bias (HFB) ranges from 0 to Inf, with an optimal value of 0. 
# Higher values of HFB indicate stronger differences between the high values 
# of \code{sim} and \code{obs}, Essentially, the closer to 0, the more similar 
# the high values of \code{sim} and \code{obs} are. 

# The median annual high-flows bias (HFB) is designed to drive the calibration of 
# hydrological models focused in the reproduction of high-flow events.

# It is inspired in the annual peak-flow bias (APFB) objective function 
# proposed by Mizukami et al. (2019). However, it has four important diferences:
# 1) instead of considering only the observed annual peak flow in each year,
#    it considers all the high flows in each year, where "high flows" are all 
#    the values above a user-defined probability of exceedence of the observed 
#    values, by default 0.1 (\code{hQ.thr=0.1}).
# 2) insted of considering only the simulated high flows for each 
#    year, which might occur in a date/time different from the date in which  
#    occurs the observed annual peak flow, it considers as many high simulated 
#    flows as the number of high observed flows for each year, each one in the 
#    exact same date/time in which the corresponding observed high flow occurred.
# 3) for each year, instead of using a single bias value (i.e., the bias in the 
#    annual peak flow), it uses the median of all the bias in the user-defined 
#    high flows 
# 4) when computing the final value of this metric, instead o using the mean of 
#    the annual values, it uses the median, in order to take a stronger 
#    representation of the bias when its distribution is not symetric.

# 'hQ.thr'     : numeric, representing the exceedence probabiliy used to identify 
#                high flows in \code{obs}. All values in \code{obs} that are equal or 
#                higher than \code{quantile(obs, hQ.thrs=hQ.thr)} are considered as 
#                high flows.\cr
#                On the other hand, the high values in \code{sim} are those located at 
#                the same i-th position than the i-th value of \code{obs} deemed as 
#                high flows. 
# 'start.month': [OPTIONAL]. Only used when the (hydrological) year of interest is 
#                different from the calendar year.
#                numeric in [1:12] indicating the starting month of the (hydrological) 
#                year. Numeric values in [1, 12] represent months in [January, December]. 
#                By default \code{start.month=1}.
# 'out.PerYear': logical, indicating whether the output of this function has to include 
#                the median annual high-flows bias obtained for the individual years in 
#                \code{sim} and \code{obs} or not.

# Ref:
# Mizukami, N., Rakovec, O., Newman, A. J., Clark, M. P., Wood, A. W., 
# Gupta, H. V., and Kumar, R.: (2019). On the choice of calibration metrics for 
# "high-flow" estimation using hydrologic models, Hydrology Earth System 
# Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019.

HFB <- function(sim, obs, ...) UseMethod("HFB")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
HFB.default <- function(sim, obs, na.rm=TRUE, 
                        hQ.thr=0.1,
                        start.month=1, out.PerYear=FALSE,
                        fun=NULL,
                        ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA
                        ) { 

  # Function to compute the high flow bias in a single year
  lHFB.year <- function(i,          # index corresponding to the position of the unique year being analised (i in [1, nyears])
                        lsim,       # list where each element has all the individual simulated values during year 'i'
                        lobs,       # list where each element has all the individual observed values during year 'i'
                        lhQ.val,    # numeric, representing the streamflow value (in m3/s) used to identify high flows in \code{obs}
                        lna.rm=TRUE
                        ) {

    # Selecting all the individual simulated and observed values during year 'i'
    llsim          <- lsim[[i]]
    llobs          <- lobs[[i]]

    # Selecting only high values in 'obs'
    llobs.high.pos <- which(llobs >= lhQ.val)
    llobs.high     <- llobs[llobs.high.pos]

    # Selecting values in 'sim' that have the same temporal location than the high values in 'obs'
    llsim.high     <- llsim[llobs.high.pos]

    # Getting the HFB for the year 'i'. Its ideal value is 1
    #lHFB  <- 1 - sqrt( stats::median( abs(llsim.high / llobs.high - 1), na.rm=na.rm )^2 )
    lHFB  <- 1 - sqrt( (stats::median( llsim.high / llobs.high, na.rm=na.rm ) - 1)^2 )

    return(lHFB)
  } #'lHFB.year' END
   

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

  if ( (hQ.thr < 0) | (hQ.thr > 1) | 
       (length(hQ.thr) > 1) | (length(hQ.thr) == 0) ) {
    hQ.val <- NA
    stop("Invalid argument: 'hQ.thr' must be a single number in [0, 1] !")
  } # IF end 

  
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

  hQ.val <- stats::quantile(obs, probs=(1-hQ.thr)) # To use 'lhQ.thr' as pbb of exceedence instead of as a quantile  
  #message("hQ.val= ", round(hQ.val,3) )

  # Checking that time of sim' and 'obs' are the same
  sim.ltime <- time(sim)
  obs.ltime <- time(obs)
  if ( !all.equal(sim.ltime, obs.ltime ) ) 
    stop("Invalid argument: 'sim' and 'obs' must have the same time attribute !")


  # Getting annual values of 'sim' and 'obs'
  years  <- format(obs.ltime, "%Y") # or 'ltimes <- sim.ltime', because 'sim.ltime' is the same as 'obs.ltime'

  # Shifting backwards the year each element in 'x', 
  # only when start.month != 1
  ltimes <- obs.ltime # or 'ltimes <- sim.ltime', because 'sim.ltime' is the same as 'obs.ltime'
  if ( start.month != 1 )
    years <- .shiftyears(ltime=ltimes, lstart.month=start.month)

  # Getting a numeric vector with the unique years in 'sim' and 'obs'
  years.unique <- unique(years)

  # Getting the number of years in 'sim' and 'obs'
  nyears <- length(years.unique)

  # Getting a list of 'sim' and 'obs' values for each year
  sim.PerYear <- split(coredata(sim), years)
  obs.PerYear <- split(coredata(obs), years) # years.sim == years.obs

  # Computing annual HFB values
  HFB.yr        <- sapply(1:nyears, FUN=lHFB.year, lsim=sim.PerYear, lobs=obs.PerYear, 
                          lhQ.val=hQ.val, lna.rm= na.rm)
  names(HFB.yr) <- years.unique

  # ideal value of beta is 1
  beta <- stats::median(HFB.yr, na.rm=na.rm)

  # ideal value of HFB is 1
  HFB <- 1 - sqrt( (beta - 1)^2 ) 

  if (out.PerYear) {
    out <- list(HFB.value=HFB, HFB.PerYear=HFB.yr)
  } else out <- HFB
    
  return(out)
} # 'HFB.default' END
#HFB.default(sim,sim, hQ.thr=0.1)
#HFB.default(sim,obs, hQ.thr=0.20)
#HFB.default(sim,obs, hQ.thr=0.10)
#HFB.default(sim,obs, hQ.thr=0.05)
#HFB.default(sim,obs, hQ.thr=0.01)


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates:                                                                     #
################################################################################

HFB.matrix <- function(sim, obs, na.rm=TRUE, 
                       hQ.thr=0.1,
                       start.month=1, out.PerYear=FALSE, 
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


  HFB                <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=nyears, ncol=ncol(obs))
  rownames(elements) <- unique(years.obs)
  colnames(elements) <- colnames(obs)


  out.single <- sapply(1:ncol(obs), function(i,x,y) { 
                  HFB[i] <- HFB.default( x[,i], y[,i], na.rm=na.rm, 
                                         hQ.thr=hQ.thr,  
                                         start.month=start.month, 
                                         out.PerYear=out.PerYear, 
                                         fun=fun, 
                                         epsilon.type=epsilon.type, 
                                         epsilon.value=epsilon.value )[[1]]
                 }, x=sim, y=obs ) 
  names(out.single) <- names(obs)


  if (out.PerYear) {        
    out.yr <- sapply(1:ncol(obs), function(i,x,y) { 
                    elements[,i] <- HFB.default( x[,i], y[,i], na.rm=na.rm, 
                                                 hQ.thr=hQ.thr,  
                                                 start.month=start.month, 
                                                 out.PerYear=out.PerYear, 
                                                 fun=fun, 
                                                 epsilon.type=epsilon.type, 
                                                 epsilon.value=epsilon.value )[[2]]
                   }, x=sim, y=obs ) 
 
    colnames(out.yr) <- names(obs)                     
  } # IF end

  if (out.PerYear) {
    out <- list(HFB.value=out.single, HFB.PerYear=out.yr)
  } else out <- out.single
  
  return(out)
     
} # 'HFB.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
HFB.data.frame <- function(sim, obs, na.rm=TRUE, 
                           hQ.thr=0.1,
                           start.month=1, out.PerYear=FALSE, 
                           fun=NULL,
                           ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
   
  HFB.matrix(sim, obs, na.rm=na.rm, hQ.thr=hQ.thr, 
             start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'HFB.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates:                                                                     #
################################################################################
HFB.zoo <- function(sim, obs, na.rm=TRUE, 
                    hQ.thr=0.1,
                    start.month=1, out.PerYear=FALSE, 
                    fun=NULL,
                    ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA) { 

  #sim <- zoo::coredata(sim)
  #if (is.zoo(obs)) obs <- zoo::coredata(obs)    

  if (is.matrix(sim) | is.data.frame(sim)) {
    HFB.matrix(sim, obs, na.rm=na.rm, hQ.thr=hQ.thr, 
                 start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm, hQ.thr=hQ.thr, 
                    start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'HFB.zoo' end
