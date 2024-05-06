# File HFB.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'HFB': Median Annual high flow bias                                          #
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
# proposed by Mizukami et al. (2019).
# However, it has four important diferences:
# 1) instead of considering only the observed annual peak flow in each year,
#    it considers all the high flows in each year, where "high flows" are all 
#    the values above a user-defined quantile of the observed values, 
#    by default 0.9 (\code{prob=0.9}).
# 2) insted of considering only the simulated high flows for each 
#    year, which might occur in a date/time different from the date in which  
#    occurs the observed annual peak flow, it considers as many high simulated 
#    flows as the number of high observed flows for each year, each one in the 
#    exact same date/time in which the corresponding observed high flow occurred.
# 3) for each year, instead of using a single bias value (i.e., the bias in the 
#    single annual peak flow), it uses the median of all the bias in the 
#    user-defined high flows 
# 4) when computing the final value of this metric, instead o using the mean of 
#    the annual values, it uses the median, in order to take a stronger 
#    representation of the bias when its distribution is not symetric.

# 'prob'       : numeric, representing the non-exceedence probabiliy used to identify 
#                high flows in \code{obs}. All values in \code{obs} that are equal or 
#                higher than \code{quantile(obs, probs=prob)} are considered as 
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
# Sciences 23, 2601–2614, doi:10.5194/hess-23-2601-2019.

HFB <- function(sim, obs, ...) UseMethod("HFB")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
HFB.default <- function(sim, obs, na.rm=TRUE, 
                        prob=0.9,
                        start.month=1, out.PerYear=FALSE,
                        fun=NULL,
                        ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA
                        ) { 

  lHFB <- function(i, lsim, lobs, lprob, lna.rm=TRUE) {

    llobs            <- lobs[[i]]
    llsim            <- lsim[[i]]
    lquant           <- stats::quantile(llobs, probs=lprob)
    llobs.high.index <- which(llobs >= lquant)
    llobs.high       <- llobs[llobs.high.index]
    llsim.high       <- llsim[llobs.high.index]

    out <- stats::median( sqrt((llsim.high/llobs.high - 1)^2), na.rm=lna.rm)
    return(out)
  } #'lHFB' END


  # Function for shifting a time vector by 'nmonths' number of months.
  .shiftyears <- function(ltime,       # Date/POSIX* object. It MUST contat MONTH and YEAR
                          lstart.month # numeric in [2,..,12], representing the months. 2:Feb, 12:Dec
                          ) {
     syears.bak        <- as.numeric(format( ltime, "%Y" ))
     syears            <- syears.bak
     smonths           <- as.numeric(format( ltime, "%m"))
     months2moveback   <- 1:(lstart.month-1)
     N                 <- length(months2moveback)
     for (i in 1:N) {
       m.index         <- which(smonths == months2moveback[i])
       m.year          <- unique(na.omit(syears.bak[m.index]))
	     m.year          <- m.year - 1
	     syears[m.index] <- m.year
     } # FOR end
     return(syears)
  } # '.shiftyears' END
   

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

  if ( (prob < 0) | (prob > 1) | 
       (length(prob) > 1) | (length(prob) == 0) )
    stop("Invalid argument: 'prob' must be a single number in [0, 1] !")

  # Selecting only valid paris of values
  vi <- valindex(sim, obs)     
  if (length(vi) > 0) {	 
    obs <- obs[vi]
    sim <- sim[vi]

    if (!is.null(fun)) {
      fun <- match.fun(fun)
      new <- preproc(sim=sim, obs=obs, fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim <- new[["sim"]]
      obs <- new[["obs"]]
    } # IF end
  } else stop("There are no points with simultaneous values of 'sim' and 'obs' !!")

  # Annual index for 'x'
  dates.sim  <- time(sim)
  dates.obs  <- time(obs)
  years.sim  <- format( dates.sim, "%Y")
  years.obs  <- format( dates.obs, "%Y")
  if (!all.equal(years.sim, years.obs)) {
    stop("Invalid argument: 'sim' and 'obs' must have the same dates !")
  } else {

      if (start.month !=1) 
        years.obs <- .shiftyears(dates.obs, start.month)

      years.unique <- unique(years.obs)
      nyears       <- length(years.unique)
    } # ELSE end


  # Getting a list of 'sim' and 'obs' values for each year
  sim.PerYear <- split(coredata(sim), years.obs)
  obs.PerYear <- split(coredata(obs), years.obs) # years.sim == years.obs


  # Computing annual HFB values
  HFB.yr <- sapply(1:nyears, FUN=lHFB, lsim=sim.PerYear, lobs=obs.PerYear, 
                   lprob=prob, lna.rm= na.rm)
  names(HFB.yr) <- years.unique

  HFB <- stats::median(HFB.yr, na.rm=na.rm)

  if (out.PerYear) {
    out <- list(HFB.value=HFB, HFB.PerYear=HFB.yr)
  } else out <- HFB
    
  return(out)
} # 'HFB.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates:                                                                     #
################################################################################

HFB.matrix <- function(sim, obs, na.rm=TRUE, 
                       prob=0.9,
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
                                         prob=prob,  
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
                                                 prob=prob,  
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
                           prob=0.9,
                           start.month=1, out.PerYear=FALSE, 
                           fun=NULL,
                           ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
   
  HFB.matrix(sim, obs, na.rm=na.rm, prob=prob, 
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
                    prob=0.9,
                    start.month=1, out.PerYear=FALSE, 
                    fun=NULL,
                    ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA) { 

  #sim <- zoo::coredata(sim)
  #if (is.zoo(obs)) obs <- zoo::coredata(obs)    

  if (is.matrix(sim) | is.data.frame(sim)) {
    HFB.matrix(sim, obs, na.rm=na.rm, prob=prob, 
                 start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm, prob=prob, 
                    start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'HFB.zoo' end
