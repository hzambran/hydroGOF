# File apfb.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'apfb': Annual peak-flows bias                                               #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
# The optimal value of apfb is 0

# The annual peak-flow bias (APFB) objective function was proposed by 
# Mizukami et al. (2019) to drive the calibration of hydrological models focused
# in the reproduction of high-flow events.

# Ref:
# Mizukami, N., Rakovec, O., Newman, A. J., Clark, M. P., Wood, A. W., 
# Gupta, H. V., and Kumar, R.: (2019). On the choice of calibration metrics for 
# "high-flow" estimation using hydrologic models, Hydrology Earth System 
# Sciences 23, 2601–2614, doi:10.5194/hess-23-2601-2019.

apfb <- function(sim, obs, ...) UseMethod("apfb")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
apfb.default <- function(sim, obs, na.rm=TRUE, 
                         start.month=1, out.PerYear=FALSE,
                         fun=NULL,
                         ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA
                         ) { 

  lapfb <- function(i, lsim, lobs, lna.rm=TRUE) {

    llobs            <- lobs[[i]]
    llsim            <- lsim[[i]]
    llobs.peak       <- max(llobs, na.rm=lna.rm)
    llsim.peak       <- max(llsim, na.rm=lna.rm)

    out <- sqrt((llsim.peak/llobs.peak - 1)^2)
    return(out)
  } #'lapfb' END


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
  } # '.shift' END
   

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

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


  # Computing annual apfb values
  apfb.yr <- sapply(1:nyears, FUN=lapfb, lsim=sim.PerYear, lobs=obs.PerYear, 
                    lna.rm= na.rm)
  names(apfb.yr) <- years.unique

  apfb <- mean(apfb.yr, na.rm=na.rm)

  if (out.PerYear) {
    out <- list(apfb.value=apfb, apfb.PerYear=apfb.yr)
  } else out <- apfb
    
  return(out)
} # 'apfb.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates:                                                                     #
################################################################################

apfb.matrix <- function(sim, obs, na.rm=TRUE, 
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


  apfb               <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=nyears, ncol=ncol(obs))
  rownames(elements) <- unique(years.obs)
  colnames(elements) <- colnames(obs)


  out.single <- sapply(1:ncol(obs), function(i,x,y) { 
                       apfb[i] <- apfb.default( x[,i], y[,i], na.rm=na.rm, 
                                                start.month=start.month, 
                                                out.PerYear=out.PerYear, 
                                                fun=fun, 
                                                epsilon.type=epsilon.type, 
                                                epsilon.value=epsilon.value )[[1]]
                 }, x=sim, y=obs ) 
  names(out.single) <- names(obs)


  if (out.PerYear) {        
    out.yr <- sapply(1:ncol(obs), function(i,x,y) { 
                    elements[,i] <- apfb.default( x[,i], y[,i], na.rm=na.rm, 
                                                 start.month=start.month, 
                                                 out.PerYear=out.PerYear, 
                                                 fun=fun, 
                                                 epsilon.type=epsilon.type, 
                                                 epsilon.value=epsilon.value )[[2]]
                   }, x=sim, y=obs ) 
 
    colnames(out.yr) <- names(obs)                     
  } # IF end

  if (out.PerYear) {
    out <- list(apfb.value=out.single, apfb.PerYear=out.yr)
  } else out <- out.single
  
  return(out)
     
} # 'apfb.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
apfb.data.frame <- function(sim, obs, na.rm=TRUE, 
                           start.month=1, out.PerYear=FALSE, 
                           fun=NULL,
                           ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
   
  apfb.matrix(sim, obs, na.rm=na.rm, 
             start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'apfb.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates:                                                                     #
################################################################################
apfb.zoo <- function(sim, obs, na.rm=TRUE, 
                    start.month=1, out.PerYear=FALSE, 
                    fun=NULL,
                    ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA) { 

  #sim <- zoo::coredata(sim)
  #if (is.zoo(obs)) obs <- zoo::coredata(obs)    

  if (is.matrix(sim) | is.data.frame(sim)) {
    apfb.matrix(sim, obs, na.rm=na.rm, 
                 start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm,
                    start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'apfb.zoo' end
