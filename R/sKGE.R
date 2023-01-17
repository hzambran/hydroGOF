# File sKGE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2022-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'sKGE': Kling-Gupta Efficiency with focus on low flows                       #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates: 13-Jul-2022 ; 14-Jul-2022                                           #
#          16-Jan-2023                                                         #
################################################################################
# The optimal value of sKGE is 1

# Ref:
# Fowler, K., Coxon, G., Freer, J., Peel, M., Wagener, T., 
# Western, A., Woods, R. and Zhang, L. (2018). Simulating runoff under 
# changing climatic conditions: A framework for model improvement.
# Water Resources Research, 54(12), pp.9812-9832. doi:https://doi.org/10.1029/2018WR023989

sKGE <- function(sim, obs, ...) UseMethod("sKGE")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
sKGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"),
                         start.month=1, out.PerYear=FALSE,
                         fun=NULL,
                         ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA
                         ) { 

  lKGE <- function(i, lsim, lobs, s=c(1,1,1), na.rm=TRUE, 
                   method=c("2009", "2012"), out.type="single",
                   fun1=NULL,
                   ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA) {
    llsim <- lsim[[i]]
    llobs <- lobs[[i]]

    out <- KGE(sim=llsim, obs=llobs, s=s, na.rm=na.rm, method=method, out.type=out.type,
               fun=fun1, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    return(out)
  } #'lKGE' END


  # Function for shifting a time vector by 'nmonths' number of months.
  .shiftyears <- function(ltime,       # Date/POSIX* object. It MUST contat MONTH and YEAR
                          lstart.month # numeric in [2,..,12], representing the months. 2:Feb, 12:Dec
                          ) {
     syears.bak        <- as.numeric(format( ltime, "%Y" ))
     syears            <- syears
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
   
    
  # Checking 'method' and 'epsilon.type'
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type)

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


  # Computing Annual KGE values
  #if (!is.null(fun)) {
  KGE.yr <- sapply(1:nyears, FUN=lKGE, lsim=sim.PerYear, lobs=obs.PerYear, s=s, 
                   na.rm= na.rm, method=method, out.type="single", 
                   fun1=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)

  names(KGE.yr) <- as.character(years.unique)

  sKGE <- mean(KGE.yr, na.rm=na.rm)

  if (out.PerYear) {
    out <- list(sKGE.value=sKGE, KGE.PerYear=KGE.yr)
  } else out <- sKGE
    
  return(out)
} # 'sKGE.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates: 14-Jul-2022                                                         #
#          16-Jan-2023                                                         #
################################################################################
sKGE.matrix <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"),
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
           
  # If the user provided a value for 's'
  if (!all.equal(s, c(1,1,1)) )  {
     if ( length(s) != 3 ) stop("Invalid argument: lenght(s) must be equal to 3 !")
     if ( sum(s) != 1 )    stop("Invalid argument: sum(s) must be equal to 1.0 !")
  } # IF end
           
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type)

  ifelse(method=="2012", vr.stg <- "Gamma", vr.stg <- "Alpha")

  sKGE               <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("r", "Beta", vr.stg)
  colnames(elements) <- colnames(obs)
          
  out <- sapply(1:ncol(obs), function(i,x,y) { 
                   sKGE[i] <- sKGE.default( x[,i], y[,i], s=s, na.rm=na.rm, 
                                              method=method, 
                                              start.month=start.month, 
                                              out.PerYear=out.PerYear, 
                                              fun=fun, ..., 
                                              epsilon.type=epsilon.type, 
                                              epsilon.value=epsilon.value )
                 }, x=sim, y=obs )  
  names(out) <- colnames(obs)                     
  
  return(out)
     
} # 'sKGE.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates: 14-Jul-2022                                                         #
#          16-Jan-2023                                                         #
################################################################################
sKGE.data.frame <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                            method=c("2009", "2012"),
                            start.month=1, out.PerYear=FALSE,  
                            fun=NULL,
                            ...,
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type) 
   
  sKGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, 
              start.month=start.month, out.PerYear=out.PerYear, fun=fun, ...,  
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'sKGE.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates: 14-Jul-2022                                                         #
#          16-Jan-2023                                                         #
################################################################################
sKGE.zoo <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                     method=c("2009", "2012"),
                     start.month=1, out.PerYear=FALSE,   
                     fun=NULL,
                     ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA) { 

  #sim <- zoo::coredata(sim)
  #if (is.zoo(obs)) obs <- zoo::coredata(obs)    

  if (is.matrix(sim) | is.data.frame(sim)) {
    sKGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, 
                start.month=start.month, out.PerYear=out.PerYear, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, s=s, na.rm=na.rm, method=method, 
                    start.month=start.month, out.PerYear=out.PerYear, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)  
     
} # 'sKGE.zoo' end
