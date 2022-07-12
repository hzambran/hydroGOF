# File sKGE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
# Copyright 2017-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'sKGE': Kling-Gupta Efficiency with focus on low flows                      #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2017                                                                #
# Updates: 07-Jul-2022 ; 11-Jul-2022 ;12-Jul-2022                              #
################################################################################
# The optimal value of sKGE is 1

# Ref1:
# Garcia, F., Folton, N. and Oudin, L. (2017). 
# Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. 
# Hydrological sciences journal, 62(7), pp.1149-1166. doi:10.1080/02626667.2017.1308511

# Ref2:
# Pushpalatha, R., Perrin, C., Le Moine, N. and Andréassian, V. (2012). 
# A review of efficiency criteria suitable for evaluating low-flow simulations. 
# Journal of Hydrology, 420, pp.171-182. doi: 10.1016/j.jhydrol.2011.11.055

sKGE <- function(sim, obs, ...) UseMethod("sKGE")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
sKGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"), 
                         fun=function(x) 1/x,
                         ...,
                         epsilon=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) { 

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo")) {
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")
  } else if (is.matrix(sim) | is.data.frame(sim)) {
       sKGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
                    epsilon=epsilon, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
                      epsilon=epsilon, epsilon.value=epsilon.value)  
} # 'sKGE.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
sKGE.matrix <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"), 
                         fun=function(x) 1/x,
                         ...,
                         epsilon=c("Pushpalatha2012", "otherFactor", "otherValue"), 
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
           
  method   <- match.arg(method)
  epsilon <- match.arg(epsilon)

  ifelse(method=="2012", vr.stg <- "Gamma", vr.stg <- "Alpha")

  sKGE              <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("r", "Beta", vr.stg)
  colnames(elements) <- colnames(obs)
          
  out <- sapply(1:ncol(obs), function(i,x,y) { 
                   sKGE[i] <- sKGE.default( x[,i], y[,i], s=s, na.rm=na.rm, 
                                              method=method, fun=fun, ..., 
                                              epsilon=epsilon, 
                                              epsilon.value=epsilon.value )
                 }, x=sim, y=obs )  
  names(out) <- colnames(obs)                     
  
  return(out)
     
} # 'sKGE.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
sKGE.data.frame <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                             method=c("2009", "2012"), 
                             fun=function(x) 1/x,
                             ...,
                             epsilon=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method   <- match.arg(method)
  epsilon <- match.arg(epsilon) 
   
  sKGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
               epsilon=epsilon, epsilon.value=epsilon.value)
     
} # 'sKGE.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
sKGE.zoo <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                     method=c("2009", "2012"), 
                     fun=function(x) 1/x,
                     ...,
                     epsilon=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA) { 
    
  # Checking 'method' and 'epsilon''
  method  <- match.arg(method)
  epsilon <- match.arg(epsilon)

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

  if (!is.null(fun)) {
     fun <- match.fun(fun)
     new <- preproc(sim=sim, obs=obs, fun=fun, ..., 
                    epsilon=epsilon, epsilon.value=epsilon.value)
     sim <- new[["sim"]]
     obs <- new[["obs"]]
  } # IF end

  # Annual index for 'x'
  dates.sim  <- time(sim)
  dates.obs  <- time(obs)
  years.sim  <- format( dates.sim, "%Y")
  years.obs  <- format( dates.obs, "%Y")
  if (!all.equal(years.sim, years.obs))
    stop("Invalid argument: 'sim' and 'obs' must have the same dates !")

  # Computing Annual KGE values
  if (!is.null(fun)) {
    KGE.yr <- stats::aggregate(x=sim, by=years.sim, fun=KGE, obs=obs, s=s, 
                               na.rm= na.rm, method=method, 
                               out.type="single", epsilon=epsilon, 
                               epsilon.value=epsilon.value, fun=fun, ...)
  } else KGE.yr <- stats::aggregate(x=sim, by=years.sim, fun=KGE, obs=obs, s=s, 
                                    na.rm= na.rm, method=method, 
                                    out.type="single", epsilon=epsilon, 
                                    epsilon.value=epsilon.value)

  sKGE <- mean(KGE.yr, na.rm=na.rm)
    
  return(sKGE)
     
  } # 'sKGE.zoo' end
