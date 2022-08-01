# File KGElf.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/
# Copyright 2017-2022 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'KGElf': Kling-Gupta Efficiency with focus on low flows                      #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 2017                                                                #
# Updates: 07-Jul-2022 ; 11-Jul-2022 ;12-Jul-2022                              #
################################################################################
# The optimal value of KGElf is 1

# Ref1:
# Garcia, F., Folton, N. and Oudin, L. (2017). 
# Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. 
# Hydrological sciences journal, 62(7), pp.1149-1166. doi:10.1080/02626667.2017.1308511

# Ref2:
# Pushpalatha, R., Perrin, C., Le Moine, N. and Andreassian, V. (2012). 
# A review of efficiency criteria suitable for evaluating low-flow simulations. 
# Journal of Hydrology, 420, pp.171-182. doi: 10.1016/j.jhydrol.2011.11.055

KGElf <- function(sim, obs, ...) UseMethod("KGElf")

# epsilon: By default it is set at one hundredth of the mean flow. See Pushpalatha et al. (2012)
KGElf.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                          method=c("2009", "2012"), 
                          fun=function(x) 1/x,
                          ...,
                          epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA) { 
  
  # Checking 'method' and 'epsilon''
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type)

  # 1) KGE(Q): KGE (2009 or 2012)
  kge <- KGE(sim=sim, obs=obs, s=s, na.rm=na.rm, method=method, out.type="single")
  
  # 2) KGE(1/Q): KGE based on Garcia et al. (2017), with epsilon based on Pushpalatha et al. (2012)
  new <- preproc(sim=sim, obs=obs, fun=fun, ...,
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  sim.lf <- new[["sim"]]
  obs.lf <- new[["obs"]]
  kge.lf <- KGE(sim=sim.lf, obs=obs.lf, s=s, na.rm=na.rm, method=method, out.type="single")
  
  # 3) [KGE(Q) + KGE(1/Q)] / 2 : comprehensive goodness-of-fit value
  out <- (kge + kge.lf) / 2
    
  return(out)
  
} # 'KGElf.default' END


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
KGElf.matrix <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2009", "2012"), 
                         fun=function(x) 1/x,
                         ...,
                         epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
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

  KGElf              <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("r", "Beta", vr.stg)
  colnames(elements) <- colnames(obs)
          
  out <- sapply(1:ncol(obs), function(i,x,y) { 
                   KGElf[i] <- KGElf.default( x[,i], y[,i], s=s, na.rm=na.rm, 
                                              method=method, fun=fun, ..., 
                                              epsilon.type=epsilon.type, 
                                              epsilon.value=epsilon.value )
                 }, x=sim, y=obs )  
  names(out) <- colnames(obs)                     
  
  return(out)
     
} # 'KGElf.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
KGElf.data.frame <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                             method=c("2009", "2012"), 
                             fun=function(x) 1/x,
                             ...,
                             epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method       <- match.arg(method)
  epsilon.type <- match.arg(epsilon.type) 
   
  KGElf.matrix(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'KGElf.data.frame' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 12-Jul-2022                                                         #
# Updates:                                                                     #
################################################################################
KGElf.zoo <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                      method=c("2009", "2012"), 
                      fun=function(x) 1/x,
                      ...,
                      epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA) { 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       KGElf.matrix(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, s=s, na.rm=na.rm, method=method, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'KGElf.zoo' end
