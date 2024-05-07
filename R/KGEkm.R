# File KGEkm.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'KGEkm': Kling-Gupta Efficiency with Knowable-moments                        #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 06-May-2024                                                         #
# Updates: 07-May-2024                                                         #
################################################################################
# The optimal value of KGEkm is 1

# Ref1:
# Pizarro, A. and Jorquera, J. (2024). Advancing objective functions in 
# hydrological modelling: Integrating knowable moments for improved simulation 
# accuracy. Journal of Hydrology, 634, 131071. doi:10.1016/j.jhydrol.2024.131071

# Ref2:
# Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez, 
# Decomposition of the mean squared error and NSE performance criteria: 
# Implications for improving hydrological modelling, 
# Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91, 
# DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694, 

# Ref3:
# Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper
# Danube basin under an ensemble of climate change scenarios, 
# Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, 
# DOI:10.1016/j.jhydrol.2012.01.011.

# Ref4: Tang, G., Clark, M. P., & Papalexiou, S. M. (2021).  
# SC-earth: a station-based serially complete earth dataset from 1950 to 2019. 
# Journal of Climate, 34(16), 6493-6511.
# DOI: 10.1175/JCLI-D-21-0067.1.


# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.

# 'Result': Kling-Gupta Efficiency between 'sim' and 'obs'

KGEkm <- function(sim, obs, ...) UseMethod("KGEkm")

KGEkm.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                          method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
                          fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA) { 

  # If the user provided a value for 's'
  if (!identical(s, c(1,1,1)) )  {
     if ( length(s) != 3 ) stop("Invalid argument: lenght(s) must be equal to 3 !")
     if ( sum(s) != 1 )    stop("Invalid argument: sum(s) must be equal to 1.0 !")
  } # IF end
     
  method   <- match.arg(method)
  out.type <- match.arg(out.type)  

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      
   
  vi <- valindex(sim, obs)
     
  if (length(vi) > 0) {
	 
    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end

    # Mean values
    mean.sim <- mean(sim, na.rm=na.rm)
    mean.obs <- mean(obs, na.rm=na.rm)

    # Dispersion measures (~ standard deviation)
    n         <- length(vi)
    i         <- seq(from=1, to=n, by=1) 
    K2sim     <- sum( 2*(i-1)*sim / (n*(n-1)) )
    K2obs     <- sum( 2*(i-1)*obs / (n*(n-1)) )
    sigma.sim <- sqrt(2*K2sim)
    sigma.obs <- sqrt(2*K2obs)
         
    # Pearson product-moment correlation coefficient
    r     <- rPearson(sim, obs)

    # Alpha is a measure of relative variability between simulated and observed values (See Ref1)
    Alpha <- sigma.sim / sigma.obs

    # Beta is the ratio between the mean of the simulated values to the mean of observations
    Beta <- mean.sim / mean.obs

    # Beta.2021 is the bias term proposed by Tang et al. (2021) to avoid the 
    # anomalously negative KE or KGEkm' values when the mean value is close to zero 
    Beta.2021 <- (mean.sim - mean.obs) / sigma.obs
       
    # CV.sim is the coefficient of variation of the simulated values [dimensionless]
    # CV.obs is the coefficient of variation of the observations [dimensionless]
    CV.sim <- sigma.sim / mean.sim
    CV.obs <- sigma.obs / mean.obs
       
    # Gamma is the variability ratio, which is used instead of Alpha (See Ref2)
    Gamma <- CV.sim / CV.obs
       
    # Variability ratio depending on 'method'
    if (method=="2012") {
      br     <- Beta
      br.stg <- "Beta"
      vr     <- Gamma
      vr.stg <- "Gamma"
    } else if (method=="2009") {
        br     <- Beta
        br.stg <- "Beta"
        vr     <- Alpha
        vr.stg <- "Alpha"
      } else if (method=="2021") {
          br     <- Beta.2021
          br.stg <- "Beta.2021"
          vr     <- Alpha
          vr.stg <- "Alpha"
        } # ELSE end

    # KGEkm Computation
    if ( (mean.obs != 0) | (sigma.obs != 0) ) {
        if ( (method=="2009") | (method=="2012") ) {
          KGEkm <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2 )
        } else KGEkm <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta.2021))^2 )
    } else {
        if ( mean.obs != 0)  warning("Warning: 'mean(obs)==0'. Beta = Inf")
        if ( sigma.obs != 0) warning("Warning: 'sd(obs)==0'. ", vr.stg, " = Inf")
        KGEkm <- NA
      } # ELSE end  
            
  } else {
      r    <- NA
      Beta <- NA
      vr   <- NA
      br   <- NA
      if (method=="2012") {
        br.stg <- "Beta"
        vr.stg <- "Gamma"
      } else if (method=="2009") {
          br.stg <- "Beta"
          vr.stg <- "Alpha" 
        } else {
            br.stg <- "Beta.2021"
            vr.stg <- "Alpha" 
          } # ELSE end
      KGEkm <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end

  if (out.type=="single") {
        out <- KGEkm
  } else {
      out <- list(KGEkm.value=KGEkm, KGEkm.elements=c(r, br, vr))
      names(out[[2]]) <- c("r", br.stg, vr.stg)
    } # ELSE end    
 
  return(out)
     
} # 'KGEkm.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 06-May-2024                                                         #
# Updates: 07-May-2024                                                         #
################################################################################
KGEkm.matrix <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                         method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
                         fun=NULL, ...,
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
           
  method   <- match.arg(method)
  out.type <- match.arg(out.type) 

  ifelse(method=="2012", vr.stg <- "Gamma", vr.stg <- "Alpha")

  KGEkm                <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("r", "Beta", vr.stg)
  colnames(elements) <- colnames(obs)
          
  if (out.type=="single") {
    out <- sapply(1:ncol(obs), function(i,x,y) { 
                   KGEkm[i] <- KGEkm.default( x[,i], y[,i], s=s, na.rm=na.rm, 
                                         method=method, out.type=out.type, 
                                         fun=fun, ..., epsilon.type=epsilon.type, 
                                         epsilon.value=epsilon.value )
                 }, x=sim, y=obs )  
    names(out) <- colnames(obs) 
  } else { out <- lapply(1:ncol(obs), function(i,x,y) { 
                         KGEkm.default( x[,i], y[,i], s=s, na.rm=na.rm, method=method, 
                                      out.type=out.type, fun=fun, ..., 
                                      epsilon.type=epsilon.type, 
                                      epsilon.value=epsilon.value )
                       }, x=sim, y=obs ) 
            for (i in 1:length(out) ) {
               KGEkm[i] <- out[[i]][[1]]
               elements[,i] <- as.numeric(out[[i]][[2]])
            } # FOR end 
            out <- list(KGEkm.value=KGEkm, KGEkm.elements=elements)
          } # ELSE end                     
  
  return(out)
     
} # 'KGEkm.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 06-May-2024                                                         #
# Updates: 07-May-2024                                                         #
################################################################################
KGEkm.data.frame <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                             method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
                             fun=NULL, ...,
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method   <- match.arg(method)
  out.type <- match.arg(out.type) 
   
  KGEkm.matrix(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, 
             fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'KGEkm.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 06-May-2024                                                         #
# Updates: 07-May-2024                                                         #
################################################################################
KGEkm.zoo <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                      method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
                      fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA) { 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       KGEkm.matrix(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, 
                  fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, 
                      fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'KGEkm.zoo' end
