# File KGE.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2014 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'KGE': Kling-Gupta Efficiency                                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 18-Jan-2011                                                         #
# Updates: 25-Aug-2011                                                         #
#          10-Oct-2012                                                         #
#          18-Oct-2012 ; 19-Oct-2012                                           #
#          24-Jan-2014                                                         #
#          28-Feb-2016 ; 17-Jul-2016                                           #
################################################################################
# The optimal value of KGE is 1

# Ref1:
# Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez, 
# Decomposition of the mean squared error and NSE performance criteria: 
# Implications for improving hydrological modelling, 
# Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91, 
# DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694, 

# Ref2:
# Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper
# Danube basin under an ensemble of climate change scenarios, 
# Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, 
# DOI:10.1016/j.jhydrol.2012.01.011.


# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.

# 'Result': Kling-Gupta Efficiency between 'sim' and 'obs'

KGE <- function(sim, obs, ...) UseMethod("KGE")

KGE.default <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                        method=c("2009", "2012"), out.type=c("single", "full"), ...) { 

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

    # Mean values
    mean.sim <- mean(sim, na.rm=na.rm)
    mean.obs <- mean(obs, na.rm=na.rm)

    # Standard deviations
    sigma.sim <- sd(sim, na.rm=na.rm)
    sigma.obs <- sd(obs, na.rm=na.rm)
         
    # Pearson product-moment correlation coefficient
    r     <- rPearson(sim, obs)

    # Alpha is a measure of relative variability between simulated and observed values (See Ref1)
    Alpha <- sigma.sim / sigma.obs

    # Beta is the ratio between the mean of the simulated values to the mean of observations
    Beta <- mean.sim / mean.obs
       
    # CV.sim is the coefficient of variation of the simulated values [dimensionless]
    # CV.obs is the coefficient of variation of the observations [dimensionless]
    CV.sim <- sigma.sim / mean.sim
    CV.obs <- sigma.obs / mean.obs
       
    # Gamma is the variability ratio, which is used instead of Alpha (See Ref2)
    Gamma <- CV.sim / CV.obs
       
    # Variability ratio depending on 'method'
    if(method=="2012") {
      vr     <- Gamma
      vr.stg <- "Gamma"
    } else {
        vr     <- Alpha
        vr.stg <- "Alpha"
      } # ELSE end

    # KGE Computation
    if ( (mean.obs != 0) | (sigma.obs != 0) ) {
        KGE <- 1 - sqrt( (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2 )
    } else {
        if ( mean.obs != 0)  warning("Warning: 'mean(obs)==0'. Beta = Inf")
        if ( sigma.obs != 0) warning("Warning: 'sd(obs)==0'. ", vr.stg, " = Inf")
        KGE <- NA
      } # ELSE end  
            
  } else {
      r    <- NA
      Beta <- NA
      vr   <- NA
      if(method=="2012") {
        vr.stg <- "Gamma"
      } else vr.stg <- "Alpha" 
      KGE <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end

  if (out.type=="single") {
        out <- KGE
  } else {
      out <- list(KGE.value=KGE, KGE.elements=c(r, Beta, vr))
      names(out[[2]]) <- c("r", "Beta", vr.stg)
    } # ELSE end    
 
  return(out)
     
} # 'KGE.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 18-Jan-2011                                                         #
# Updates: 25-Aug-2011                                                         #
#          10-Oct-2012                                                         #
#          18-Oct-2012 ; 19-Oct-2012                                           #
################################################################################
KGE.matrix <- function (sim, obs, s=c(1,1,1), na.rm=TRUE, 
                        method=c("2009", "2012"), out.type=c("single", "full"), ...){ 

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

  KGE                <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("r", "Beta", vr.stg)
  colnames(elements) <- colnames(obs)
          
  if (out.type=="single") {
    out <- sapply(1:ncol(obs), function(i,x,y) { 
                   KGE[i] <- KGE.default( x[,i], y[,i], s=s, na.rm=na.rm, method=method, out.type=out.type, ... )
                 }, x=sim, y=obs )  
    names(out) <- colnames(obs) 
  } else { out <- lapply(1:ncol(obs), function(i,x,y) { 
                         KGE.default( x[,i], y[,i], s=s, na.rm=na.rm, method=method, out.type=out.type, ... )
                       }, x=sim, y=obs ) 
            for (i in 1:length(out) ) {
               KGE[i] <- out[[i]][[1]]
               elements[,i] <- as.numeric(out[[i]][[2]])
            } # FOR end 
            out <- list(KGE.value=KGE, KGE.elements=elements)
          } # ELSE end                     
  
  return(out)
     
} # 'KGE.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 18-Jan-2011                                                         #
# Updates: 25-Aug-2011                                                         #
#          10-Oct-2012                                                         #
#          18-Oct-2012 ; 19-Oct-2012                                           #
################################################################################
KGE.data.frame <- function (sim, obs, s=c(1,1,1), na.rm=TRUE, 
                            method=c("2009", "2012"), out.type=c("single", "full"), ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method   <- match.arg(method)
  out.type <- match.arg(out.type) 
   
  KGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, ...)
     
} # 'KGE.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Aug-2016                                                         #
################################################################################
KGE.zoo <- function(sim, obs, s=c(1,1,1), na.rm=TRUE, 
                    method=c("2009", "2012"), out.type=c("single", "full"), ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       KGE.matrix(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, ...)
    } else NextMethod(sim, obs, s=s, na.rm=na.rm, method=method, out.type=out.type, ...)
     
  } # 'KGE.zoo' end
