# File KGEnp.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 http://cran.r-project.org/web/packages/hydroGOF/
#                                 http://www.rforge.net/hydroGOF/ ; 
#                                 
# Copyright 2023-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'KGEnp': non-parametric version of the Kling-Gupta Efficiency                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-Jan-2023                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
# This function is based on the description provided by Pool et al. (2018) at:
# https://www.tandfonline.com/doi/suppl/10.1080/02626667.2018.1552002?scroll=top&role=tab

# The optimal value of KGEnp is 1

# Ref:
# Pool, S., Vis, M. and Seibert, J. (2018). 
# Evaluating model performance: towards a non-parametric variant of the Kling-Gupta efficiency. 
# Hydrological Sciences Journal, 63(13-14), pp.1941-1953.
# doi:/10.1080/02626667.2018.1552002.


# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 's'   : scaling factors.

# 'Result': non-parametric Kling-Gupta between 'sim' and 'obs'

KGEnp <- function(sim, obs, ...) UseMethod("KGEnp")

KGEnp.default <- function(sim, obs, na.rm=TRUE, 
                          out.type=c("single", "full"), 
                          fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA) { 

  out.type <- match.arg(out.type)  

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      
   
  epsilon.type <- match.arg(epsilon.type)  

  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
  vi <- valindex(sim, obs)

  nQ <- length(vi) # nQ = length(sim) = length(obs)
     
  if (nQ > 0) {
	 
    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ...)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end

    # Mean values
    mean.sim <- mean(sim, na.rm=na.rm)
    mean.obs <- mean(obs, na.rm=na.rm)

    # Normalized flow duration curves
    fdc.sim <- sort(sim, decreasing=FALSE) / (mean.sim * nQ)
    fdc.obs <- sort(obs, decreasing=FALSE) / (mean.obs * nQ)
         
    # Spearman's rank correlation coefficient
    rS    <- cor(sim, obs, method="spearman", use="pairwise.complete.obs")

    # Alpha is a non-parametric form of the variability, based on the FDC (See Ref)
    Alpha <- 1 - 0.5 * sum(abs(fdc.sim - fdc.obs))

    # Beta is the ratio between the mean of the simulated values to the mean of observations
    Beta <- mean.sim / mean.obs

    # KGEnp Computation
    if ( mean.obs != 0 ) {
        KGEnp <- 1 - sqrt( (Beta-1)^2 + (Alpha-1)^2 + (rS-1)^2 )
    } else {
        if ( mean.obs != 0)  warning("Warning: 'mean(obs)==0'. Beta = Inf")
        KGEnp <- NA
      } # ELSE end  
            
  } else {
      r     <- NA
      Beta  <- NA
      Alpha <- NA
      KGEnp <- NA
      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end

  if (out.type=="single") {
        out <- KGEnp
  } else {
      out <- list(KGEnp.value=KGEnp, KGEnp.elements=c(r, Beta, Alpha))
      names(out[[2]]) <- c("rSpearman", "Beta", "Alpha")
    } # ELSE end    
 
  return(out)
     
} # 'KGEnp.default' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-Jan-2023                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
KGEnp.matrix <- function(sim, obs, na.rm=TRUE, 
                         out.type=c("single", "full"), 
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) { 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
     stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
           paste(dim(sim), collapse=" "), "] != [", 
           paste(dim(obs), collapse=" "), "] )", sep="") )
           

  out.type <- match.arg(out.type) 


  KGEnp              <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=3, ncol=ncol(obs))
  rownames(elements) <- c("rSpearman", "Beta", "Alpha")
  colnames(elements) <- colnames(obs)
          
  if (out.type=="single") {
    out <- sapply(1:ncol(obs), function(i,x,y) { 
                   KGEnp[i] <- KGEnp.default( x[,i], y[,i], na.rm=na.rm, 
                                              out.type=out.type, 
                                              fun=fun, ..., 
                                              epsilon.type=epsilon.type,  
                                              epsilon.value=epsilon.value)
                 }, x=sim, y=obs )  
    names(out) <- colnames(obs) 
  } else { out <- lapply(1:ncol(obs), function(i,x,y) { 
                         KGEnp.default( x[,i], y[,i], na.rm=na.rm, 
                                        out.type=out.type, 
                                        fun=fun, ..., 
                                        epsilon.type=epsilon.type,  
                                        epsilon.value=epsilon.value 
                                      )
                       }, x=sim, y=obs ) 
            for (i in 1:length(out) ) {
               KGEnp[i] <- out[[i]][[1]]
               elements[,i] <- as.numeric(out[[i]][[2]])
            } # FOR end 
            out <- list(KGEnp.value=KGEnp, KGEnp.elements=elements)
          } # ELSE end                     
  
  return(out)
     
} # 'KGEnp.matrix' end


################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 13-Jan-2023                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
KGEnp.data.frame <- function(sim, obs, na.rm=TRUE, 
                             out.type=c("single", "full"), 
                             fun=NULL, ...,
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) { 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  method   <- match.arg(method)
  out.type <- match.arg(out.type) 
   
  KGEnp.matrix(sim, obs, na.rm=na.rm, out.type=out.type, 
               fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'KGEnp.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 13-Jan-2023                                                         #
# Updates:                                                                     #
################################################################################
KGEnp.zoo <- function(sim, obs, na.rm=TRUE, 
                      out.type=c("single", "full"), 
                      fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA) { 
    
  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)
   
  if (is.matrix(sim) | is.data.frame(sim)) {
     KGEnp.matrix(sim, obs, na.rm=na.rm, out.type=out.type,
                  fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
  } else NextMethod(sim, obs, na.rm=na.rm, out.type=out.type,
                    fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'KGEnp.zoo' end
