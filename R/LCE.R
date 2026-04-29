# File LCE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Lee and Choi Efficiency (LCE)                                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 29-Abr-2026                                                         #
################################################################################
# Reference:                                                                   #
#  Lee, J. S. and Choi, H. I. (2022). A rebalanced performance criterion for   #
#  hydrological model calibration. Journal of Hydrology, 606, 127372.          #
#  https://doi.org/10.1016/j.jhydrol.2021.127372                               #
################################################################################
# This goodness-of-fit measure was proposed by Lee and Choi (2022) as an       #
# alternative to the Liu-Mean Efficiency (LME), designed to provide a          #
# multi-dimensional and diagnostically balanced evaluation of model            #
# performance, whereas LME is fundamentally a single-error-based metric.       #
################################################################################
# LME evaluates performance through a normalized mean squared error, so it 
# measures overall accuracy relative to the mean magnitude of observations,  
# but it does not distinguish between different sources of error (e.g., timing, 
# variability, or bias). As a result, two simulations with very different 
# hydrological behaviors (e.g., correct mean but wrong dynamics, or correct 
# variability but biased magnitude) can yield similar LME values.
# In contrast, LCE explicitly decomposes model performance into three 
# interacting components: correlation (r), variability ratio (alpha), and 
# bias (Beta) and, crucially, introduces the terms r*Alpha and r/Alpha to  
# enforce consistency between timing and variability
################################################################################
# The Lee and Choi Efficiency (LCE) is a goodness-of-fit metric that evaluates 
# how well simulated values reproduce observed data by jointly assessing 
# correlation, variability, and bias, while explicitly penalizing imbalances
# between correlation and variability through two complementary terms 
# (r*Alpha and r/Alpha).
################################################################################
# LCE values range from -Inf to 1, where:
# - LCE = 1 indicates perfect agreement (i.e., identical timing, dispersion, 
#   and magnitude between simulated and observed series), 
# - values close to 1 indicate high overall consistency across these attributes, 
# - values around 0 indicate that discrepancies in correlation-variability 
#   balance and bias are substantial relative to the ideal conditions, and 
# - negative values reflect progressively poorer performance, typically 
#   arising when the simulation fails to reproduce either the temporal dynamics 
#   (low r), the relative variability (Alpha), or the mean magnitude (Beta), or 
#   when these components are inconsistently balanced. 
################################################################################
# By construction, LCE is particularly sensitive to mismatches in the interaction 
# between correlation and variability, making it a more stringent and 
# diagnostically informative criterion than traditional efficiency metrics in 
# hydrological model evaluation.
################################################################################

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Lee and Choi Efficiency between 'sim' and 'obs'

LCE <- function(sim, obs, ...) UseMethod("LCE")

LCE.default <- function(sim, obs, na.rm=TRUE,
                        out.type=c("single", "full"),
                        fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012",
                                       "otherFactor", "otherValue"),
                        epsilon.value=NA) {

  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo"))) )
    stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

  vi <- valindex(sim, obs)

  if (length(vi) > 0) {

    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ...,
                      epsilon.type=epsilon.type,
                      epsilon.value=epsilon.value)
      sim <- new[["sim"]]
      obs <- new[["obs"]]
    }

    mean.sim  <- mean(sim, na.rm=na.rm)
    mean.obs  <- mean(obs, na.rm=na.rm)
    sigma.sim <- stats::sd(sim, na.rm=na.rm)
    sigma.obs <- stats::sd(obs, na.rm=na.rm)

    r     <- rPearson(sim, obs)
    Alpha <- sigma.sim / sigma.obs
    Beta  <- mean.sim / mean.obs

    rAlpha <- r * Alpha
    rAlpha.inv <- r / Alpha

    if ( (mean.obs != 0) & (sigma.obs != 0) & (Alpha != 0) ) {

      LCE <- 1 - sqrt( (rAlpha - 1)^2 +
                       (rAlpha.inv - 1)^2 +
                       (Beta - 1)^2 )

    } else {

      LCE <- NA

      if (mean.obs == 0)
        warning("Warning: 'mean(obs)==0'. Beta = Inf")

      if (sigma.obs == 0)
        warning("Warning: 'sd(obs)==0'. Alpha = Inf")

      if (Alpha == 0)
        warning("Warning: 'Alpha==0'. r/Alpha is undefined")
    }

  } else {

    r <- Alpha <- Beta <- rAlpha <- rAlpha.inv <- NA
    LCE <- NA

    warning("There are no pairs of 'sim' and 'obs' without missing values !")
  }

  if (out.type=="single") {

    out <- LCE

  } else {

    out <- list(LCE.value=LCE,
                LCE.elements=c(r, Alpha, Beta, rAlpha, rAlpha.inv))

    names(out[[2]]) <- c("r", "Alpha", "Beta", "rAlpha", "rOverAlpha")
  }

  return(out)

} # 'LCE.default' end


LCE.matrix <- function(sim, obs, na.rm=TRUE,
                       out.type=c("single", "full"),
                       fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012",
                                      "otherFactor", "otherValue"),
                       epsilon.value=NA) {

  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [",
          paste(dim(sim), collapse=" "), "] != [",
          paste(dim(obs), collapse=" "), "] )", sep="") )

  out.type <- match.arg(out.type)

  LCE                <- rep(NA, ncol(obs))
  elements           <- matrix(NA, nrow=5, ncol=ncol(obs))
  rownames(elements) <- c("r", "Alpha", "Beta", "rAlpha", "rOverAlpha")
  colnames(elements) <- colnames(obs)

  if (out.type=="single") {

    out <- sapply(1:ncol(obs), function(i, x, y) {
      LCE[i] <- LCE.default(x[, i], y[, i],
                            na.rm=na.rm,
                            out.type=out.type,
                            fun=fun, ...,
                            epsilon.type=epsilon.type,
                            epsilon.value=epsilon.value)
    }, x=sim, y=obs)

    names(out) <- colnames(obs)

  } else {

    out <- lapply(1:ncol(obs), function(i, x, y) {
      LCE.default(x[, i], y[, i],
                  na.rm=na.rm,
                  out.type=out.type,
                  fun=fun, ...,
                  epsilon.type=epsilon.type,
                  epsilon.value=epsilon.value)
    }, x=sim, y=obs)

    for (i in 1:length(out)) {
      LCE[i]         <- out[[i]][[1]]
      elements[, i] <- as.numeric(out[[i]][[2]])
    }

    out <- list(LCE.value=LCE, LCE.elements=elements)
  }

  return(out)

} # 'LCE.matrix' end


LCE.data.frame <- function(sim, obs, na.rm=TRUE,
                           out.type=c("single", "full"),
                           fun=NULL, ...,
                           epsilon.type=c("none", "Pushpalatha2012",
                                          "otherFactor", "otherValue"),
                           epsilon.value=NA) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  out.type <- match.arg(out.type)

  LCE.matrix(sim, obs,
             na.rm=na.rm,
             out.type=out.type,
             fun=fun, ...,
             epsilon.type=epsilon.type,
             epsilon.value=epsilon.value)

} # 'LCE.data.frame' end


LCE.zoo <- function(sim, obs, na.rm=TRUE,
                    out.type=c("single", "full"),
                    fun=NULL, ...,
                    epsilon.type=c("none", "Pushpalatha2012",
                                   "otherFactor", "otherValue"),
                    epsilon.value=NA) {

  sim <- zoo::coredata(sim)

  if (is.zoo(obs))
    obs <- zoo::coredata(obs)

  out.type <- match.arg(out.type)

  if (is.matrix(sim) | is.data.frame(sim)) {

    LCE.matrix(sim, obs,
               na.rm=na.rm,
               out.type=out.type,
               fun=fun, ...,
               epsilon.type=epsilon.type,
               epsilon.value=epsilon.value)

  } else {

    NextMethod(sim, obs,
               na.rm=na.rm,
               out.type=out.type,
               fun=fun, ...,
               epsilon.type=epsilon.type,
               epsilon.value=epsilon.value)
  }

} # 'LCE.zoo' end