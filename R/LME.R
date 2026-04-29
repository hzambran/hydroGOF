# File LME.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Liu-Mean Efficiency (LME)                                                    #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 28-Abr-2026                                                         #
################################################################################
# Reference:                                                                   #
#  Liu, D. (2020). A rational performance criterion for hydrological model.    #
#  Journal of Hydrology, 590, 125488.                                          #
#  https://doi.org/10.1016/j.jhydrol.2020.125488                               #
################################################################################
# This goodness-of-fit measure was proposed by Liu et al. (2020) as an 
# alternative to the Nash-Sutcliffe efficiency (NSE), designed to provide a 
# more balanced assessment of model performance by normalising the mean squared 
# error using the mean of the observed values instead of their variance.
################################################################################
# The Liu-Mean Efficiency emphasises proportional error relative to the mean 
# magnitude of the observed variable, making it particularly useful in 
# hydrological applications where the mean value is a meaningful scale for 
# evaluating prediction accuracy.
################################################################################
# The Liu-Mean Efficiency (LME) is a goodness-of-fit metric designed to 
# evaluate hydrological model performance by jointly assessing two fundamental 
# aspects of agreement between simulated and observed time series: 
# - the combined representation of correlation and variability, through the 
#   product ( r\alpha ) (reflecting how well the model reproduces the temporal 
#   dynamics and dispersion of flows), and 
# - the bias ratio ( \beta ) (reflecting systematic over- or underestimation 
#   of mean magnitude). 
################################################################################
# The metric has a theoretical range from -Inf to 1, where:
# - a value of 1 indicates perfect agreement in both dynamic behavior and mean 
#   flow conditions, meaning the simulation reproduces the observed variability 
#   structure and central tendency exactly; 
# - values near 0 indicate substantial discrepancies in timing, variability, 
#   or bias, such that the simulation departs meaningfully from observed 
#   behavior; and 
# - negative values indicate progressively poorer performance, typically 
#   arising when variability is poorly represented, correlation is weak, or 
#   mean bias is large. 

# Consequently, in hydrological model calibration and evaluation, values closer 
# to 1 denote higher overall fidelity of the simulated hydrological regime, 
# while increasingly negative values signal structural deficiencies in the 
# model's representation of flow dynamics or water balance.
################################################################################


# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Liu-Mean Efficiency between 'sim' and 'obs'

LME <- function(sim, obs, ...) UseMethod("LME")

###############################################################################
# Default method
###############################################################################

LME.default <- function(sim, obs, na.rm=TRUE,
                        out.type=c("single","full"),
                        fun=NULL, ...,
                        epsilon.type=c("none",
                                       "Pushpalatha2012",
                                       "otherFactor",
                                       "otherValue"),
                        epsilon.value=NA) {

  if (missing(sim)) stop("Argument 'sim' is missing")
  if (missing(obs)) stop("Argument 'obs' is missing")

  if (!is.numeric(sim)) stop("'sim' must be numeric")
  if (!is.numeric(obs)) stop("'obs' must be numeric")

  if (length(sim) != length(obs))
    stop("'sim' and 'obs' must have the same length")

  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  #############################################################################
  # Remove NA pairs
  #############################################################################

  if (na.rm) {
    valid <- !(is.na(sim) | is.na(obs))
    sim <- sim[valid]
    obs <- obs[valid]
  }

  if (length(sim) == 0)
    return(NA_real_)

  #############################################################################
  # Epsilon handling
  #############################################################################

  if (epsilon.type != "none") {

    if (epsilon.type == "Pushpalatha2012") {

      eps <- mean(obs, na.rm=TRUE) / 100

    } else if (epsilon.type == "otherFactor") {

      if (is.na(epsilon.value))
        stop("'epsilon.value' must be provided")

      eps <- epsilon.value * mean(obs, na.rm=TRUE)

    } else if (epsilon.type == "otherValue") {

      if (is.na(epsilon.value))
        stop("'epsilon.value' must be provided")

      eps <- epsilon.value
    }

    sim <- sim + eps
    obs <- obs + eps
  }

  #############################################################################
  # Transformation
  #############################################################################

  if (!is.null(fun)) {

    if (!is.function(fun))
      stop("'fun' must be a function")

    sim <- fun(sim, ...)
    obs <- fun(obs, ...)
  }

  #############################################################################
  # Core statistics
  #############################################################################

  mean.sim <- mean(sim)
  mean.obs <- mean(obs)

  sd.sim <- stats::sd(sim)
  sd.obs <- stats::sd(obs)

  if (sd.obs == 0)
    return(NA_real_)

  r <- stats::cor(sim, obs)

  Alpha <- sd.sim / sd.obs
  Beta  <- mean.sim / mean.obs

  #############################################################################
  # Liu Mean Efficiency
  #############################################################################

  LME.value <- 1 - sqrt((r * Alpha - 1)^2 +
                        (Beta - 1)^2)

  if (out.type == "single") {

    return(LME.value)

  } else {

    elements <- c(r, Alpha, Beta)

    names(elements) <- c("r", "Alpha", "Beta")

    return(list(LME.value    = LME.value,
                LME.elements = elements))
  }

} # 'LME.default' end


################################################################################
# Matrix method                                                                #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 28-Apr-2026                                                         #
################################################################################
LME.matrix <- function(sim, obs, na.rm=TRUE,
                       out.type=c("single","full"),
                       fun=NULL, ...,
                       epsilon.type=c("none",
                                      "Pushpalatha2012",
                                      "otherFactor",
                                      "otherValue"),
                       epsilon.value=NA) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
     stop( paste("Invalid argument: dim(sim) != dim(obs) ( [",
           paste(dim(sim), collapse=" "), "] != [",
           paste(dim(obs), collapse=" "), "] )", sep="") )

  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  LME                <- rep(NA, ncol(obs))
  elements           <- matrix(NA, nrow=2, ncol=ncol(obs))
  rownames(elements) <- c("MSE", "MeanObs")
  colnames(elements) <- colnames(obs)

  if (out.type=="single") {

    out <- sapply(1:ncol(obs), function(i, x, y) {

                   LME[i] <- LME.default(
                                 sim=x[, i],
                                 obs=y[, i],
                                 na.rm=na.rm,
                                 out.type=out.type,
                                 fun=fun, ...,
                                 epsilon.type=epsilon.type,
                                 epsilon.value=epsilon.value)

                 }, x=sim, y=obs)

    names(out) <- colnames(obs)

  } else {

    out <- lapply(1:ncol(obs), function(i, x, y) {

                    LME.default(
                        sim=x[, i],
                        obs=y[, i],
                        na.rm=na.rm,
                        out.type=out.type,
                        fun=fun, ...,
                        epsilon.type=epsilon.type,
                        epsilon.value=epsilon.value)

                  }, x=sim, y=obs)

    for (i in 1:length(out)) {
       LME[i]        <- out[[i]][[1]]
       elements[, i] <- as.numeric(out[[i]][[2]])
    }

    out <- list(LME.value= LME,
                LME.elements= elements)
  }

  return(out)

} # 'LME.matrix' end

###############################################################################
# data.frame method
###############################################################################
LME.data.frame <- function(sim, obs, na.rm=TRUE,
                           out.type=c("single","full"),
                           fun=NULL, ...,
                           epsilon.type=c("none",
                                          "Pushpalatha2012",
                                          "otherFactor",
                                          "otherValue"),
                           epsilon.value=NA) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  LME.matrix(sim=sim, obs=obs,
             na.rm=na.rm,
             out.type=out.type,
             fun=fun, ...,
             epsilon.type=epsilon.type,
             epsilon.value=epsilon.value)

} # 'LME.data.frame' end

###############################################################################
# zoo method
###############################################################################
LME.zoo <- function(sim, obs, na.rm=TRUE,
                    out.type=c("single","full"),
                    fun=NULL, ...,
                    epsilon.type=c("none",
                                   "Pushpalatha2012",
                                   "otherFactor",
                                   "otherValue"),
                    epsilon.value=NA) {

  sim <- zoo::coredata(sim)
  if (is.zoo(obs))
     obs <- zoo::coredata(obs)

  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)


  if (is.matrix(sim) | is.data.frame(sim)) {

     LME.matrix(sim, obs,
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

} # 'LME.zoo' end