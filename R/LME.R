# File LME.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Liu-Mean Efficiency (LME)                                                    #
################################################################################
# Reference:                                                                   #
#  Liu, D. (2020). A rational performance criterion for hydrological model.    #
#  Journal of Hydrology, 590, 125488.                                          #
#  https://doi.org/10.1016/j.jhydrol.2020.125488                               #
################################################################################
# Started: 28-Abr-2026                                                         #
################################################################################
# The Liu–Mean Efficiency (LME) is a goodness-of-fit metric designed to 
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
# 'Result': Joint Divergence Kling-Gupta Efficiency between 'sim' and 'obs'

LME <- function(sim, obs, ...) UseMethod("LME")

###############################################################################
# Default method
###############################################################################

LME.default <- function(sim, obs,
                        na.rm=TRUE,
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


###############################################################################
# Matrix method
###############################################################################

LME.matrix <- function(sim, obs,
                       na.rm=TRUE,
                       out.type=c("single","full"),
                       fun=NULL, ...,
                       epsilon.type=c("none",
                                      "Pushpalatha2012",
                                      "otherFactor",
                                      "otherValue"),
                       epsilon.value=NA) {

  if (!is.matrix(sim))
    stop("'sim' must be a matrix")

  if (!is.matrix(obs))
    stop("'obs' must be a matrix")

  if (!all(dim(sim) == dim(obs)))
    stop("'sim' and 'obs' must have the same dimensions")

  nvar <- ncol(sim)

  LME.values <- numeric(nvar)

  if (out.type == "full")
    elements <- matrix(NA_real_,
                       nrow=3,
                       ncol=nvar)

  for (i in seq_len(nvar)) {

    res <- LME.default(sim=sim[, i],
                       obs=obs[, i],
                       na.rm=na.rm,
                       out.type="full",
                       fun=fun, ...,
                       epsilon.type=epsilon.type,
                       epsilon.value=epsilon.value)

    LME.values[i] <- res$LME.value

    if (out.type == "full")
      elements[, i] <- res$LME.elements
  }

  if (out.type == "single") {

    return(LME.values)

  } else {

    rownames(elements) <- c("r",
                            "Alpha",
                            "Beta")

    return(list(LME.value    = LME.values,
                LME.elements = elements))
  }

} # 'LME.matrix' end

###############################################################################
# data.frame method
###############################################################################

LME.data.frame <- function(sim, obs, ...) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  LME.matrix(sim=sim, obs=obs, ...)

} # 'LME.data.frame' end

###############################################################################
# zoo method
###############################################################################

LME.zoo <- function(sim, obs, ...) {

  if (!requireNamespace("zoo", quietly=TRUE))
    stop("Package 'zoo' is required")

  sim <- zoo::coredata(sim)
  obs <- zoo::coredata(obs)

  LME.matrix(sim=sim, obs=obs, ...)

} # 'LME.zoo' end