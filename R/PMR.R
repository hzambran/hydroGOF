# File PMR.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'PMR': Proxy for Model Robustness                                            #
#        Normalized mean absolute deviation of moving-window bias              #
#        from the global bias                                                  #
################################################################################
# Reference:                                                                   #
# Royer-Gaspard, P., Andreassian, V., and Thirel, G. (2021).                   #
# Technical note: PMR - a proxy metric to assess hydrological model robustness #
# in a changing climate, Hydrology and Earth System Sciences, 25, 5703--5716,  #
# https://doi.org/10.5194/hess-25-5703-2021.                                   #
################################################################################
# Started: 27-Mar-2026                                                         #
################################################################################
# The proxy for model robustness (PMR) is a dimensionless performance metric 
# that quantifies the temporal stability of model bias by measuring how much 
# the bias computed over moving sub-periods deviates, on average, from the 
# overall bias of the simulation relative to observations. 
# In practical terms, it evaluates whether a model maintains consistent 
# performance across different climatic or hydrological conditions rather 
# than performing well only during specific periods. 
################################################################################
# The PMR ranges from 0 to +Inf: 
#
# - a value of 0 indicates perfect robustness, meaning that the model bias is 
#   identical across all sub-periods and therefore fully stable over time;  
# - values close to 0 indicate high robustness with only minor temporal 
#   variability in bias; and progressively 
# - larger values indicate decreasing robustness, reflecting increasing 
#   variability in model bias between periods; and very large values 
#   suggest substantial non-stationarity in model errors, implying that the 
#   model performance is highly sensitive to changes in hydrological  
#   conditions or calibration/validation periods.

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Nash-sutcliffe Efficiency between 'sim' and 'obs'


PMR <- function(sim, obs, ...) UseMethod("PMR")

PMR.default <- function(sim, obs,
                        k=NULL,
                        min.years=5,
                        days.per.year=365,
                        na.rm=TRUE,
                        fun=NULL,
                        ...,
                        epsilon.type=c("none",
                                       "Pushpalatha2012",
                                       "otherFactor",
                                       "otherValue"),
                        epsilon.value=NA) {

  #' Compute default window length corresponding to 5 years of data
  get_default_value_for_k <- function(x, years = 5, days.per.year = 365) {

    #'
    #' @param x A zoo time series
    #' @param years Number of years to use for the adaptive window (default: 5)
    #' @param days.per.year Use 365.25 instead of 365 if long climatological series are expected.
    #'
    #' @return Integer value for k (number of observations)
    #' @importFrom hydroTSM sfreq
    #' @importFrom zoo index

    if (!inherits(x, "zoo"))
      stop("'x' must be a 'zoo' time series")

    freq <- hydroTSM::sfreq(x)

    # Map frequency to observations per year
    obs_per_year <- switch(freq,
      hourly  = days.per.year * 24,
      daily   = days.per.year,
      weekly  = 52,
      monthly = 12,
      annual  = 1,
      stop("Unsupported or unknown temporal frequency returned by 'sfreq' !")
    )

    k <- years * obs_per_year

    # Basic safeguard: ensure at least two windows can be formed
    if (length(x) < 2 * k) 
      warning("'x' is short relative to 'k' (", k, "). Please specify 'k' explicitly.") 

    return(as.integer(k))

  } # 'get_default_value_for_k' END


  epsilon.type <- match.arg(epsilon.type)

  if ( !inherits(sim, "zoo") | !inherits(obs, "zoo"))
    stop("Invalid argument: 'sim' and 'obs' must be 'zoo' objects !")

  if (is.null(k)) 
    k <- get_default_value_for_k(obs, years = min.years)

  vi <- valindex(sim, obs)

  if (length(vi) > 0) {

    obs <- obs[vi]
    sim <- sim[vi]

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)

      new <- preproc(sim=sim,
                     obs=obs,
                     fun=fun1,
                     ...,
                     epsilon.type=epsilon.type,
                     epsilon.value=epsilon.value)

      sim <- new[["sim"]]
      obs <- new[["obs"]]
    }

    n <- length(obs)

    if (n < k) {
      PMRval <- NA
      warning("Length of series smaller than window length 'k'")
      return(PMRval)
    } # IF end

    Qobs_mean <- mean(obs)

    if ( (Qobs_mean == 0) | is.na(Qobs_mean) ) {
      PMRval <- NA
      warning("'mean(obs)=0' => not possible to compute 'PMR'")
      return(PMRval)
    } # IF end

    mean_bias <- mean(sim) - mean(obs)

    N <- n - k + 1

    moving_bias <- numeric(N)

    for (i in seq_len(N)) {

      idx <- i:(i + k - 1)

      moving_bias[i] <-
        mean(sim[idx]) - mean(obs[idx])
    } # FOR end

    deviations <- abs(moving_bias - mean_bias)

    PMRval <- 2 * mean(deviations) / Qobs_mean

  } else {

      PMRval <- NA

      warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end

  return(PMRval)

} # 'PMR.default' END



PMR.matrix <- function(sim, obs,
                       k=NULL,
                       min.years=5,
                       days.per.year=365,
                       na.rm=TRUE,
                       fun=NULL,
                       ...,
                       epsilon.type=c("none",
                                      "Pushpalatha2012",
                                      "otherFactor",
                                      "otherValue"),
                       epsilon.value=NA) {

  epsilon.type <- match.arg(epsilon.type)

  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop(paste("Invalid argument: dim(sim) != dim(obs) ( [",
               paste(dim(sim), collapse=" "),
               "] != [",
               paste(dim(obs), collapse=" "),
               "] )",
               sep=""))

  PMRval <- rep(NA, ncol(obs))

  PMRval <- sapply(
    1:ncol(obs),
    function(i, x, y) {

      PMR.default(
        x[, i],
        y[, i],
        k=k,
        min.years=min.years,
        days.per.year=days.per.year,
        na.rm=na.rm,
        fun=fun,
        ...,
        epsilon.type=epsilon.type,
        epsilon.value=epsilon.value
      )

    },
    x=sim,
    y=obs
  ) # sapply END

  names(PMRval) <- colnames(obs)

  return(PMRval)

} # 'PMR.matriz' END


PMR.data.frame <- function(sim, obs,
                           k=NULL,
                           min.years=5,
                           days.per.year=365,
                           na.rm=TRUE,
                           fun=NULL,
                           ...,
                           epsilon.type=c("none",
                                          "Pushpalatha2012",
                                          "otherFactor",
                                          "otherValue"),
                           epsilon.value=NA) {

  epsilon.type <- match.arg(epsilon.type)

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  PMR.matrix(
    sim,
    obs,
    k=k,
    min.years=min.years,
    days.per.year=days.per.year,
    na.rm=na.rm,
    fun=fun,
    ...,
    epsilon.type=epsilon.type,
    epsilon.value=epsilon.value
  )

} # 'PMR.data.frame' END


PMR.zoo <- function(sim, obs,
                    k=NULL,
                    min.years=5,
                    days.per.year=365,
                    na.rm=TRUE,
                    fun=NULL,
                    ...,
                    epsilon.type=c("none",
                                   "Pushpalatha2012",
                                   "otherFactor",
                                   "otherValue"),
                    epsilon.value=NA) {

  epsilon.type <- match.arg(epsilon.type)

  sim <- zoo::coredata(sim)

  if (is.zoo(obs))
    obs <- zoo::coredata(obs)

  if (is.matrix(sim) | is.data.frame(sim)) {

    PMR.matrix(
      sim,
      obs,
      k=k,
      min.years=min.years,
      days.per.year=days.per.year,
      na.rm=na.rm,
      fun=fun,
      ...,
      epsilon.type=epsilon.type,
      epsilon.value=epsilon.value
    )

  } else {

    NextMethod(
      sim,
      obs,
      k=k,
      min.years=min.years,
      days.per.year=days.per.year,
      na.rm=na.rm,
      fun=fun,
      ...,
      epsilon.type=epsilon.type,
      epsilon.value=epsilon.value
    )

  }

} # 'PMR.zoo' end

