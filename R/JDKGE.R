# File JDKGE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2026-2026 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'JDKGE': Joint Divergence Kling-Gupta Efficiency (JDKGE)                     #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 28-Abr-2026 ; 29-Apr-2026                                           #
################################################################################
# Reference:                                                                   #
#  Ficchi, A.; Bavera, D.; Grimaldi, S.; Moschini, F.; Pistocchi, A.;          #
#  Russo, C.; Salamon, P.; Toreti, A. (2026). Improving low and high flow      #
#  simulations at once: An enhanced metric for hydrological model calibrations.#
#  EGUsphere [preprint], https://doi.org/10.5194/egusphere-2026-43.            #
################################################################################
# The Joint Divergence Kling-Gupta Efficiency (JDKGE) is a composite 
# performance metric designed to evaluate hydrological model simulations by 
# simultaneously assessing four complementary attributes of agreement between 
# simulated and observed time series: 
# - linear correlation (timing and co-variability), 
# - relative variability (dispersion), 
# - bias (systematic over- or underestimation), and 
# - similarity of the full flow distributions through a divergence measure 
#  (typically the Jensen-Shannon divergence applied to log-transformed flows).
################################################################################
# Its theoretical range of JDKGE extends from -Inf to 1, where:
# - a value of 1 indicates perfect agreement across all four components, 
#   meaning that simulated values reproduce the observed timing, variability, 
#   mean magnitude, and distributional behavior exactly; 
# - a value of 0 indicates that the combined discrepancies in these attributes
#    are of the same overall magnitude as the reference deviations embedded 
#    in the metric formulation; and
# - negative values indicate progressively poorer performance, reflecting 
#   increasing mismatch in one or more components, such that the simulation 
#   deviates substantially from the observed statistical and distributional 
#   characteristics. 
################################################################################
# In practical hydrological model evaluation, therefore, values closer to 1 
# denote higher overall fidelity of the simulation across both 
# **central tendencies and extremes**, while increasingly negative values 
# signal structural deficiencies in the model representation of the 
# hydrological regime.
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Joint Divergence Kling-Gupta Efficiency between 'sim' and 'obs'



################################################################################
# 'JDKGE': Joint Divergence Kling-Gupta Efficiency                             #
################################################################################

JDKGE <- function(sim, obs, ...) UseMethod("JDKGE")


.JDKGE_check_s <- function(s) {

  if (!identical(s, c(1, 1, 1, 1))) {
    if (length(s) != 4)
      stop("Invalid argument: length(s) must be equal to 4 !")
    if (sum(s) != 1)
      stop("Invalid argument: sum(s) must be equal to 1.0 !")
  }

} # '.JDKGE_check_s' end


.JDKGE_timestep_seconds <- function(x) {

  timestep <- 86400

  if (inherits(x, "zoo")) {

    idx <- time(x)

    if (length(idx) > 1) {
      if (inherits(idx, "POSIXt")) {
        dt <- median(diff(as.numeric(idx)), na.rm=TRUE)
      } else if (inherits(idx, "Date")) {
        dt <- median(diff(as.numeric(idx)), na.rm=TRUE) * 86400
      } else {
        dt <- NA_real_
      }

      if (is.finite(dt) && (dt > 0))
        timestep <- dt
    }

  } else if (inherits(x, "ts")) {

    freq <- stats::frequency(x)

    if (is.finite(freq) && (freq > 0))
      timestep <- (365.25 * 86400) / freq
  }

  return( timestep )

} # '.JDKGE_timestep_seconds' end


.JDKGE_resolve_epsilon <- function(sim, obs, na.rm=TRUE,
                                   epsilon.type=c("paper", "none", "Pushpalatha2012",
                                                  "otherFactor", "otherValue"),
                                   epsilon.value=NA) {

  epsilon.type <- match.arg(epsilon.type)

  if (epsilon.type %in% c("otherFactor", "otherValue")) {
    if (is.na(epsilon.value))
      stop("Missing argument: you need to provide 'epsilon.value' !")
    if (!is.numeric(epsilon.value))
      stop("Invalid argument: 'epsilon.value' must be numeric !")
  } # IF end

  if (epsilon.type == "paper") {
    positive <- c(sim[sim > 0], obs[obs > 0])

    if (length(positive) <= 0)
      return(NA_real_)

    epsilon <- min(1e-6, 0.1 * min(positive))
  } else if (epsilon.type == "Pushpalatha2012") {
      epsilon <- mean(obs, na.rm=na.rm) / 100
    } else if (epsilon.type == "otherFactor") {
        epsilon <- epsilon.value * mean(obs, na.rm=na.rm)
      } else if (epsilon.type == "otherValue") {
          epsilon <- epsilon.value
        } else epsilon <- 0

  return( epsilon )

} # '.JDKGE_resolve_epsilon' end


.JDKGE_jsd_hist <- function(sim, obs, timestep=86400, na.rm=TRUE,
                            epsilon.type=c("paper", "none", "Pushpalatha2012",
                                           "otherFactor", "otherValue"),
                            epsilon.value=NA) {

  if (length(sim) != length(obs))
    stop("Invalid argument: length(sim) != length(obs)")

  if (any(!is.finite(sim)) || any(!is.finite(obs)))
    stop("Invalid argument: 'sim' and 'obs' must be finite")

  if (any(sim < 0) || any(obs < 0))
    stop("Invalid argument: negative values are not allowed in JDKGE")

  epsilon <- .JDKGE_resolve_epsilon(sim=sim, obs=obs, na.rm=na.rm,
                                    epsilon.type=epsilon.type,
                                    epsilon.value=epsilon.value)

  if (is.na(epsilon)) {
    return(list(jsd=NA_real_, epsilon=NA_real_, nbins=NA_integer_, alpha=NA_real_))
  }

  if ((epsilon <= 0) && (any(sim == 0) || any(obs == 0))) {
    warning("Zero flows detected: choose a positive 'epsilon.type' or 'epsilon.value' to compute JDKGE", call.=FALSE)
    return(list(jsd=NA_real_, epsilon=epsilon, nbins=NA_integer_, alpha=NA_real_))
  } # IF end

  sim.log <- log(ifelse(sim == 0, epsilon, sim))
  obs.log <- log(ifelse(obs == 0, epsilon, obs))

  if (identical(sim.log, obs.log)) {
    return(list(jsd=0, epsilon=epsilon, nbins=25L, alpha=epsilon))
  }

  xmin <- min(c(sim.log, obs.log))
  xmax <- max(c(sim.log, obs.log))

  iqr.obs <- stats::IQR(obs.log, na.rm=TRUE, type=7)
  h.min   <- min(1e2 * epsilon, 1e-1)

  if (is.finite(iqr.obs) && (iqr.obs > 0)) {
    h.fd <- 2 * iqr.obs * (length(obs.log)^(-1/3))
  } else h.fd <- 0

  h <- max(h.fd, h.min)

  tsf <- timestep / 86400
  if (!is.finite(tsf) || (tsf <= 0))
    tsf <- 1

  span <- xmax - xmin
  if (!is.finite(span) || (span <= 0))
    span <- h

  n.raw  <- ceiling((tsf^(1/3)) * span / h)
  nbins  <- max(min(n.raw, 100), 25)
  breaks <- seq(xmin, xmax, length.out=nbins + 1)

  h.sim <- graphics::hist(sim.log, breaks=breaks, plot=FALSE,
                          include.lowest=TRUE, right=TRUE)
  h.obs <- graphics::hist(obs.log, breaks=breaks, plot=FALSE,
                          include.lowest=TRUE, right=TRUE)

  width <- diff(h.sim$breaks)[1]

  d.sim <- h.sim$counts / (length(sim.log) * width)
  d.obs <- h.obs$counts / (length(obs.log) * width)

  alpha <- epsilon

  p.tilde <- d.sim + alpha
  q.tilde <- d.obs + alpha

  p <- p.tilde / sum(p.tilde)
  q <- q.tilde / sum(q.tilde)
  m <- 0.5 * (p + q)

  idx.p <- (p > 0) & (m > 0)
  idx.q <- (q > 0) & (m > 0)

  jsd <- 0.5 * sum(p[idx.p] * log2(p[idx.p] / m[idx.p])) +
         0.5 * sum(q[idx.q] * log2(q[idx.q] / m[idx.q]))

  return( list(jsd=jsd, epsilon=epsilon, nbins=nbins, alpha=alpha) )

} # '.JDKGE_jsd_hist' end


JDKGE.default <- function(sim, obs,
                          s=c(1, 1, 1, 1),
                          na.rm=TRUE,
                          method=c("2012", "2009", "2021"),
                          out.type=c("single", "full"),
                          fun=NULL, ...,
                          epsilon.type=c("paper", "none", "Pushpalatha2012",
                                         "otherFactor", "otherValue"),
                          epsilon.value=NA,
                          density.method="hist",
                          nbins="paper",
                          timestep=86400) {

  #####################################
  # Checkings
  #####################################
  .JDKGE_check_s(s)

  method       <- match.arg(method)
  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  if (!identical(density.method, "hist"))
    stop("JDKGE follows the paper formulation and only supports density.method='hist'")

  if (!identical(nbins, "paper"))
    stop("JDKGE follows the paper formulation and computes histogram bins internally")

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo"))) ) {
    stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  }

  vi <- valindex(sim, obs)

  if (length(vi) <= 0) {
    warning("There are no pairs of 'sim' and 'obs' without missing values !")
    JDK <- NA_real_
    elements <- c(r=NA_real_, Beta=NA_real_, Gamma=NA_real_, Delta=NA_real_)
    if (out.type == "single") return(JDK)
    return(list(JDKGE.value=JDK, JDKGE.elements=elements))
  } # IF end

  obs <- as.numeric(obs[vi])
  sim <- as.numeric(sim[vi])

  if (!is.null(fun)) {
    fun1 <- match.fun(fun)
    new <- preproc(sim=sim, obs=obs,
                   fun=fun1, ...,
                   epsilon.type=epsilon.type,
                   epsilon.value=epsilon.value)
    sim <- new[["sim"]]
    obs <- new[["obs"]]
  } # IF end

  keep <- is.finite(sim) & is.finite(obs)
  sim  <- sim[keep]
  obs  <- obs[keep]

  if (length(sim) <= 0) {
    warning("There are no finite pairs of 'sim' and 'obs' after preprocessing !")
    JDK <- NA_real_
    elements <- c(r=NA_real_, Beta=NA_real_, Gamma=NA_real_, Delta=NA_real_)
    if (out.type == "single") return(JDK)
    return(list(JDKGE.value=JDK, JDKGE.elements=elements))
  } # IF end

  if (any(sim < 0) || any(obs < 0)) {
    warning("Negative values detected: JDKGE is defined for non-negative flows only", call.=FALSE)
    JDK <- NA_real_
    elements <- c(r=NA_real_, Beta=NA_real_, Gamma=NA_real_, Delta=NA_real_)
    if (out.type == "single") return(JDK)
    return(list(JDKGE.value=JDK, JDKGE.elements=elements))
  } # IF end

  #####################################
  # Starting computations of the JDKGE
  #####################################

  r <- stats::cor(sim, obs)

  mean.sim  <- mean(sim, na.rm=na.rm)
  mean.obs  <- mean(obs, na.rm=na.rm)
  sigma.sim <- stats::sd(sim, na.rm=na.rm)
  sigma.obs <- stats::sd(obs, na.rm=na.rm)

  Beta  <- NA_real_
  Gamma <- NA_real_
  Alpha <- NA_real_
  Delta <- NA_real_
  JDK   <- NA_real_

  if (method %in% c("2009", "2012")) {
    if (mean.obs == 0) {
      warning("Warning: 'mean(obs)==0'. Beta is undefined", call.=FALSE)
    } else Beta <- mean.sim / mean.obs
  } else {
      if (sigma.obs == 0) {
        warning("Warning: 'sd(obs)==0'. Beta.2021 is undefined", call.=FALSE)
      } else Beta <- (mean.sim - mean.obs) / sigma.obs
    } # ELSE end

  if (method == "2012") {
    if (mean.sim == 0 || mean.obs == 0 || sigma.obs == 0) {
      if (mean.sim == 0)
        warning("Warning: 'mean(sim)==0'. Gamma is undefined", call.=FALSE)
      if (mean.obs == 0)
        warning("Warning: 'mean(obs)==0'. Gamma is undefined", call.=FALSE)
      if (sigma.obs == 0)
        warning("Warning: 'sd(obs)==0'. Gamma is undefined", call.=FALSE)
    } else Gamma <- (sigma.sim / mean.sim) / (sigma.obs / mean.obs)
  } else {
      if (sigma.obs == 0) {
        warning("Warning: 'sd(obs)==0'. Alpha is undefined", call.=FALSE)
      } else Alpha <- sigma.sim / sigma.obs
      Gamma <- Alpha
    } # ELSE end


  jsd.info <- .JDKGE_jsd_hist(sim=sim, obs=obs, timestep=timestep, na.rm=na.rm,
                              epsilon.type=epsilon.type,
                              epsilon.value=epsilon.value)

  if (!is.na(jsd.info$jsd))
    Delta <- 1 - jsd.info$jsd

  vr     <- if (method == "2012") Gamma else Alpha
  br.stg <- if (method == "2021") "Beta.2021" else "Beta"
  vr.stg <- if (method == "2012") "Gamma" else "Alpha"

  if (is.finite(r) && is.finite(Beta) && is.finite(vr) && is.finite(Delta)) {
    JDK <- 1 - sqrt((s[1] * (r - 1))^2 +
                    (s[2] * (vr - 1))^2 +
                    (s[3] * (Beta - 1))^2 +
                    (s[4] * (Delta - 1))^2)
  } # IF end

  elements        <- c(r=r, Beta=Beta, vr, Delta=Delta)
  names(elements) <- c("r", br.stg, vr.stg, "Delta")

  if (out.type == "single") {
    out <- JDK
  } else out <- list(JDKGE.value=JDK, JDKGE.elements=elements)

  return( out )
} # 'JDKGE.default' end


JDKGE.matrix <- function(sim, obs,
                         s=c(1, 1, 1, 1),
                         na.rm=TRUE,
                         method=c("2012", "2009", "2021"),
                         out.type=c("single", "full"),
                         fun=NULL, ...,
                         epsilon.type=c("paper", "none", "Pushpalatha2012",
                                        "otherFactor", "otherValue"),
                         epsilon.value=NA,
                         density.method="hist",
                         nbins="paper",
                         timestep=86400) {

  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop("Invalid argument: dim(sim) != dim(obs)")

  .JDKGE_check_s(s)

  method       <- match.arg(method)
  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  JDK <- rep(NA_real_, ncol(obs))
  elements <- matrix(NA_real_, nrow=4, ncol=ncol(obs))
  if (method == "2012") {
    rownames(elements) <- c("r", "Beta", "Gamma", "Delta")
  } else if (method == "2009") {
      rownames(elements) <- c("r", "Beta", "Alpha", "Delta")
    } else rownames(elements) <- c("r", "Beta.2021", "Alpha", "Delta")

  colnames(elements) <- colnames(obs)

  if (out.type == "single") {
    out <- sapply(seq_len(ncol(obs)), function(i, x, y) {
      JDKGE.default(x[, i], y[, i],
                    s=s, na.rm=na.rm, method=method, out.type="single",
                    fun=fun, ..., epsilon.type=epsilon.type,
                    epsilon.value=epsilon.value, density.method=density.method,
                    nbins=nbins, timestep=timestep)
    }, x=sim, y=obs)
    names(out) <- colnames(obs)
    return(out)
  } # IF end

  tmp <- lapply(seq_len(ncol(obs)), function(i, x, y) {
    JDKGE.default(x[, i], y[, i],
                  s=s, na.rm=na.rm, method=method, out.type="full",
                  fun=fun, ..., epsilon.type=epsilon.type,
                  epsilon.value=epsilon.value, density.method=density.method,
                  nbins=nbins, timestep=timestep)
  }, x=sim, y=obs)

  for (i in seq_along(tmp)) {
    JDK[i] <- tmp[[i]][[1]]
    elements[, i] <- as.numeric(tmp[[i]][[2]])
  } # FOR end

  return ( list(JDKGE.value=JDK, JDKGE.elements=elements) )

} # 'JDKGE.matrix' end


JDKGE.data.frame <- function(sim, obs,
                             s=c(1, 1, 1, 1),
                             na.rm=TRUE,
                             method=c("2012", "2009", "2021"),
                             out.type=c("single", "full"),
                             fun=NULL, ...,
                             epsilon.type=c("paper", "none", "Pushpalatha2012",
                                            "otherFactor", "otherValue"),
                             epsilon.value=NA,
                             density.method="hist",
                             nbins="paper",
                             timestep=86400) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  JDKGE.matrix(sim, obs,
               s=s, na.rm=na.rm, method=method, out.type=out.type,
               fun=fun, ..., epsilon.type=epsilon.type,
               epsilon.value=epsilon.value, density.method=density.method,
               nbins=nbins, timestep=timestep)

} # 'JDKGE.data.frame' end


JDKGE.zoo <- function(sim, obs,
                      s=c(1, 1, 1, 1),
                      na.rm=TRUE,
                      method=c("2012", "2009", "2021"),
                      out.type=c("single", "full"),
                      fun=NULL, ...,
                      epsilon.type=c("paper", "none", "Pushpalatha2012",
                                     "otherFactor", "otherValue"),
                      epsilon.value=NA,
                      density.method="hist",
                      nbins="paper",
                      timestep=NA) {

  if (is.na(timestep))
    timestep <- .JDKGE_timestep_seconds(sim)

  sim <- zoo::coredata(sim)

  if (zoo::is.zoo(obs))
    obs <- zoo::coredata(obs)

  if (is.matrix(sim) || is.data.frame(sim)) {
    JDKGE.matrix(sim, obs,
                 s=s, na.rm=na.rm, method=method, out.type=out.type,
                 fun=fun, ..., epsilon.type=epsilon.type,
                 epsilon.value=epsilon.value, density.method=density.method,
                 nbins=nbins, timestep=timestep)
  } else JDKGE.default(sim, obs,
                       s=s, na.rm=na.rm, method=method, out.type=out.type,
                       fun=fun, ..., epsilon.type=epsilon.type,
                       epsilon.value=epsilon.value, density.method=density.method,
                       nbins=nbins, timestep=timestep)

} # 'JDKGE.zoo' end
