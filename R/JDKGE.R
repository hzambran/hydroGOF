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
# Started: 28-Abr-2026                                                         #
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


################################################################################
# Jensen-Shannon Divergence (JSD)
# It is a low-level numerical kernel that assumes probability vectors are  
# already available.
################################################################################
# JSD expects two numeric vectors representing discrete probability masses 
# (or counts that can be normalized). It performs only the divergence 
# calculation: normalization, mixture distribution construction, and evaluation 
# of the symmetric Kullback-Leibler terms. It does not know anything about 
# hydrological time series, log transformations, binning, or density estimation. 
# Because of this narrow scope, .JSD is computationally simpler, easier to test, 
# and reusable in other contexts where probability vectors are already 
# defined, for example, when histograms have been precomputed or when working 
# with categorical distributions. Conceptually, it implements the mathematical 
# core of the divergence.
################################################################################

.JSD <- function(p, q) {

  if (length(p) != length(q))
    stop("Invalid argument: length(p) != length(q)")

  if (sum(p) == 0 | sum(q) == 0)
    return(NA)

  p <- p / sum(p)
  q <- q / sum(q)

  m <- 0.5 * (p + q)

  KL <- function(a, b) {
    idx <- which(a > 0 & b > 0)
    sum(a[idx] * log(a[idx] / b[idx]))
  }

  0.5 * KL(p, m) + 0.5 * KL(q, m)

} # '.JSD' end


################################################################################
# Jensen-Shannon divergence from densities
################################################################################
# This function supports both histogram and kernel density estimation. 
# The KDE branch uses density() with a common evaluation grid to ensure 
# comparability of the two empirical distributions.
################################################################################
# This is a higher-level convenience wrapper that first estimates those 
# probability distributions from raw data using a specified density estimation 
# method (histogram or kernel density), and then computes the divergence
################################################################################
# It takes raw continuous data vectors (e.g., log-transformed flows) and is 
# responsible for estimating empirical probability distributions before calling 
# the divergence logic internally. The estimation method is controlled through 
# density.method, typically "hist" (bin counts) or "kde" (kernel density 
# estimation). Thus, .JSD_density encapsulates both the statistical estimation 
# step and the divergence calculation, making it more flexible but also more 
# computationally expensive and methodologically opinionated. It is therefore 
# more appropriate when the user wants to switch between distribution 
# representations without modifying the surrounding code.
################################################################################
.JSD_density <- function(sim, obs,
                         density.method=c("hist", "kde"),
                         nbins="Sturges",
                         ...) {

  density.method <- match.arg(density.method)

  if (density.method == "hist") {

    breaks <- graphics::hist(c(sim, obs), plot=FALSE, breaks=nbins)$breaks

    h.sim <- graphics::hist(sim, plot=FALSE, breaks=breaks)

    h.obs <- graphics::hist(obs, plot=FALSE, breaks=breaks)

    p <- h.sim$counts
    q <- h.obs$counts

  } else if (density.method == "kde") {

      rng <- range(c(sim, obs))

      d.sim <- stats::density(sim,  from=rng[1], to=rng[2], ...)

      d.obs <- stats::density(obs, from=rng[1], to=rng[2], n=length(d.sim$x), ...)

      p <- d.sim$y
      q <- d.obs$y

    } # ELSE end

  if (sum(p) == 0 | sum(q) == 0)
    return(NA_real_)

  p <- p / sum(p)
  q <- q / sum(q)

  m <- 0.5 * (p + q)

  idx1 <- (p > 0) & (m > 0)
  idx2 <- (q > 0) & (m > 0)

  jsd <- 0.5 * sum(p[idx1] * log(p[idx1] / m[idx1])) +
         0.5 * sum(q[idx2] * log(q[idx2] / m[idx2]))

  return(jsd)

} # '.JSD_density' END


################################################################################
# Default method
################################################################################

JDKGE.default <- function(sim, obs,
                          s=c(1,1,1,1),
                          na.rm=TRUE,
                          method=c("2009","2012","2021"),
                          out.type=c("single", "full"),
                          fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", 
                                        "otherFactor", "otherValue"),
                          epsilon.value=NA,
                          density.method=c("hist","kde"),
                          nbins="Sturges") {

  # Scaling factors
  if (!identical(s, c(1,1,1,1))) {
    if (length(s) != 4)
      stop("Invalid argument: length(s) must be equal to 4 !")
    if (sum(s) != 1)
      stop("Invalid argument: sum(s) must be equal to 1.0 !")
  } # IF end

  method       <- match.arg(method)
  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  if ( is.na(match(class(sim), c("integer","numeric","ts","zoo"))) |
       is.na(match(class(obs), c("integer","numeric","ts","zoo"))) )
    stop("Invalid argument type: 'sim' & 'obs' have to be numeric")

  vi <- valindex(sim, obs)

  if (length(vi) > 0) {

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

    if (any(sim <= 0) | any(obs <= 0)) {

      r   <- Beta <- Gamma <- Delta <- NA
      JDK <- NA

      warning("Non-positive values detected => log-transform not possible")

    } else {

        mean.sim <- mean(sim, na.rm=na.rm)
        mean.obs <- mean(obs, na.rm=na.rm)

        sigma.sim <- sd(sim, na.rm=na.rm)
        sigma.obs <- sd(obs, na.rm=na.rm)

        # Correlation
        r <- cor(sim, obs)

        # Bias and variability components depend on 'method'
        mean.sim <- mean(sim, na.rm=na.rm)
        mean.obs <- mean(obs, na.rm=na.rm)

        if ( (mean.obs == 0) & ( (method == "2009") | (method == "2012") ) )
          warning("Warning: 'mean(obs)==0'. Beta is undefined")

        if ( (mean.sim == 0) &  (method == "2012") )
          warning("Warning: 'mean(obs)==0'. Beta is undefined !")

        sigma.sim <- sd(sim, na.rm=na.rm)
        sigma.obs <- sd(obs, na.rm=na.rm)

        if ( (sigma.obs == 0) & ( (method == "2009") | (method == "2021") ) )
          warning("Warning: 'sd(obs)==0'. Variability ratio is undefined !")

        if (method == "2009") {

          # Variability: standard deviation ratio
          vr     <- sigma.sim / sigma.obs
          vr.stg <- "Alpha"

          # Bias: mean ratio
          br     <- mean.sim / mean.obs
          br.stg <- "Beta"

        } else if (method == "2012") {

            # Coefficient of variation ratio
            CV.sim <- sigma.sim / mean.sim
            CV.obs <- sigma.obs / mean.obs

            vr     <- CV.sim / CV.obs
            vr.stg <- "Gamma"

            # Bias unchanged
            br     <- mean.sim / mean.obs
            br.stg <- "Beta"

          } else if (method == "2021") {

              # Variability: standard deviation ratio
              vr     <- sigma.sim / sigma.obs
              vr.stg <- "Alpha"

              # Standardized bias
              br     <- (mean.sim - mean.obs) / sigma.obs
              br.stg <- "Beta.2021"

            } # END

        # Distribution component (log flows)
        log.sim <- log(sim)
        log.obs <- log(obs)

        jsd <- .JSD_density(sim=log.sim, obs=log.obs, 
                            density.method=density.method,
                            nbins=nbins, ...)

        Delta <- 1 - jsd

        if ( (mean.obs != 0) & (sigma.obs != 0) & !is.na(jsd) ) {

          JDK <- 1 - sqrt( (s[1]*(r-1))^2  + (s[2]*(vr-1))^2  +
                           (s[3]*(br-1))^2 + (s[4]*(Delta-1))^2 )

        } else {

            JDK <- NA

            if (mean.obs == 0)
              warning("Warning: 'mean(obs)==0'. Beta = Inf")

            if (sigma.obs == 0)
              warning("Warning: 'sd(obs)==0'. Gamma = Inf")

          } # ELSE end

      } # ELSE end

  } else {

      r <- Beta <- Gamma <- Delta <- NA
      JDK <- NA

      warning("There are no pairs of 'sim' and 'obs' without missing values !")

    } # ELSE end

  if (out.type == "single") {

    out <- JDK

  } else {

      elements <- c(r, br, vr, Delta)

      names(elements) <- c("r", br.stg, vr.stg, "Delta")

      out <- list( JDKGE.value = JDK, JDKGE.elements = elements )

    } # ELSE end

  return(out)

} # 'JDKGE.default' END


JDKGE.matrix <- function(sim, obs,
                         s=c(1,1,1,1),
                         na.rm=TRUE,
                         method=c("2009","2012","2021"),
                         out.type=c("single", "full"),
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", 
                                        "otherFactor", "otherValue"),
                         epsilon.value=NA,
                         density.method=c("hist","kde"),
                         nbins="Sturges") {

  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop(paste("Invalid argument: dim(sim) != dim(obs)"))

  if (!identical(s, c(1,1,1,1))) {
    if (length(s) != 4)
      stop("Invalid argument: length(s) must be equal to 4 !")
    if (sum(s) != 1)
      stop("Invalid argument: sum(s) must be equal to 1.0 !")
  } # IF end

  method       <- match.arg(method)
  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  JDK <- rep(NA, ncol(obs))

  elements <- matrix( NA, nrow=4, ncol=ncol(obs) )

  if (method == "2012") {
    vr.stg <- "Gamma"
    br.stg <- "Beta"
  } else if (method == "2009") {
      vr.stg <- "Alpha"
      br.stg <- "Beta"
    } else {
        vr.stg <- "Alpha"
        br.stg <- "Beta.2021"
      } # ELSE end

  rownames(elements) <- c("r", br.stg, vr.stg, "Delta")
  colnames(elements) <- colnames(obs)

  if (out.type == "single") {

    out <- sapply(1:ncol(obs), function(i, x, y) {

      JDKGE.default( x[,i],  y[,i], s=s, na.rm=na.rm,
        method=method, out.type="single", fun=fun,  ...,
        epsilon.type=epsilon.type, epsilon.value=epsilon.value,
        nbins=nbins
      )

    }, x=sim, y=obs)

    names(out) <- colnames(obs)

  } else {

    tmp <- lapply(1:ncol(obs), function(i, x, y) {

      JDKGE.default( x[,i], y[,i], s=s, na.rm=na.rm,
        method=method, out.type="full", fun=fun, ...,
        epsilon.type=epsilon.type, epsilon.value=epsilon.value,
        nbins=nbins
      )

    }, x=sim, y=obs)

    for (i in 1:length(tmp)) {

      JDK[i] <- tmp[[i]][[1]]

      elements[,i] <- as.numeric( tmp[[i]][[2]] )

    } # FOR end

    out <- list( JDKGE.value = JDK, JDKGE.elements = elements )

  } # ELSE end

  return(out)

} # 'JDKGE.matrix' END


################################################################################
# Author: Mauricio Zambrano-Bigiarini style adaptation                         #
################################################################################
JDKGE.data.frame <- function(sim, obs,
                             s=c(1,1,1,1),
                             na.rm=TRUE,
                             method=c("2009","2012","2021"),
                             out.type=c("single", "full"),
                             fun=NULL, ...,
                             epsilon.type=c("none", "Pushpalatha2012", 
                                            "otherFactor", "otherValue"),
                             epsilon.value=NA,
                             density.method=c("hist","kde"),
                             nbins="Sturges") {

  # Coercion to matrix 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  method       <- match.arg(method)
  out.type     <- match.arg(out.type)
  epsilon.type <- match.arg(epsilon.type)

  JDKGE.matrix(sim, obs, s=s, na.rm=na.rm,
               method=method, out.type=out.type,
               fun=fun, ...,
               epsilon.type=epsilon.type,
               epsilon.value=epsilon.value,
               nbins=nbins)

} # 'JDKGE.data.frame' END


################################################################################
# Author: Mauricio Zambrano-Bigiarini style adaptation                         #
################################################################################
JDKGE.zoo <- function(sim, obs,
                      s=c(1,1,1,1),
                      na.rm=TRUE,
                      method=c("2009","2012","2021"),
                      out.type=c("single", "full"),
                      fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", 
                                     "otherFactor", "otherValue"),
                      epsilon.value=NA,
                      density.method=c("hist","kde"),
                      nbins="Sturges") {

  # Extract core data (same logic as KGE)
  sim <- zoo::coredata(sim)

  if (is.zoo(obs))
    obs <- zoo::coredata(obs)

  if (is.matrix(sim) | is.data.frame(sim)) {

    JDKGE.matrix(sim, obs, s=s, na.rm=na.rm,
                 method=method, out.type=out.type,
                 fun=fun, ...,
                 epsilon.type=epsilon.type,
                 epsilon.value=epsilon.value,
                 nbins=nbins)

  } else {

      NextMethod(sim, obs, s=s, na.rm=na.rm,
                 method=method, out.type=out.type,
                 fun=fun, ...,
                 epsilon.type=epsilon.type,
                 epsilon.value=epsilon.value,
                 nbins=nbins)

    } # ELSE

} # 'JDKGE.zoo' END