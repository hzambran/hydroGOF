##################################################
# 'wNSE': weighted Nash-Sutcliffe Efficiency   #
##################################################
# Started: March 2021                            #
##################################################

# Ref:
# \cite{Hundecha, Y. and Merz, B. (2012), Exploring the Relationship between Changes in Climate and Floods Using a Model-Based Analysis, Water Resour. Res., 48(4), 1-21, doi: 10.1029/2011WR010527}. 

# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

wNSE <- function(sim, obs, ...) UseMethod("wNSE")

wNSE.default <- function(sim, obs, na.rm = TRUE, ...) {

   if (is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
     )stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

   vi <- valindex(sim, obs)

   obs <- obs[vi]
   sim <- sim[vi]
   denominator <- sum(obs * ((obs - mean(obs))^2))

   if (denominator != 0) {

   wNSE <- 1 - (sum(obs * ((obs - sim)^2)) / denominator)

   }else {
      wNSE <- NA
      warning("'sum( obs * ( ( obs - mean(obs) ) )^2 ) = 0', it is not possible to compute 'wNSE'")
     } # ELSE end

   return(wNSE)

} # 'wNSE.default' end


wNSE.matrix <- function(sim, obs, na.rm = TRUE, ...) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop( paste("Invalid argument: dim(sim) != dim(obs) ([",
          paste(dim(sim), collapse = " "), "] != [",
          paste(dim(obs), collapse = " "), "])", sep = ""))

  wNSE <- rep(NA, ncol(obs))

  wNSE <- sapply(1:ncol(obs), function(i, x, y) {
                 wNSE[i] <- wNSE.default(x[, i], y[, i], na.rm = na.rm, ...)
               }, x = sim, y = obs)

  names(wNSE) <- colnames(obs)
  return(wNSE)

} # 'wNSE.matrix' end


wNSE.data.frame <- function(sim, obs, na.rm = TRUE, ...) { 

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSE.matrix(sim, obs, na.rm = na.rm, ...)

} # 'wNSE.data.frame' end


################################################################################
wNSE.zoo <- function(sim, obs, na.rm=TRUE, ...) {

    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)

    if (is.matrix(sim) | is.data.frame(sim)) {
       wNSE.matrix(sim, obs, na.rm = na.rm, ...)
    } else NextMethod(sim, obs, na.rm = na.rm, ...)

  } # 'wNSE.zoo' end
