##################################################
# 'wNSE': weighted Nash-Sutcliffe Efficiency     #
##################################################
# Started: March 2021                            #
##################################################
# Author: sluedtke (github user)                #
##################################################
# Updates: ( by Mauricio Zambrano-Bigiarini)     #
#          19-Jan-2024                           #
##################################################

# References:

#\cite{Nash, J. E. and J. V. Sutcliffe (1970), 
#      River flow forecasting through conceptual models part I -A discussion of principles, 
#      Journal of Hydrology, 10 (3), 282-290} \cr

#\cite{Hundecha, Y., B\'{a}rdossy, A. (2004). 
#      Modeling of the effect of land use changes on the runoff generation of a 
#      river basin through parameter regionalization of a watershed model. 
#      Journal of hydrology, 292(1-4), 281-295. 
#      doi:10.1016/j.jhydrol.2004.01.002} \cr

#\cite{Hundecha, Y., Ouarda, T. B., B\'{a}rdossy, A. (2008). 
#      Regional estimation of parameters of a rainfall‐runoff model at ungauged 
#      watersheds using the 'spatial' structures of the parameters within a canonical 
#      physiographic‐climatic space. Water Resources Research, 44(1). 
#      doi:10.1029/2006WR005439} \cr

#\cite{Hundecha, Y. and Merz, B. (2012), 
#      Exploring the Relationship between Changes in Climate and Floods Using a 
#      Model-Based Analysis, Water Resour. Res., 48(4), 1-21, 
#      doi:10.1029/2011WR010527}. 


# 'obs' : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim' : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Modified Nash-sutcliffe Efficiency between 'sim' and 'obs'

wNSE <- function(sim, obs, ...) UseMethod("wNSE")

wNSE.default <- function(sim, obs, na.rm = TRUE, fun=NULL, ..., 
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA) {

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type) 

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
         is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
      ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")

  vi <- valindex(sim, obs)

  if (length(vi) > 0) {	 
    # Filtering 'obs' and 'sim', selecting only those pairs of elements 
    # that are present both in 'x' and 'y' (NON- NA values)
    obs <- obs[vi]
    sim <- sim[vi]

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end    

    # Testing for zero values in obs, which leads to -Inf as result
    zero.index <- which(obs == 0)
    if (length(zero.index > 0)) {
        warning("'wNSE' can not be computed: some elements in 'obs' are zero !",
                call. = FALSE)
    } # IF end

    denominator <- sum(obs * ((obs - mean(obs))^2))

    if (denominator != 0) {      
      wNSE <- 1 - (sum(obs * ((obs - sim)^2)) / denominator)
    } else {
        NS <- NA
        warning("'sum( obs * ( ( obs - mean(obs) ) )^2 ) = 0', it is not possible to compute 'wNSE'")
      } 
  } else {
       NS <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end

  return(wNSE)

} # 'wNSE.default' end


wNSE.matrix <- function(sim, obs, na.rm = TRUE, fun=NULL, ..., 
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA) {

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type) 

  # Checking that 'sim' and 'obs' have the same dimensions
  if (all.equal(dim(sim), dim(obs)) != TRUE)
    stop( paste("Invalid argument: dim(sim) != dim(obs) ([",
          paste(dim(sim), collapse = " "), "] != [",
          paste(dim(obs), collapse = " "), "])", sep = ""))

  wNSE <- rep(NA, ncol(obs))

  wNSE <- sapply(1:ncol(obs), function(i, x, y) {
                 wNSE[i] <- wNSE.default(x[, i], y[, i], na.rm = na.rm, fun=fun, ..., 
                                         epsilon.type=epsilon.type, epsilon.value=epsilon.value)
               }, x = sim, y = obs)

  names(wNSE) <- colnames(obs)
  return(wNSE)

} # 'wNSE.matrix' end


wNSE.data.frame <- function(sim, obs, na.rm = TRUE, fun=NULL, ..., 
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA) { 

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type) 

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)

  wNSE.matrix(sim, obs, na.rm = na.rm, fun=fun, ..., 
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)

} # 'wNSE.data.frame' end


################################################################################
wNSE.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA) {

    # Checking 'epsilon.type'
    epsilon.type <- match.arg(epsilon.type) 

    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)

    if (is.matrix(sim) | is.data.frame(sim)) {
       wNSE.matrix(sim, obs, na.rm = na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm = na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)

  } # 'wNSE.zoo' end
