################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# It was originated in the following github issue:                             #
#   https://github.com/hzambran/hydroGOF/issues/16#issue-1736556320            #
################################################################################
# Started: 29-Nov-2023                                                         #
# Updates: 19-Jan-2024                                                         #
################################################################################
.R2 <- function(sim, obs, fun=NULL, ...,
                epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                epsilon.value=NA) {

  # index of those elements that are present both in 'sim' and 'obs' (NON- NA values)
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
  } # IF end    

  Om    <- mean(obs)
  SSres <- sum( (obs - sim)^2 )
  SStot <- sum( (obs - Om)^2 )
  R2    <- 1 - SSres/SStot
  return(R2)
} # '.R2' END
