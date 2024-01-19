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
  Om    <- mean(obs)
  SSres <- sum( (obs - sim)^2 )
  SStot <- sum( (obs - Om)^2 )
  R2    <- 1 - SSres/SStot
  return(R2)
} # '.R2' END
