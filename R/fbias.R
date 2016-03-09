# File fbias.R
# Part of the hydroGOF package, https://github.com/hzambran/hydroGOF
# Copyright 2008-2016 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                    'fbias': Frequency Bias                                 ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
# Reference: -) Jolliffe, I.T., Stephenson, D.B. (Eds.), 2003.                ##
#               Forecast verification: A practitioners guide in atmospheric   ##
#               science. John Wiley & Sons Ltd, England.                      ##
#            -) http://www.cawcr.gov.au/projects/verification/                ##
#            -) http://www.wxonline.info/topics/verif2.html                   ##
################################################################################

# The frequency bias compares the number of events identified by the 
# simulation/forecast to the number of events actaully observed.
# It is commonly referred as \textit{bias} when there is no possible confusion 
# with other meanings of the term
# The optimal value of \textit{fBias} is 1.0 (unbiased), i.e., the event was 
# predicted by the simulations/forecast the same number of times than the 
# actual observations, while $fbias>1$ indicates an overestimation of the 
# occurrences by the simulaitons/forecast, whereas $fBias<1$ reveals the event 
# was identified by the simulaitons/forecast less times than it was actaully 
# observed.
# A fbias of 1 (fbias = 1) indicates a perfect match
# A fbias less than 1 (fbias < 1) indicates the event was identified by the simulaitons/forecast less times than it was actually observed.
# A fbias greater than 1 (fbias > 1) indicates the event was identified by the simulaitons/forecast more times than it was actually observed.


# 'obs'   : 'factor' with observed values. If 'obs' is not a factor, use 'breaks' (when provided) to classify 'obs' in categorical classes (i.e., factor)
# 'sim'   : 'factor' with simulated values. If 'sim' is not a factor, use 'breaks' (when provided) to classify 'sim' in categorical classes (i.e., factor)
# 'trgt'  : class/event to be looked for in 'sim' and 'obs' to evaluate the correct events/forecasts
# 'Result': fbias between 'sim' and 'obs'

fbias <-function(sim, obs, trgt=NA, ...) UseMethod("fbias")

fbias.default <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=FALSE, labels=NA, na.rm=TRUE, verbose=TRUE, ...){ 

   if ( is.na(match(class(sim), c("factor"))) | 
        is.na(match(class(obs), c("factor"))) 
      ) { if ( (missing(breaks)) ) {
         stop("Missing argument: 'breaks' have to be provided when 'sim' & 'obs' are not factors !")  
       } else {
          if (verbose) message("Using 'breaks' to identify factor classes....")
          if (!is.na(labels)) {
            sim <- cut(sim, breaks=breaks, include.lowest=include.lowest, labels=labels)
            obs <- cut(obs, breaks=breaks, include.lowest=include.lowest, labels=labels)
          } else {
              sim <- cut(sim, breaks=breaks, include.lowest=include.lowest)
              obs <- cut(obs, breaks=breaks, include.lowest=include.lowest)
            }
         } 
       }

   if (!(all.equal(levels(sim), levels(obs))))
     warning("'sim' and 'obs' do not have the same classes.")   

   if (missing(trgt)) {
     stop("Missing argument: You have to provide 'trgt' to compute fbias !")    
   } else {
     if ( !(trgt %in% levels(sim) ) | !(trgt %in% levels(obs) ) )
       stop("Invalid argument: 'trgt' has to be present in both 'sim' and 'obs' !")  
     } # ELSE end

   vi <- valindex(sim, obs)
     
   obs <- obs[vi]
   sim <- sim[vi]

   # Ne: Total number of events
   Ne <- length(obs)
     
   # H: Hits
   H <- sum(obs==sim)

   # M: Missed
   obs.events.index <- which(obs==trgt)
   M <- sum(sim[obs.events.index] != obs[obs.events.index])

   # F: False alarms
   sim.events.index <- which(sim==trgt)
   F <- sum(sim[sim.events.index] != obs[sim.events.index])

   # CN: correct negative
   obs.noevents.index <- which(obs!=trgt)
   sim.noevents.index <- which(sim!=trgt)
   CN <- length( intersect(obs.noevents.index, sim.noevents.index) )

   # He:
   He <- (H+F)*(H+M)/Ne

   # fbias
   fbias <- (H-F) / (H+M)
     
   return(fbias)
     
} # 'fbias' end


################################################################################
##                    'fbias': Frequency Bias                                 ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
fbias.matrix <- function (sim, obs, trgt=NA, breaks=NA, na.rm=TRUE, verbose=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  fbias <- rep(NA, ncol(obs))       
          
  fbias <- sapply(1:ncol(obs), function(i,x,y) { 
                 fbias[i] <- fbias.default( x[,i], y[,i], trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose, ... )
               }, x=sim, y=obs )    
                     
  names(fbias) <- colnames(obs)
  
  return(fbias)
     
} # 'fbias.matrix' end


################################################################################
##                    'fbias': Frequency Bias                                 ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
fbias.data.frame <- function (sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  fbias.matrix(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
} # 'fbias.data.frame' end



################################################################################
##                    'fbias': Frequency Bias                                 ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
fbias.zoo <- function(sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       fbias.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
  } # 'fbias.zoo' end

