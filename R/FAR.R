# File FAR.R
# Part of the hydroGOF package, https://github.com/hzambran/hydroGOF
# Copyright 2008-2016 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                       'FAR': False Alarm Ratio                             ## 
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

# The false alarm ratio (FAR) measures the fraction of events/forecasts 
# that are INcorrectly identified by the simulations
# A FAR of 1 (FAR = 1) indicates that all the events were INcorectly identified by the simulations  (i.e., a complete failure!).
# A FAR of 0 (FAR = 0) indicates that all the events were corectly identified by the simulations.
# Essentially, the closer the model efficiency is to 0, the better are the simulations.  

# 'obs'   : 'factor' with observed values. If 'obs' is not a factor, use 'breaks' (when provided) to classify 'obs' in categorical classes (i.e., factor)
# 'sim'   : 'factor' with simulated values. If 'sim' is not a factor, use 'breaks' (when provided) to classify 'sim' in categorical classes (i.e., factor)
# 'trgt'  : class/event to be looked for in 'sim' and 'obs' to evaluate the correct events/forecasts
# 'Result': FAR between 'sim' and 'obs'

FAR <-function(sim, obs, trgt=NA, ...) UseMethod("FAR")

FAR.default <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=FALSE, labels=NA, na.rm=TRUE, verbose=TRUE, ...){ 

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
     stop("Missing argument: You have to provide 'trgt' to compute FAR !")    
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

   # FAR
   FAR <- F / (H+F)
     
   return(FAR)
     
} # 'FAR' end


################################################################################
##                       'FAR': False Alarm Ratio                             ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
FAR.matrix <- function (sim, obs, trgt=NA, breaks=NA, na.rm=TRUE, verbose=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  FAR <- rep(NA, ncol(obs))       
          
  FAR <- sapply(1:ncol(obs), function(i,x,y) { 
                 FAR[i] <- FAR.default( x[,i], y[,i], trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose, ... )
               }, x=sim, y=obs )    
                     
  names(FAR) <- colnames(obs)
  
  return(FAR)
     
} # 'FAR.matrix' end


################################################################################
##                       'FAR': False Alarm Ratio                             ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
FAR.data.frame <- function (sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  FAR.matrix(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
} # 'FAR.data.frame' end



################################################################################
##                       'FAR': probability of detection                      ## 
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
FAR.zoo <- function(sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       FAR.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
  } # 'FAR.zoo' end

