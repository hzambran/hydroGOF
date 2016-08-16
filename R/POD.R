# File POD.R
# Part of the hydroGOF package, https://github.com/hzambran/hydroGOF
# Copyright 2008-2016 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016 ; 09-Mar-2016 ; 09-Aug-2016                            ##
################################################################################
# Reference: -) Jolliffe, I.T., Stephenson, D.B. (Eds.), 2003.                ##
#               Forecast verification: A practitioners guide in atmospheric   ##
#               science. John Wiley & Sons Ltd, England.                      ##
#            -) http://www.cawcr.gov.au/projects/verification/                ##
#            -) http://www.wxonline.info/topics/verif2.html                   ##
################################################################################

# The probability of detection (POD) measures the fraction of events/forecasts 
# that are correctly identified by the simulations
# An POD of 1 (POD = 1) indicates that all the events were corectly identified by the simulations. 
# A POD of 0 (POD = 0) indicates thatall the events were INcorectly identified by the simulations (i.e., a complete failure!)
# Essentially, the closer the model efficiency is to 1, the more accurate are the simulations.  

# 'obs'   : 'factor' with observed values. If 'obs' is not a factor, use 'breaks' (when provided) to classify 'obs' in categorical classes (i.e., factor)
# 'sim'   : 'factor' with simulated values. If 'sim' is not a factor, use 'breaks' (when provided) to classify 'sim' in categorical classes (i.e., factor)
# 'trgt'  : class/event to be looked for in 'sim' and 'obs' to evaluate the correct events/forecasts
# 'Result': POD between 'sim' and 'obs'

POD <-function(sim, obs, trgt=NA, ...) UseMethod("POD")

POD.default <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=FALSE, labels=NA, na.rm=TRUE, verbose=TRUE, ...){ 

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
     stop("Missing argument: You have to provide 'trgt' to compute POD !")    
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
   #H <- sum(obs==sim)
   obs.events.index <- which(obs==trgt)
   sim.events.index <- which(sim==trgt)
   H <- length( intersect(obs.events.index, sim.events.index) )

   # M: Missed
   #obs.events.index <- which(obs==trgt)
   #M <- sum(sim[obs.events.index] != obs[obs.events.index])
   obs.events.index   <- which(obs==trgt)
   sim.noevents.index <- which(sim!=trgt)
   M <- length( intersect(obs.events.index, sim.noevents.index) )

   # F: False alarms
   #sim.events.index <- which(sim==trgt)
   #FA <- sum(sim[sim.events.index] != obs[sim.events.index])
   obs.noevents.index <- which(obs!=trgt)
   sim.events.index   <- which(sim==trgt)
   FA <- length( intersect(obs.noevents.index, sim.events.index) )

   # CN: correct negative
   obs.noevents.index <- which(obs!=trgt)
   sim.noevents.index <- which(sim!=trgt)
   CN <- length( intersect(obs.noevents.index, sim.noevents.index) )

   # He:
   He <- ( H + FA ) * ( H + M ) / Ne

   # POD
   POD <- H / (H+M)
     
   return(POD)
     
} # 'POD' end


################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016                                                        ##
################################################################################
POD.matrix <- function (sim, obs, trgt=NA, breaks=NA, na.rm=TRUE, verbose=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  POD <- rep(NA, ncol(obs))       
          
  POD <- sapply(1:ncol(obs), function(i,x,y) { 
                 POD[i] <- POD.default( x[,i], y[,i], trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose, ... )
               }, x=sim, y=obs )    
                     
  names(POD) <- colnames(obs)
  
  return(POD)
     
} # 'POD.matrix' end


################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016                                                        ##
################################################################################
POD.data.frame <- function (sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  POD.matrix(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
} # 'POD.data.frame' end



################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016                                                        ##
################################################################################
POD.zoo <- function(sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       POD.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
  } # 'POD.zoo' end

