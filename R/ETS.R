# File ETS.R
# Part of the hydroGOF package, https://github.com/hzambran/hydroGOF
# Copyright 2008-2016 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
##                    'ETS': Equitable Threat Score                           ## 
##                  (also known as Gilbert's Skill Score)                     ##
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

# The equitable threat score (ETS) was developed as a modification to CSI, to 
# take into account the number of hits that would hae been occurred purely by chance
# ETS ranges form -1/3 to 1, with 1 being its optimum value
# A ETS of -0 (ETS = 0) indicates no skill
# A ETS of 1 (ETS = 1) indicates a perfect match

# 'obs'   : 'factor' with observed values. If 'obs' is not a factor, use 'breaks' (when provided) to classify 'obs' in categorical classes (i.e., factor)
# 'sim'   : 'factor' with simulated values. If 'sim' is not a factor, use 'breaks' (when provided) to classify 'sim' in categorical classes (i.e., factor)
# 'trgt'  : class/event to be looked for in 'sim' and 'obs' to evaluate the correct events/forecasts
# 'Result': ETS between 'sim' and 'obs'

ETS <-function(sim, obs, trgt=NA, ...) UseMethod("ETS")

ETS.default <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=FALSE, labels=NA, na.rm=TRUE, verbose=TRUE, ...){ 

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
     stop("Missing argument: You have to provide 'trgt' to compute ETS !")    
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

   # ETS
   ETS <- (H-He) / (H+F+M-He)
     
   return(ETS)
     
} # 'ETS' end


################################################################################
##                    'ETS': Equitable Threat Score                           ## 
##                  (also known as Gilbert's Skill Score)                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
ETS.matrix <- function (sim, obs, trgt=NA, breaks=NA, na.rm=TRUE, verbose=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  ETS <- rep(NA, ncol(obs))       
          
  ETS <- sapply(1:ncol(obs), function(i,x,y) { 
                 ETS[i] <- ETS.default( x[,i], y[,i], trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose, ... )
               }, x=sim, y=obs )    
                     
  names(ETS) <- colnames(obs)
  
  return(ETS)
     
} # 'ETS.matrix' end


################################################################################
##                    'ETS': Equitable Threat Score                           ## 
##                  (also known as Gilbert's Skill Score)                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
ETS.data.frame <- function (sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  ETS.matrix(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
} # 'ETS.data.frame' end



################################################################################
##                    'ETS': Equitable Threat Score                           ## 
##                  (also known as Gilbert's Skill Score)                     ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 09-Mar-2016                                                        ##
# Updates:                                                                    ##
################################################################################
ETS.zoo <- function(sim, obs, trgt, breaks, na.rm=TRUE, verbose=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       ETS.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, trgt=trgt, breaks=breaks, na.rm=na.rm, verbose=verbose,...)
     
  } # 'ETS.zoo' end

