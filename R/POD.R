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
# Updates: 08-Mar-2016 ; 09-Mar-2016 ; 09-Aug-2016 ; 16-Aug-2016              ##
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

POD.default <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=TRUE,
                       labels=NA, na.rm=TRUE, out.type=c("single", "full"), 
                       verbose=TRUE, ...){ 

   # Checking that 'trgt' was provided 
   if (is.na(trgt)) 
     stop("Missing argument: You have to provide 'trgt' to compute PC !")    

   # Getting the initial length of 'obs'
   L.obs.ini <- length(obs)

   if ( is.na(match(class(sim), c("factor"))) | 
        is.na(match(class(obs), c("factor"))) ) { 
      if ( ( (class(breaks)=="numeric") | (class(breaks)=="integer") ) ) {
        if (verbose) message("[ Using 'breaks' to identify factor classes....]")
        if (class(labels)=="character") {
          sim <- cut(sim, breaks=breaks, right=!include.lowest, labels=labels)
          obs <- cut(obs, breaks=breaks, right=!include.lowest, labels=labels)
        } else {
            sim <- cut(sim, breaks=breaks, right=!include.lowest)
            obs <- cut(obs, breaks=breaks, right=!include.lowest)
          } # ELSE end
         if (verbose) message("[ classes(obs): ", paste(levels(obs), collapse = ", "), " ]")
      } else stop("Missing argument: 'breaks' have to be provided when 'sim' & 'obs' are not factors !")  
   } # IF end

   # Checking that 'obs' and 'sim' has the same classes
   if (!(all.equal(levels(sim), levels(obs))))
     warning("'sim' and 'obs' do not have the same classes.")   

   # Checking that 'trgt' is present in both 'sim' and 'obs'
   if ( !(trgt %in% levels(sim) ) | !(trgt %in% levels(obs) ) )
     stop("Invalid argument: 'trgt' has to be present in both 'sim' and 'obs' !")  

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
     
   # If verbose, it shows some intermediate elements of the computation
   if (verbose) message("Hits             : ", H)
   if (verbose) message("False Alarms     : ", FA)
   if (verbose) message("Missed           : ", M)
   if (verbose) message("Correct Negatives: ", CN)
   if (verbose) message("Number of points : ", Ne)
   if (L.obs.ini!=Ne) {
     if (verbose) message("Discarded points : ", L.obs.ini-Ne)
   } # IF end

  # final output
  if (out.type=="single") {
        out <- POD
  } else {
      out <- list(POD.value=POD, POD.elements=c(H, FA, M, CN, Ne, L.obs.ini-Ne))
      names(out[[2]]) <- c("Hits", "False Alarms", "Missed", "Correct Negatives", "Number of points", "Discarded points")
    } # ELSE end 
     
   return(out)
     
} # 'POD' end


################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016 ; 16-Aug-2016                                          ##
################################################################################
POD.matrix <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=TRUE,
                       labels=NA, na.rm=TRUE, out.type=c("single", "full"), 
                       verbose=FALSE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  out.type <- match.arg(out.type) 

  POD                <- rep(NA, ncol(obs))       
  elements           <- matrix(NA, nrow=6, ncol=ncol(obs))
  rownames(elements) <- c("Hits", "False Alarms", "Missed", "Correct Negatives", "Number of points", "Discarded points")
  colnames(elements) <- colnames(obs)
          
  if (out.type=="single") {
    out <- sapply(1:ncol(obs), function(i,x,y) { 
                   POD[i] <- POD.default( x[,i], y[,i], trgt=trgt, breaks=breaks, 
                                      include.lowest=include.lowest, labels=labels, 
                                      na.rm=na.rm, out.type=out.type, 
                                      verbose=verbose, ... )
                 }, x=sim, y=obs )  
    names(out) <- colnames(obs) 
  } else { out <- lapply(1:ncol(obs), function(i,x,y) { 
                         POD.default( x[,i], y[,i], trgt=trgt, breaks=breaks, 
                                      include.lowest=include.lowest, labels=labels, 
                                      na.rm=na.rm, out.type=out.type, 
                                      verbose=verbose, ... )
                       }, x=sim, y=obs ) 
            for (i in 1:length(out) ) {
               POD[i] <- out[[i]][[1]]
               elements[,i] <- as.numeric(out[[i]][[2]])
            } # FOR end 
            out <- list(POD.value=POD, POD.elements=elements)
          } # ELSE end  

  return(out)
     
} # 'POD.matrix' end


################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016  ; 16-Aug-2016                                         ##
################################################################################
POD.data.frame <- function (sim, obs, trgt=NA, breaks=NA, include.lowest=TRUE,
                       labels=NA, na.rm=TRUE, out.type=c("single", "full"), 
                       verbose=FALSE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  out.type <- match.arg(out.type) 
   
  POD.matrix(sim, obs, trgt=trgt, breaks=breaks, include.lowest=include.lowest, 
            labels=labels, na.rm=na.rm, out.type=out.type, verbose=verbose, ... )
     
} # 'POD.data.frame' end



################################################################################
##                       'POD': probability of detection                      ## 
##                           (also known as Hit Rate)                         ##
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                        ##
################################################################################
# Created: 06-Mar-2016                                                        ##
# Updates: 08-Mar-2016 ; 16-Aug-2016                                          ##
################################################################################
POD.zoo <- function(sim, obs, trgt=NA, breaks=NA, include.lowest=TRUE,
                       labels=NA, na.rm=TRUE, out.type=c("single", "full"), 
                       verbose=TRUE, ...){ 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       POD.matrix(sim, obs, trgt=trgt, breaks=breaks, include.lowest=include.lowest, 
                 labels=labels, na.rm=na.rm, out.type=out.type, verbose=FALSE, ... )
    } else NextMethod(sim, obs, trgt=trgt, breaks=breaks, include.lowest=include.lowest, 
                      labels=labels, na.rm=na.rm, out.type=out.type, verbose=verbose, ... )
     
  } # 'POD.zoo' end

