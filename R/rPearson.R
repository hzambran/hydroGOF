# File rPearson.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2009-2016 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

# Before Oct 27th 2009, this function was included in 'gof' function
# Up to 17-Jul2016, this function was a hidden function 

# The 'r.Pearson' coefficient ranges from -1 to 1. 
# A value of 1 shows that a linear equation describes the relationship 
# perfectly and positively, with all data points lying on the same line 
# and with Y increasing with X. 
# A score of -1 shows that all data points lie on a single line but 
# that Y increases as X decreases. 
# A value of 0 shows that a linear model is not needed, i.e., that there 
# is no linear relationship between the variables.

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 27-Oct-2009                                                         #
# Updates: 17-Jul-2016                                                         #
################################################################################
rPearson <-function(sim, obs, ...) UseMethod("rPearson")

rPearson.default <- function(sim, obs,...) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  
  rPearson <- cor(sim, obs, method="pearson", use="pairwise.complete.obs")      
  # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
  # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
  
  #if ( is.matrix(r.Pearson) | is.data.frame(r.Pearson) ) {
  #r.Pearson        <- diag(r.Pearson)
  #}
  
  return(rPearson)
  
} # 'rPearson.default' end

rPearson.matrix <- function (sim, obs, na.rm=TRUE, ...){

    rPearson <- rep(NA, ncol(obs))       
          
    rPearson <- sapply(1:ncol(obs), function(i,x,y) { 
                 rPearson[i] <- rPearson.default( x[,i], y[,i], na.rm=na.rm, ... )
            }, x=sim, y=obs )            
           
    return(rPearson)
     
  } # 'rPearson.matrix' END
  
  
rPearson.data.frame <- function (sim, obs, na.rm=TRUE, ...){

    sim <- as.matrix(sim)
    obs <- as.matrix(obs)
	
    rPearson.matrix(sim, obs, na.rm=na.rm, ...)        
     
  } # 'rPearson.data.frame' END
  
 
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 17-Jul-2016                                                         #
################################################################################
rPearson.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rPearson.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'rPearson.zoo' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 17-Jul-2016                                                         #
# Updates:                                                                     #
################################################################################
.rPearson <-function(sim, obs, ...) UseMethod("rPearson")
