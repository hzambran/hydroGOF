# File rPearson.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2009-2023 Mauricio Zambrano-Bigiarini
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

# References:
# 1) \url{https://en.wikipedia.org/wiki/Pearson_correlation_coefficient}

# 2) Pearson, K. (1920). Notes on the history of correlation. 
#    Biometrika, 13(1), 25-45. doi:10.2307/2331722

# 3) Schober, P., Boer, C., Schwarte, L. A. (2018). Correlation coefficients: appropriate 
#   use and interpretation. Anesthesia & analgesia, 126(5), 1763-1768. 
#   doi:10.1213/ANE.0000000000002864

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 27-Oct-2009                                                         #
# Updates: 17-Jul-2016                                                         #
#          16-Jan-2023                                                         #
################################################################################
rPearson <-function(sim, obs, ...) UseMethod("rPearson")

rPearson.default <- function(sim, obs, fun=NULL, ..., 
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

  vi <- valindex(sim, obs)
     
  if (length(vi) > 0) {
	 
    obs <- as.numeric(obs[vi])
    sim <- as.numeric(sim[vi])

    if (!is.null(fun)) {
      fun1 <- match.fun(fun)
      new  <- preproc(sim=sim, obs=obs, fun=fun1, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
      sim  <- new[["sim"]]
      obs  <- new[["obs"]]
    } # IF end
  
    rPearson <- cor(sim, obs, method="pearson", use="pairwise.complete.obs")      
    # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
    # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
  
    #if ( is.matrix(r.Pearson) | is.data.frame(r.Pearson) ) {
    #r.Pearson        <- diag(r.Pearson)
    #}

  } else {
           rPearson <- NA
           warning("There are no pairs of 'sim' and 'obs' without missing values !")
         } # ELSE end
  
  return(rPearson)
  
} # 'rPearson.default' end



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 27-Oct-2009                                                         #
# Updates: 17-Jul-2016                                                         #
#          16-Jan-2023                                                         #
################################################################################
rPearson.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                            epsilon.value=NA){

    rPearson <- rep(NA, ncol(obs))       
          
    rPearson <- sapply(1:ncol(obs), function(i,x,y) { 
                 rPearson[i] <- rPearson.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
            }, x=sim, y=obs )            
           
    return(rPearson)
     
} # 'rPearson.matrix' END
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 27-Oct-2009                                                         #
# Updates: 17-Jul-2016                                                         #
#          16-Jan-2023                                                         #
################################################################################  
rPearson.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                                epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                                epsilon.value=NA){

    sim <- as.matrix(sim)
    obs <- as.matrix(obs)
	
    rPearson.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)        
     
  } # 'rPearson.data.frame' END
  
 
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 17-Jul-2016                                                         #
#          16-Jan-2023                                                         #
################################################################################
rPearson.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rPearson.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                       epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'rPearson.zoo' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 17-Jul-2016                                                         #
# Updates:                                                                     #
################################################################################
.rPearson <-function(sim, obs, ...) UseMethod("rPearson")
