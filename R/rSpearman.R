# File rSpearman.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

# From https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient:
# In statistics, Spearman's rank correlation coefficient or Spearman's ρ, named after 
# Charles Spearman[1] and often denoted by the Greek letter \rho  (rho) or as r_{s}, 
# is a nonparametric measure of rank correlation (statistical dependence between the 
# rankings of two variables). It assesses how well the relationship between two variables
# can be described using a monotonic function.

# The Spearman correlation between two variables is equal to the Pearson correlation between 
# the rank values of those two variables; while Pearson's correlation assesses linear 
# relationships, Spearman's correlation assesses monotonic relationships (whether linear 
# or not). 

# If there are no repeated data values, a perfect Spearman correlation of +1 or −1 occurs 
# when each of the variables is a perfect monotone function of the other.

# Intuitively, the Spearman correlation between two variables will be high when observations
# have a similar (or identical for a correlation of 1) rank (i.e. relative position label 
# of the observations within the variable: 1st, 2nd, 3rd, etc.) between the two variables, 
# and low when observations have a dissimilar (or fully opposed for a correlation of −1) 
# rank between the two variables.

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################
rSpearman <-function(sim, obs, ...) UseMethod("rSpearman")

rSpearman.default <- function(sim, obs, fun=NULL, ..., 
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
  
    rSpearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs") 
    # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
    # between observed and simulated values for each variable is given by the diagonal of 'rSpearman' 
 

  } else {
           rSpearman <- NA
           warning("There are no pairs of 'sim' and 'obs' without missing values !")
         } # ELSE end
  
  return(rSpearman)
  
} # 'rSpearman.default' end



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################
rSpearman.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA){

    rSpearman <- rep(NA, ncol(obs))       
          
    rSpearman <- sapply(1:ncol(obs), function(i,x,y) { 
                        rSpearman[i] <- rSpearman.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                 }, x=sim, y=obs )            
           
    return(rSpearman)
     
} # 'rSpearman.matrix' END
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################  
rSpearman.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                                 epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                                 epsilon.value=NA){

    sim <- as.matrix(sim)
    obs <- as.matrix(obs)
	
    rSpearman.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)        
     
} # 'rSpearman.data.frame' END
  
 
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################
rSpearman.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rSpearman.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                       epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'rSpearman.zoo' end
