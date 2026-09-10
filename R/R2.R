# File R2.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2024-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

# The coefficient of determination (R2) is computed as the squared Pearson
# product-moment correlation coefficient between simulated and observed values.
# It ranges from 0 to 1 when the correlation is defined.

# References:
#1) https://en.wikipedia.org/wiki/Coefficient_of_determination

#2) Box, G. E. (1966). Use and abuse of regression. Technometrics, 8(4), 625-629. 
#   doi:10.1080/00401706.1966.10490407.

#3) Hahn, G. J. (1973). The coefficient of determination exposed. Chemtech, 3(10), 609-612. 
#   Aailable online at: \url{https://www2.hawaii.edu/~cbaajwe/Ph.D.Seminar/Hahn1973.pdf}.

#4) Barrett, J. P. (1974). The coefficient of determination-some limitations. 
#   The American Statistician, 28(1), 19-20. doi:10.1080/00031305.1974.10479056.

################################################################################
# 'R2': coefficient of determination                                           #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# It was originated in the following github issue:                             #
#   https://github.com/hzambran/hydroGOF/issues/16#issue-1736556320            #
################################################################################
# Started: 29-Nov-2023                                                         #
# Updates: 19-Jan-2024                                                         #
################################################################################
R2 <-function(sim, obs, ...) UseMethod("R2")

R2.default <- function(sim, obs, fun=NULL, ..., 
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA) {

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     
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

    r  <- cor(sim, obs, method="pearson", use="pairwise.complete.obs")
    R2 <- r^2
  
  } else {
           R2 <- NA
           warning("There are no pairs of 'sim' and 'obs' without missing values !")
         } # ELSE end
  
  return(R2)
  
} # 'R2.default' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################
R2.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){

    R2 <- rep(NA, ncol(obs))       
          
    R2 <- sapply(1:ncol(obs), function(i,x,y) { 
                        R2[i] <- R2.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                 }, x=sim, y=obs )            
           
    return(R2)
     
} # 'R2.matrix' END
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################  
R2.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){

    sim <- as.matrix(sim)
    obs <- as.matrix(obs)
	
    R2.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
              epsilon.type=epsilon.type, epsilon.value=epsilon.value)        
     
} # 'R2.data.frame' END
  
 
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jan-2024                                                         #
# Updates:                                                                     #
################################################################################
R2.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ..., 
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       R2.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                       epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'R2.zoo' end
