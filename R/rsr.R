# File rsr.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2010-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'rsr': Ratio of RMSE to the Standard Deviation of the Observations           #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 03-Feb-2010;                                                        #
# Updates: 16-Jan-2023                                                         #
################################################################################

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Ratio of RMSE to the Standard Deviation of the Observations
#           It varies from 0 (its optimal value), which means zero RMSE and 
#           therefore a perfect model simulation, to +Inf. The lower the RSR, 
#           the better the model performance. Moriasi+al2007 suggest that 
#           a good performance is obtained for RSR < 0.7

# Ref: Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, 
#      R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic 
#      quantification of accuracy in watershed simulations. 
#      Transactions of the ASABE. 50(3):885-900.

rsr <-function(sim, obs, ...) UseMethod("rsr")

rsr.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")


  epsilon.type <- match.arg(epsilon.type)  

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
 
     #Root mean squared error
     rmse    <- rmse(sim=sim, obs=obs, na.rm=na.rm, fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA)
     
     #Standard deviation of the observations
     sd.obs <- sd(obs, na.rm=na.rm)
     
     if ( sd.obs > 0 ) {     
       rsr <- rmse / sd.obs     
     } else {
         rsr <- NA
         warning("'sd(obs)=0' -> it is not possible to compute 'RSR' !")  
       } 
   } else {
         rsr <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
     
   return( rsr )
     
} # 'rsr.default' end
  

################################################################################
# 'rsr': Ratio of RMSE to the Standard Deviation of the Observations           #
################################################################################
# Started: 03-Feb-2010;                                                        #
# Updates: 16-Jan-2023                                                         #
################################################################################  
rsr.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

    rsr <- rep(NA, ncol(obs))       
          
    rsr <- sapply(1:ncol(obs), function(i,x,y) { 
                 rsr[i] <- rsr.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                        epsilon.type=epsilon.type,  
                                        epsilon.value=epsilon.value)
            }, x=sim, y=obs )            
           
    return(rsr)  
     
  } # 'rsr.matrix' end
  

################################################################################
# 'rsr': Ratio of RMSE to the Standard Deviation of the Observations           #
################################################################################
# Started: 03-Feb-2010;                                                        #
# Updates: 16-Jan-2023                                                         #
################################################################################
rsr.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rsr.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'rsr.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
rsr.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rsr.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
} # 'rsr.zoo' end
