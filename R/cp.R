# File cp.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'cp': Coefficient of Persistence                                             #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 18-Dec-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          16-Jan-2023                                                         #
#          20-Jan-2024                                                         #
################################################################################

# Persistence Index (Kitadinis and Bras, 1980; Corradini et al., 1986) 
# is used to compare the model  performance agains a simple model using 
# the observed value of the previous day as the prediction for the current day.

#Kitanidis, P.K., and Bras, R.L. 1980. Real-time forecasting with a conceptual
#hydrologic model. 2. Applications and results. Water Resources Research,
#Vol. 16, No. 6, pp. 1034:1044.

# The coefficient of persistencec omparest he predictions of the model 
# with the predictions obtained by assuming that the process is a Wiener
# process(variance increasing linearly with time), in which case,
# the best estimate for the future is given by the latest measurement 
# (Kitadinis and Bras, 1980) 

# Persistence model efficiency (PME) is a normalized model evaluation statistic
# that quantifies the relative magnitude of the residual variance (noise)
# to the variance of the errors obtained by the use of a simple persistence 
# model 
# ("Ref: Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, 
#   R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic 
#   quantification of accuracy in watershed simulations. 
#   Transactions of the ASABE. 50(3):885-900.. 

# PME ranges from 0 to 1, with PME = 1 being the optimal value. 
# PME values should be larger than 0.0 to indicate a minimally acceptable
# model performance (Gupta et al., 1999

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Persistence Index Efficiency between 'sim' and 'obs'

cp <-function(sim, obs, ...) UseMethod("cp")

cp.default <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){ 

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

  # the next two lines are required for avoiding an strange behaviour 
  # of the difference function when sim and obs are time series.
  if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
  if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)

  # Checking 'epsilon.type'
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
     
     # lenght of the data sets that will be ocnsidered for the ocmputations
     n <- length(obs)
      
     denominator <- sum( ( obs[2:n] - obs[1:(n-1)] )^2 )
     
    if ( (denominator != 0) & (!is.na(denominator)) ) {      
       cp <- ( 1 - ( sum( (obs[2:n] - sim[2:n])^2 ) / denominator ) )     
     } else {
         cp <- NA
         warning("'sum((obs[2:n]-obs[1:(n-1))^2)=0' -> it is not possible to compute 'cp' !")  
       }
   } else {
         cp <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
     } # ELSE end
     
   return(cp)
     
} # 'cp.default' end


################################################################################
# 'cp': Coefficient of Persistence                                             #
################################################################################
# Started: 18-Dec-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          16-Jan-2023                                                         #
################################################################################
cp.matrix <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){ 

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  cp <- rep(NA, ncol(obs))       
          
  cp <- sapply(1:ncol(obs), function(i,x,y) { 
                 cp[i] <- cp.default( x[,i], y[,i], na.rm=na.rm, fun=fun, ..., 
                                      epsilon.type=epsilon.type,  
                                      epsilon.value=epsilon.value)
                 }, x=sim, y=obs )    
                     
   names(cp) <- colnames(obs)
     
   return(cp)
     
} # 'cp.matrix' end


################################################################################
# 'cp': Coefficient of Persistence                                             #
################################################################################
# Started: 18-Dec-2008;                                                        #
# Updates: 06-Sep-2009;                                                        #
#          16-Jan-2023                                                         #
################################################################################
cp.data.frame <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                          epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                          epsilon.value=NA){ 
 
  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  cp.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
            epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'cp.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates: 16-Jan-2023                                                         #
################################################################################
cp.zoo <- function(sim, obs, na.rm=TRUE, fun=NULL, ...,
                   epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                   epsilon.value=NA){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       cp.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'cp.zoo' end
