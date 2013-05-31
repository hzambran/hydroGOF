########################################
# 'P': Coefficient of Persistence      #
########################################
# December 18th, 2008;  06-Sep-09      #
########################################
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

cp.default <- function (sim, obs, na.rm=TRUE, ...){ 

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements that are present both in 'x' and 'y' (NON- NA values)
     obs <- obs[vi]
     sim <- sim[vi]
     
     # the next two lines are required for avoiding an strange behaviour 
     # of the difference function when sim and obs are time series.
     if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
     if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)
     
     # lenght of the data sets that will be ocnsidered for the ocmputations
     n <- length(obs)
      
     denominator <- sum( ( obs[2:n] - obs[1:(n-1)] )^2 )
     
     if (denominator != 0) {
      
     cp <- ( 1 - ( sum( (obs[2:n] - sim[2:n])^2 ) / denominator ) )
     
     } else stop("'sum((obs[2:n]-obs[1:(n-1))^2)=0', it is not possible to compute 'P'")  
     
     return(cp)
     
} # 'cp.default' end


cp.matrix <- function (sim, obs, na.rm=TRUE, ...){ 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

  cp <- rep(NA, ncol(obs))       
          
  cp <- sapply(1:ncol(obs), function(i,x,y) { 
                 cp[i] <- cp.default( x[,i], y[,i], na.rm=na.rm, ... )
                 }, x=sim, y=obs )    
                     
   names(cp) <- colnames(obs)
     
   return(cp)
     
} # 'cp.matrix' end


cp.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  cp.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'cp.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
cp.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       cp.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'cp.zoo' end
