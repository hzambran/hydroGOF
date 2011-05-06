##############################################
# 'nrmse': Normalized Root Mean Square Error #
############################################## 
#   15-Dic-2008; 06-Sep-09    #
############################### 
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'norm'  : character, indicating the value to be used to normalise the RMS. Valid values are:
#           -) 'sdobs' : standard deviation of observations.
#           -) 'maxmin': difference between maximum and minimum observed values

# 'Result': Normalized Root Mean Square Error between 'sim' and 'obs', 
#           when multiplied by 100 its units is %

nrmse <-function(sim, obs, ...) UseMethod("nrmse")
 
nrmse.default <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

    # Checking that the user provied a valid argument for 'norm'       
    if (is.na(match(norm, c("sd", "maxmin") ) ) ) 
       stop("Invalid argument: 'norm' must be in c('sd', 'maxmin')")
       
    if (norm=="sd") {
      cte <- sd(obs, na.rm=na.rm)
    } else if (norm=="maxmin") {
        cte <- ( max(obs, na.rm= na.rm) - min(obs, na.rm =na.rm) )
      } # ELSE end

     rmse <- rmse(sim, obs, na.rm) 
     
     if (max(obs, na.rm= na.rm) - min(obs, na.rm= na.rm) != 0) {
     
       nrmse <- rmse / cte
     
     } else stop("'obs' is constant, it is not possible to compute 'nrmse'")  
     
     return( round( 100*nrmse, 1) )
     
  } # 'nrmse.default' end
  
  
nrmse.matrix <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
          
  # Checking that the user provied a valid argument for 'norm'       
  if (is.na(match(norm, c("sdobs", "maxmin") ) ) ) 
     stop("Invalid argument: 'norm' must be in c('sd', 'maxmin')")

  nrmse <- rep(NA, ncol(obs))       
          
  nrmse <- sapply(1:ncol(obs), function(i,x,y) { 
                 nrmse[i] <- nrmse.default( x[,i], y[,i], na.rm=na.rm, norm=norm, ... )
               }, x=sim, y=obs )    
                     
  names(nrmse) <- colnames(obs)
  
  return(nrmse)
     
} # 'nrms.matrix' end


nrmse.data.frame <- function (sim, obs, na.rm=TRUE, norm="sd", ...) {

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  nrmse.matrix(sim, obs, na.rm=na.rm, norm=norm, ...)
     
} # 'nrmse.data.frame' end
  
