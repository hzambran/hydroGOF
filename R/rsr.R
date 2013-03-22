########################################################################
# 'rsr': Ratio of RMSE to the Standard Deviation of the Observations   #
########################################################################
#   03-Feb-2010                  #
##################################
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

rsr.default <- function (sim, obs, na.rm=TRUE, ...){

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements 
     # that are present both in 'x' and 'y' (NON- NA values)
     obs <- obs[vi]
     sim <- sim[vi]
 
     #Root mean squared error
     rmse    <- rmse(sim=sim, obs=obs, na.rm=na.rm, ...)
     
     #Standard deviation of the observations
     sd.obs <- sd(obs, na.rm=na.rm)
     
     if ( sd.obs > 0 ) {
     
       rsr <- rmse / sd.obs
     
     } else {
         rsr <- NA
         warning("'sd(obs)=0', it is not possible to compute 'RSR'")  
       } # ELSE end
     
     return( rsr )
     
} # 'rsr.default' end
  
  
rsr.matrix <- function (sim, obs, na.rm=TRUE, ...){

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

    rsr <- rep(NA, ncol(obs))       
          
    rsr <- sapply(1:ncol(obs), function(i,x,y) { 
                 rsr[i] <- rsr.default( x[,i], y[,i], na.rm=na.rm, ... )
            }, x=sim, y=obs )            
           
    return(rsr)  
     
  } # 'rsr.matrix' end
  

rsr.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  rsr.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'rsr.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
rsr.zoo <- function(sim, obs, na.rm=TRUE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       rsr.matrix(sim, obs, na.rm=na.rm, ...)
    } else NextMethod(sim, obs, na.rm=na.rm, ...)
     
  } # 'rsr.zoo' end
