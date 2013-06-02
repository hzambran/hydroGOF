# File pbiasfdc.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'pbiasfdc': PBIAS in the slope of the midsegment of the Flow Duration Curve  #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 03-Feb-2010                                                         #
################################################################################
# Updates: 15-Apr-2013                                                         #
################################################################################
# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'lQ.thr': numeric, used to classify low flows. All the streamflows with a probaility of exceedence larger or equal to 'lQ.thr' are classified as low flows
# 'hQ.thr': numeric, used to classify high flows. All the streamflows with a probaility of exceedence lower or equal to 'hQ.thr' are classified as high flows
# 'plot'  : a logical value indicating if the flow duration curves corresponding to 'obs' and 'sim' have to be  plotted or not.

# 'Result': Percent Bias in the slope of the midsegment of the flow duration curve [%]
#           It measures the vertical soil moisture redistribution

# Ref:  Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), 
#       A process-based diagnostic approach to model evaluation: 
#       Application to the NWS distributed hydrologic model, 
#       Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716.


pbiasfdc <-function(sim, obs, ...) UseMethod("pbiasfdc")

pbiasfdc.default <- function (sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, plot=TRUE, verbose=FALSE, ...){

     # Returns the position in the vector 'x' where the scalar 'Q' is located
     Qposition <- function(x, Q) {     
       Q.dist  <- abs(x - Q)
       Q.index <- which.min( Q.dist ) 
       return(Q.index)       
     } # end

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
     vi <- valindex(sim, obs)
     
     # Filtering 'obs' and 'sim', selecting only those pairs of elements 
	 # that are present both in 'x' and 'y' (NON- NA values)
     obs <- as.numeric(obs[vi])
     sim <- as.numeric(sim[vi])
     		      
     # Computing the FDC for simulations and observations
     obs.fdc <- fdc(obs, plot=FALSE) # hydroTSM::fdc
     sim.fdc <- fdc(sim, plot=FALSE) # hydroTSM::fdc
     
     # Finding the flow value corresponding to the 'lQ.thr' pbb of excedence
     obs.lQ <- obs[Qposition(obs.fdc, lQ.thr)]
     obs.hQ <- obs[Qposition(obs.fdc, hQ.thr)]
     
     sim.lQ <- sim[Qposition(sim.fdc, lQ.thr)]
     sim.hQ <- sim[Qposition(sim.fdc, hQ.thr)]     
     
     denominator <- ( log(obs.hQ) -  log(obs.lQ) )
     
     if ( denominator > 0 ) {
     
       pbiasfdc <- 100 * ( ( ( log(sim.hQ) -  log(sim.lQ) ) / denominator ) - 1 )
     
     } else {
         pbiasfdc <- NA 
         warning("'log(obs.hQ) -  log(obs.lQ) = 0', it is not possible to compute 'pbiasfdc'") 
       } # ELSE end
     
      if (plot) {
        tmp <- as.matrix(cbind(obs, sim))
        fdc(tmp, lQ.thr=lQ.thr, hQ.thr=hQ.thr, verbose=verbose, ...)
        legend("bottomleft", legend=paste("BiasFDCms=", round(pbiasfdc,1), "%", sep=""), bty="n")
      } # IF end 
     
     return( pbiasfdc )
     
} # 'pbiasfdc.default' end
  
  
pbiasfdc.matrix <- function (sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, plot=TRUE, verbose=FALSE, ...){

    # Checking that 'sim' and 'obs' have the same dimensions
    if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

    pbiasfdc <- rep(NA, ncol(obs))       
          
    pbiasfdc <- sapply(1:ncol(obs), function(i,x,y) { 
                 pbiasfdc[i] <- pbiasfdc.default( x[,i], y[,i], lQ.thr=lQ.thr, hQ.thr=hQ.thr, na.rm=na.rm, plot=plot, verbose=verbose, ...)
            }, x=sim, y=obs )            
           
    return(pbiasfdc)  
     
  } # 'pbiasfdc.matrix' end
  

pbiasfdc.data.frame <- function (sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, plot=TRUE, verbose=FALSE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  pbiasfdc.matrix(sim, obs, lQ.thr=lQ.thr, hQ.thr=hQ.thr, na.rm=na.rm, plot=plot, verbose=verbose, ...)
     
} # 'pbiasfdc.data.frame' end 



################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
pbiasfdc.zoo <- function(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, plot=TRUE, verbose=FALSE, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       pbiasfdc.matrix(sim, obs, lQ.thr=lQ.thr, hQ.thr=hQ.thr, na.rm=na.rm, plot=FALSE, verbose=verbose, ...)
    } else NextMethod(sim, obs, lQ.thr=lQ.thr, hQ.thr=hQ.thr, na.rm=na.rm, plot=plot, verbose=verbose, ...)
     
  } # 'pbiasfdc.zoo' end
