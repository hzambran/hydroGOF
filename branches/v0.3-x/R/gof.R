# File gof.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008 -> 03 Feb 2009;                                         #
# Updates: 06-Sep-09                                                           #
#          2010                                                                #
#          21-Jan-2011                                                         #
#          08-May-2012                                                         #
################################################################################

# It computes:
# 'me'        : Mean Error
# 'mae'       : Mean Absolute Error
# 'rms'       : Root Mean Square Error
# 'nrms'      : Normalized Root Mean Square Error
# 'r'         : Pearson Correlation coefficient ( -1 <= r <= 1 )
# 'r.Spearman': Spearman Correlation coefficient ( -1 <= r <= 1 ) 
# 'R2'        : Coefficient of Determination ( 0 <= r2 <= 1 )
#               Gives the proportion of the variance of one variable that
#               that is predictable from the other variable
# 'rSD'       : Ratio of Standard Deviations, rSD = SD(sim) / SD(obs)
# 'RSR'       : Ratio of the RMSE to the standard deviation of the observations
# 'NSE'       : Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )
# 'mNSE'      : Modified Nash-Sutcliffe Efficiency
# 'rNSE'      : Relative Nash-Sutcliffe Efficiency
# 'd'         : Index of Agreement( 0 <= d <= 1 )
# 'md'        : Modified Index of Agreement( 0 <= md <= 1 )
# 'rd'        : Relative Index of Agreement( 0 <= md <= 1 )
# 'PI'        : Persistence Index ( 0 <= PI <= 1 ) 
# 'PBIAS'     : Percent Bias ( -1 <= PBIAS <= 1 )
# 'bR2'       : weighted coefficient of determination
# 'KGE'       : Kling-Gupta efficiency
# 'VE'        : Volumetric efficiency

gof <-function(sim, obs, ...) UseMethod("gof")

gof.default <- function (sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                         j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                         lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){

     method   <- match.arg(method)
     
     ME     <- me(sim, obs, na.rm=na.rm)
     MAE    <- mae(sim, obs, na.rm=na.rm)
     MSE    <- mse(sim, obs, na.rm=na.rm)
     RMSE   <- rmse(sim, obs, na.rm=na.rm) 
     NRMSE  <- nrmse(sim, obs, na.rm=na.rm, norm=norm)
     RSR    <- rsr(sim, obs, na.rm=na.rm, ...)
     rSD    <- rSD(sim, obs, na.rm=na.rm)     
     PBIAS  <- pbias(sim, obs, na.rm=na.rm, ...)
     NSE    <- NSE(sim, obs, na.rm=na.rm, ...)
     mNSE   <- mNSE(sim, obs, na.rm=na.rm, j=j, ...)
     rNSE   <- rNSE(sim, obs, na.rm=na.rm, ...)
     d      <- d(sim, obs, na.rm=na.rm, ...)
     md     <- md(sim, obs, na.rm=na.rm, ...)
     rd     <- rd(sim, obs, na.rm=na.rm, ...)
     cp     <- cp(sim, obs, na.rm=na.rm, ...)
     r      <- .rPearson(sim, obs)
     bR2    <- br2(sim, obs, na.rm=na.rm, ...)     
     KGE    <- KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", ...) 
     VE     <- VE(sim, obs, na.rm=na.rm, ...)     
     
     # 'R2' is the Coefficient of Determination
     # The coefficient of determination, R2, is useful because it gives the proportion of
     # the variance (fluctuation) of one variable that is predictable from the other variable.
     # It is a measure that allows us to determine how certain one can be in making
     # predictions from a certain model/graph.
     # The coefficient of determination is the ratio of the explained variation to the total
     # variation.
     # The coefficient of determination is such that 0 <  R2 < 1,  and denotes the strength
     # of the linear association between x and y. 
     R2 <- r^2
      
     if (do.spearman) {
       r.Spearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs") 
     
       # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
       # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson' 
       if ( is.matrix(r.Spearman) | is.data.frame(r.Spearman) ) {
         r.Spearman        <- diag(r.Spearman)
        } # IF end
        
     } # IF end
     
     if (do.pbfdc) { pbfdc  <- pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, ...) }
     
     gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, md, rd, cp, r, R2, bR2, KGE, VE)     
     
     rownames(gof)[5] <- "NRMSE %"
     rownames(gof)[6] <- "PBIAS %"    
     
     if (do.spearman) { gof <- rbind(gof, r.Spearman) }
     
     if (do.pbfdc) { 
       gof <- rbind(gof, pbfdc) 
       rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
     } # IF end
     
     # Rounding the final results, ofr avoiding scientific notation
     gof <- round(gof, digits)
     
     return(gof)
     
} # 'gof.default' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008 -> 03 Feb 2009;                                         #
# Updates: 06-Sep-09                                                           #
#          2010                                                                #
#          21-Jan-2011                                                         #
#          08-May-2012                                                         #
################################################################################
gof.matrix <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                       j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                       lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){
    
    # Temporal variable for some computations
    tmp <- gof(1:10,1:10)
    
    # Number of objective functions currently computed by gof
    ngof <- nrow(tmp) 
    
    # Name of the objective functions computed by 'gof'
    gofnames <- rownames(tmp)

    # Creating the matrix that will store the final results
    gof <- matrix(NA, ncol(obs), nrow=ngof)   
       
    # Computing the goodness-of-fit measures for each column of 'sim' and 'obs'      
    gof <- sapply(1:ncol(obs), function(i,x,y) { 
                 gof[, i] <- gof.default( x[,i], y[,i], na.rm=na.rm, 
                                        do.spearman=do.spearman, do.pbfdc=FALSE, 
                                        j=j, norm=norm, s=s, method=method, 
                                        lQ.thr=lQ.thr, hQ.thr=hQ.thr, 
                                        digits=digits, ... )
            }, x=sim, y=obs )            
     
    rownames(gof) <- gofnames
    colnames(gof) <- colnames(sim)
           
    return(gof)  
     
  } # 'gof.matrix' end
  

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008 -> 03 Feb 2009;                                         #
# Updates: 06-Sep-09                                                           #
#          2010                                                                #
#          21-Jan-2011                                                         #
#          08-May-2012 ;                                                       #
################################################################################
gof.data.frame <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                           j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                           lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  gof.matrix(sim, obs, na.rm=na.rm, do.spearman=do.spearman, do.pbfdc=FALSE, 
             j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
             digits=digits, ...)
     
} # 'gof.data.frame' end 


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 05-Nov-2012                                                         #
# Updates: 22-Mar-2013                                                         #
################################################################################
gof.zoo <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                    j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                    lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       gof.matrix(sim, obs, na.rm=na.rm, do.spearman=FALSE, do.pbfdc=FALSE, 
             j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
             digits=digits, ...)
    } else
        NextMethod(sim, obs, na.rm=na.rm, do.spearman=FALSE, do.pbfdc=FALSE, 
                   j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
                   digits=digits, ...)
     
  } # 'gof.zoo' end
  
