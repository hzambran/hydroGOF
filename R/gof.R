# File gof.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2024 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008 -> 03 Feb 2009;                                         #
# Updates: 06-Sep-09                                                           #
#          2010                                                                #
#          21-Jan-2011                                                         #
#          08-May-2012                                                         #
#          14-Jan-2023 ; 15-Jan-2023 ; 16-Jan-2023                             #
#          19-Jan-2024 ; 20-Jan-2024 ; 23-Mar-2024                             #            #
################################################################################

# It computes:

# 1) 'me'         : Mean Error
# 2) 'mae'        : Mean Absolute Error
# 3) 'mse'        : Mean Square Error
# 4) 'rmse'       : Root Mean Square Error
# 5) 'ubRMSE'     : unbiased Root Mean Square Error
# 6) 'nrms'       : Normalized Root Mean Square Error
# 7) 'PBIAS'      : Percent Bias ( -1 <= PBIAS <= 1 )
# 8) 'RSR'        : Ratio of the RMSE to the standard deviation of the observations
# 9) 'rSD'        : Ratio of Standard Deviations, rSD = SD(sim) / SD(obs)
# 10) 'NSE'       : Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )
# 11) 'mNSE'      : Modified Nash-Sutcliffe Efficiency
# 12) 'rNSE'      : Relative Nash-Sutcliffe Efficiency
# 13) 'wNSE'      : Weighted Nash-Sutcliffe Efficiency
# 14) 'd'         : Index of Agreement( 0 <= d <= 1 )
# 15) 'dr'        : Refined Index of Agreement( -1 <= dr <= 1 )
# 16) 'md'        : Modified Index of Agreement( 0 <= md <= 1 )
# 17) 'rd'        : Relative Index of Agreement( 0 <= rd <= 1 )
# 18) 'cp'        : Coefficient of Persistence ( 0 <= cp <= 1 ) 
# 19) 'r'         : Pearson Correlation coefficient ( -1 <= r <= 1 )
# 20) 'R2'        : Coefficient of Determination ( 0 <= R2 <= 1 )
# 21) 'bR2'       : Weighted coefficient of determination
# 22) 'VE'        : Volumetric efficiency
# 23) 'KGE'       : Kling-Gupta efficiency (-Inf < KGE <= 1)
# 24) 'KGElf'     : Kling-Gupta efficiency with focus on low values (-Inf < KGElf <= 1)
# 25) 'KGEnp'     : Non-parametric Kling-Gupta efficiency (-Inf < KGEnp <= 1)
# 26) 'sKGE'      : Split Kling-Gupta efficiency (-Inf < sKGE <= 1)
# 27) 'rSpearman' : Spearman correlation coefficient ( -1 <= r <= 1 ) 
# 28) 'pbiasFDC'  : PBIAS in the slope of the midsegment of the Flow Duration Curve  ( 0 <= pbiasFDC ) 

gof <-function(sim, obs, ...) UseMethod("gof")

gof.default <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                        j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                        lQ.thr=0.7, hQ.thr=0.2, start.month=1, 
                        digits=2, fun=NULL, ...,
                        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                        epsilon.value=NA){

     method        <- match.arg(method)
     epsilon.type  <- match.arg(epsilon.type)
     
     ME     <- me(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     MAE    <- mae(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     MSE    <- mse(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     RMSE   <- rmse(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     ubRMSE <- ubRMSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
     NRMSE  <- nrmse(sim, obs, na.rm=na.rm, norm=norm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     RSR    <- rsr(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     rSD    <- rSD(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
     PBIAS  <- pbias(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     NSE    <- NSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     mNSE   <- mNSE(sim, obs, na.rm=na.rm, j=j, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     rNSE   <- rNSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     wNSE   <- wNSE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     d      <- d(sim, obs, na.rm=na.rm, fun=fun, ..., 
                 epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     dr     <- dr(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     md     <- md(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     rd     <- rd(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     cp     <- cp(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     r      <- .rPearson(sim, obs, fun=fun, ..., 
                         epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     R2     <- R2(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
     bR2    <- br2(sim, obs, na.rm=na.rm, fun=fun, ..., 
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
     VE     <- VE(sim, obs, na.rm=na.rm, fun=fun, ..., 
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)     
     KGE    <- KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single", 
                    fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
     KGElf  <- KGElf(sim, obs, na.rm=na.rm, s=s, method=method, 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     KGEnp  <- KGEnp(sim, obs, na.rm=na.rm, out.type="single", fun=fun, ..., 
                     epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
                     
     if ( zoo::is.zoo(sim) & zoo::is.zoo(obs) ) {
       do.sKGE <- TRUE
       sKGE    <- sKGE(sim, obs, s=s, na.rm=na.rm, method=method, 
                       start.month=start.month, out.PerYear=FALSE, fun=fun, ..., 
                       epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
     } else {
         do.sKGE <- FALSE
         sKGE <- NA
       } # ELSE end
       
     # Creating the basic output object
     gof <- rbind(  ME,   MAE,   MSE,  RMSE, ubRMSE, 
                 NRMSE, PBIAS,   RSR,   rSD,    NSE, 
                  mNSE,  rNSE,  wNSE,     d,     dr,    
                    md,    rd,    cp,     r,     R2, 
                   bR2,    VE,   KGE, KGElf,  KGEnp)   
     
     # Changing names to NRMSE and PBIAS
     rownames(gof)[6] <- "NRMSE %"
     rownames(gof)[7] <- "PBIAS %"   
     
     # Adding Split KGE
     if (do.sKGE) { # 'sKGE' is presented after 'KGElf'
       gof <- rbind(gof, sKGE) 
       rownames(gof)[length(rownames(gof))] <- "sKGE"
     } # IF end

     # Adding r.Spearman
     if (do.spearman) {
       r.Spearman <- rSpearman(sim, obs, na.rm=na.rm, fun=fun, ..., 
                               epsilon.type=epsilon.type, epsilon.value=epsilon.value) 
       gof <- rbind(gof, r.Spearman) 
       rownames(gof)[length(rownames(gof))] <- "rSpearman"
     } # IF end
     
     # Adding pbiasfdc
     if (do.pbfdc) { 
       pbfdc  <- pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, 
                          fun=fun, ..., epsilon.type=epsilon.type, epsilon.value=epsilon.value)
                          
       gof <- rbind(gof, pbfdc) 
       rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
     } # IF end
     
     # Rounding the final results, for avoiding scientific notation
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
#          15-Jan-2023                                                         #
#          19-Jan-2024                                                         #
################################################################################
gof.matrix <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                       j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                       lQ.thr=0.7, hQ.thr=0.2, start.month=1, 
                       digits=2, fun=NULL, ...,
                       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                       epsilon.value=NA){
    
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
                                        do.spearman=do.spearman, do.pbfdc=do.pbfdc, 
                                        j=j, norm=norm, s=s, method=method, 
                                        lQ.thr=lQ.thr, hQ.thr=hQ.thr, 
                                        digits=digits, fun=fun, ...,  
                                        epsilon.type=epsilon.type, 
                                        epsilon.value=epsilon.value)
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
#          15-Jan-2023                                                         #
#          19-Jan-2024                                                         #
################################################################################
gof.data.frame <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                           j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                           lQ.thr=0.7, hQ.thr=0.2, start.month=1, 
                           digits=2, fun=NULL, ...,
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  gof.matrix(sim, obs, na.rm=na.rm, do.spearman=do.spearman, do.pbfdc=do.pbfdc, 
             j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
             digits=digits, fun=fun, ...,  
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'gof.data.frame' end 


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 05-Nov-2012                                                         #
# Updates: 22-Mar-2013                                                         #
#          15-Jan-2023                                                         #
#          19-Jan-2024                                                         #
################################################################################
gof.zoo <- function(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                    j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), 
                    lQ.thr=0.7, hQ.thr=0.2, start.month=1, 
                    digits=2, fun=NULL, ...,
                    epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                    epsilon.value=NA){
    
    #sim <- zoo::coredata(sim)
    #if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       gof.matrix(sim, obs, na.rm=na.rm, do.spearman=do.spearman, do.pbfdc=do.pbfdc, 
                  j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
                  digits=digits, fun=fun, ...,  
                  epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else
        NextMethod(sim, obs, na.rm=na.rm, do.spearman=do.spearman, do.pbfdc=do.pbfdc, 
                   j=j, norm=norm, s=s, method=method, lQ.thr=lQ.thr, hQ.thr=hQ.thr,
                   digits=digits, fun=fun, ...,  
                   epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'gof.zoo' end
  
