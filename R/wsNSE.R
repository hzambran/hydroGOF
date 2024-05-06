# File wsNSE.R
# Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF
#                                 https://cran.r-project.org/package=hydroGOF
#                                 http://www.rforge.net/hydroGOF/ ;
# Copyright 2008-2023 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'wsNSE': weighted seasonal Nash-Sutcliffe Efficiency                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
# Following the traditional Nash-Sutcliffe efficiency, wsNSE ranges from 
# -Inf to 0, with an optimal value of 1

# This function is designed to drive the calibration of hydrological models 
# focused in the reproduction of high- or -low-flow events.

# It was proposed by Zambrano-Bigiarini and Bellin (2012), inspired by the well  
# known Nash-Sutcliffe efficiency (NSE, Nash and Sutcliffe, 1970), and the  
# commentaries made by Schaefli and Gupta (2007) and Criss and Winston (2008).

# Refs:
# 1) Nash, J.E.; J.V. Sutcliffe. (1970). River flow forecasting through 
#    conceptual models. Part 1: a discussion of principles, Journal of Hydrology
#    10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6
# 2) Zambrano-Bigiarini, M.; Bellin, A. (2012). Comparing goodness-of-fit 
#    measures for calibration of models focused on extreme events. 
#    EGU General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1.
# 3) Schaefli, B.; Gupta, H. (2007). Do Nash values have value?. 
#    Hydrological Processes 21, 2075-2080. doi:10.1002/hyp.6825
# 4) Criss, R. E.; Winston, W. E. (2008), Do Nash values have value? 
#    Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. 
#    doi:10.1002/hyp.7072
# 5) Yilmaz, K. K.; Gupta, H. V.; Wagener, T. (2008), A process-based
#    diagnostic approach to model evaluation: Application to the NWS
#    distributed hydrologic model, Water Resour. Res., 44, W09417,
#    doi:10.1029/2007WR006716.
# 6) Krause, P.; Boyle, D. P.; Base, F. (2005). Comparison of different 
#    efficiency criteria for hydrological model assessment, 
#    Adv. Geosci., 5, 89-97. doi:10.5194/adgeo-5-89-2005.
# 7) Legates, D. R.; G. J. McCabe Jr. (1999), Evaluating the Use of 
#    "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, 
#    Water Resources Research, 35(1), 233--241. doi:10.1029/1998WR900018.




# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
#         : numeric, representing an arbitrary value used to power the 
#           differences between observations and simulations. By default j=2,
#           which mimics the traditional Nash-Sutcliffe function, mainly focused 
#           on thr representation of high flows. For low flows, suggested values 
#           for 'j' are 1, 1/2 or 1/3. See Legates and McCabe, (1999) and 
#           Krausse et al. (2005) for a discussion of suggested vbalues of 'j'.
# 'lambda': number in [0, 1] representing the weight given to the high-flow part 
#           of the signal, with 'lambda' close to 1 when focusing on high-flow
#           events, and 'lambda' close to zero when focusing in low-flow events
# 'lQ.thr': numeric, representing the exceedence probability used to identify 
#           low-flows in the flow duration curve. See Yilmaz et al., (2008).
#           All the values to the right of it are deemed as low flows. 
#           Default value is 0.7.
# 'hQ.thr': numeric, representing the exceedence probability used to identify 
#           high-flows in the flow duration curve. See Yilmaz et al., (2008).
#           All the values to the left of it are deemed as high flows. 
#           Default value is 0.2.
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': weighted seasonal Nash-sutcliffe Efficiency between 'sim' and 'obs'

wsNSE <-function(sim, obs, ...) UseMethod("wsNSE")

wsNSE.default <- function (sim, obs, na.rm=TRUE, 
                           j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
                           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                           epsilon.value=NA){ 



  lweight <- function(x, llambda, llQ, lhQ, lna.rm=TRUE) {

    w <- ifelse(x >= lhQ, lambda, 
                ifelse(x  <= llQ, 1 - llambda, 
                (1 - llambda) + (2*llambda - 1) * (x - llQ) / (lhQ - llQ) ) 
                )
        
    return(w)
  } #'lweight' END


  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type) 

  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo", "xts"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo", "xts")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo', 'xts')")      

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

    # Number of observations
    n <- length(obs) 

    # Low and high-flow threshold values
    hQ <- stats::quantile(obs, probs=1-hQ.thr, na.rm=na.rm)
    lQ <- stats::quantile(obs, probs=1-lQ.thr, na.rm=na.rm)

    # Computing annual hfb values
    w <-  lweight(x=obs, llambda=lambda, llQ=lQ, lhQ=hQ, lna.rm=na.rm)
     
    denominator <- sum( abs(w*(obs - mean(obs)))^j )
     
    if ( (denominator != 0) & (!is.na(denominator)) ) {      
      wsNSE <- 1 - ( sum( abs(w*(obs - sim))^j ) / denominator )     
    } else {
        wsNSE <- NA
        warning("'sum( abs( w*(obs - mean(obs)) )^j ) = 0' => it is not possible to compute 'wsNSE'")  
      } 
  } else {
       wsNSE <- NA
       warning("There are no pairs of 'sim' and 'obs' without missing values !")
    } # ELSE end
     
  return(wsNSE)
     
} # 'wsNSE' end



################################################################################
# 'wsNSE': weighted seasonal Nash-Sutcliffe Efficiency                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
wsNSE.matrix <- function(sim, obs, na.rm=TRUE, 
                         j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){ 

  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
 
  wNSE <- rep(NA, ncol(obs))       
          
  wNSE <- sapply(1:ncol(obs), function(i,x,y) { 
                 wNSE[i] <- wsNSE.default( x[,i], y[,i], na.rm=na.rm, 
                                           j=j, lambda=lambda, lQ.thr=lQ.thr, 
                                           hQ.thr=hQ.thr, fun=fun, ..., 
                                           epsilon.type=epsilon.type, epsilon.value=epsilon.value)
               }, x=sim, y=obs )    
                     
  names(wNSE) <- colnames(obs)
  
  return(wNSE)
     
} # 'wsNSE.matrix' end



################################################################################
# 'wsNSE': weighted seasonal Nash-Sutcliffe Efficiency                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
wsNSE.data.frame <- function(sim, obs, na.rm=TRUE, 
                             j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
                             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                             epsilon.value=NA){ 
 
  # Checking 'epsilon.type'
  epsilon.type <- match.arg(epsilon.type)  

  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  wsNSE.matrix(sim, obs, na.rm=na.rm, j=j, lambda=lambda, lQ.thr=lQ.thr, 
               hQ.thr=hQ.thr, fun=fun, ..., 
               epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
} # 'wsNSE.data.frame' end



################################################################################
# 'wsNSE': weighted seasonal Nash-Sutcliffe Efficiency                         #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: 04-May-2024                                                         #
# Updates: 05-May-2024                                                         #
################################################################################
wsNSE.zoo <- function(sim, obs, na.rm=TRUE, 
                      j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){ 
    
    # Checking 'epsilon.type'
    epsilon.type <- match.arg(epsilon.type) 

    #sim <- zoo::coredata(sim)
    #if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       wsNSE.matrix(sim, obs, na.rm=na.rm, j=j, lambda=lambda, lQ.thr=lQ.thr, 
                    hQ.thr=hQ.thr, fun=fun, ..., 
                    epsilon.type=epsilon.type, epsilon.value=epsilon.value)
    } else NextMethod(sim, obs, na.rm=na.rm, j=j, lambda=lambda, lQ.thr=lQ.thr, 
                      hQ.thr=hQ.thr, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)
     
  } # 'wsNSE.zoo' end

