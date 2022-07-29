########################################
# 'IoA': Index of Agreement            #
########################################
# December 18th, 2008;  06-Sep-09      #
# 28-Feb-2016                          #
# Updates: 20-Jul-2022 ; 29-Jul-2022   #
########################################
# 1) Willmott, C.J., Robeson, S.M. and Matsuura, K. (2012). A refined index of model performance. International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419.
# 2) Willmott, C.J., Robeson, S.M., Matsuura, K. and Ficklin, D.L. (2015). Assessment of three dimensionless measures of model performance. Environmental Modelling & Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012
# 3) Willmott, C.J. 1981. On the validation of models. Physical Geography, 2, 184-194
# 4) Willmott, C. J. (1984). "On the evaluation of model performance in physical geography." Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460.
# 5) Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241. 


# Index of Agreement (Willmott et al., 1984) range from 0.0 to 1.0 
# and the closer to 1 the better the performance of the model 

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values

# 'Result': Index of Agreement between 'sim' and 'obs'

dr <-function(sim, obs, ...) UseMethod("dr")

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jul-2022                                                         #
# Updates: 29-Jul-2022                                                         #
################################################################################
dr.default <- function(sim, obs, na.rm=TRUE,
                      fun=NULL, ...,
                      epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                      epsilon.value=NA){ 

     if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")

     epsilon.type <- match.arg(epsilon.type)

     # index of those elements that are present both in 'x' and 'y' (NON- NA values)
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
     
       # the next two lines are required for avoiding an strange behaviour 
       # of the difference function when sim and obs are time series.
       if ( !is.na(match(class(sim), c("ts", "zoo"))) ) sim <- as.numeric(sim)
       if ( !is.na(match(class(obs), c("ts", "zoo"))) ) obs <- as.numeric(obs)
     
       # Mean of the observed values
       Om <- mean(obs, na.rm=na.rm)

       # Constant 'c' value
       c <- 2

       # Components of the denominator
       A <- sum( abs(sim - obs) )
       B <- c*sum( abs(obs - Om) )
     
       if (A <= B) {      
         dr <- 1 - A / B     
       } else dr <- 1 - B / A 
     } else {
         dr <- NA
         warning("There are no pairs of 'sim' and 'obs' without missing values !")
       } # ELSE end
     
     return(dr) 
     
} # 'dr.default' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jul-2022                                                         #
# Updates: 29-Jul-2022                                                         #
################################################################################
dr.matrix <- function(sim, obs, na.rm=TRUE,
                     fun=NULL, ...,
                     epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA){ 
 
 # Checking that 'sim' and 'obs' have the same dimensions
 if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )

 dr <- rep(NA, ncol(obs))       
          
 dr <- sapply(1:ncol(obs), function(i,x,y) { 
             dr[i] <- dr.default( x[,i], y[,i], na.rm=na.rm, ... )
             }, x=sim, y=obs, na.rm=na.rm, fun=fun, ..., 
             epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
                     
  names(dr) <- colnames(obs)
  return(dr)
     
} # 'dr.matrix' end


dr.data.frame <- function(sim, obs, na.rm=TRUE,
                         fun=NULL, ...,
                         epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                         epsilon.value=NA){  
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  dr.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
            epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
     
} # 'dr.data.frame' end


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 20-Jul-2022                                                         #
# Updates: 29-Jul-2022                                                         #
################################################################################
dr.zoo <- function(sim, obs, na.rm=TRUE,
                  fun=NULL, ...,
                  epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
                  epsilon.value=NA){ 
    
    sim <- zoo::coredata(sim)
    if (is.zoo(obs)) obs <- zoo::coredata(obs)
    
    if (is.matrix(sim) | is.data.frame(sim)) {
       dr.matrix(sim, obs, na.rm=na.rm, fun=fun, ..., 
                epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
    } else NextMethod(sim, obs, na.rm=na.rm, fun=fun, ..., 
                      epsilon.type=epsilon.type, epsilon.value=epsilon.value)    
     
  } # 'dr.zoo' end
