########################################
# 'NSeff': Nash-sutcliffe Efficiency   #
########################################
# 15-Dic-2008   ; 06-Sep-09            #
########################################
# Nash-Sutcliffe efficiencies (Nash and Sutcliffe, 1970) range from -∞ to 1. 
# An efficiency of 1 (NSeff = 1) corresponds to a perfect match of modeled to the observed data. 
# An efficiency of 0 (NSeff = 0) indicates that the model predictions are as accurate
# as the mean of the observed data, whereas 
# an efficiency less than zero (-∞ < NSeff < 0) occurs when the observed mean is a better predictor than the model.
# Essentially, the closer the model efficiency is to 1, the more accurate the model is.  

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Nash-sutcliffe Efficiency between 'sim' and 'obs'

NSeff <-function(sim, obs, ...) UseMethod("NSeff")

NSeff.default <- function (sim, obs, na.rm=TRUE, ...){ 

   if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
          is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
     ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")      

   vi <- valindex(sim, obs)
     
   obs <- obs[vi]
   sim <- sim[vi]
     
   denominator <- sum( (obs - mean(obs))^2 )
     
   if (denominator != 0) {
      
     NS <- 1 - ( sum( (obs - sim)^2 ) / denominator )
     
   } else stop("'sum((obs - mean(obs))^2)=0' => it is not possible to compute 'NSeff'")  
     
   return(NS)
     
} # 'NSeff' end


NSeff.matrix <- function (sim, obs, na.rm=TRUE, ...){ 
 
  NS <- rep(NA, ncol(obs))       
          
  NS <- sapply(1:ncol(obs), function(i,x,y) { 
                 NS[i] <- NSeff.default( x[,i], y[,i], na.rm=na.rm, ... )
               }, x=sim, y=obs )    
                     
  names(NS) <- colnames(obs)
  
  return(NS)
     
} # 'NSeff.matrix' end


NSeff.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  NSeff.matrix(sim, obs, na.rm=na.rm, ...)
     
} # 'NSeff.data.frame' end
