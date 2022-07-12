# File preproc.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'preproc': It applies a user-defined function to simulated and observed      #
#            values before computing any goodness-of-fit function, probably    #
#            adding a user-defined (and small) 'epsilon' value in order to     #
#            allow the use of logarithm and other similar functions that do    #
#            not work with zero values                                         #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Reference: Pushpalatha, R., Perrin, C., Le Moine, N., & Andreassian, V.      #
#            (2012). A review of efficiency criteria suitable for evaluating   #
#            low-flow simulations. Journal of Hydrology, 420, 171-182.         #
#            DOI: 10.1016/j.jhydrol.2011.11.055                                #
################################################################################
# Started: 29-Jun-2017                                                         #
# Updates: 11-Jul-2022 ; 12-Jul-2022                                           #
################################################################################
# 'sim'     : numeric, with simulated values
# 'obs'     : numeric, with observed values
# 'FUN'     : function to be applied to 'sim' and 'obs' in order to obtain 
#             transformed values thereof before applying any goodness-of-fit 
#             function included in the hydroGOF package
# '...'     : additional argument to be passed to FUN
# 'epsilon' : argument used to define a numeric value to be added to both 'sim' 
#             and 'obs' before applying FUN. It is was  designed to allow the 
#             use of logarithm and other similar functions that do not work with 
#             zero values. It must be one of the following three possible values:
#             -) "Pushpalatha2012": one hundredth of the mean observed values is 
#                                   added to both 'sim' and 'obs', as described  
#                                   in Pushpalatha et al., (2012). 
#             -) "otherFactor"    : the numeric value defined in the \code{epsilon.value} 
#                                   argument is used to multiply the the mean 
#                                   observed values, instead of the 
#                                   one hundredth (1/100) described in Pushpalatha et al. (2012). 
#                                   The resulting value is then added to both 
#                                   \code{sim} and \code{obs}.
#             -) "otherValue"     : the numeric value defined in the 'epsilon.value'
#                                   argument is directly added to both 'sim' and 'obs'
# 'epsilon.value': numeric value to be added to both 'sim' and 'obs' when 
#                  'epsilon="other"'
# 'Output': a list with two numeric vectors:
#           1) 'sim': simulated values after adding 'epsilon.value' and 
#                     applying 'FUN' 
#           2) 'obs': observed values after adding 'epsilon.value' and 
#                     applying 'FUN' 
preproc <- function (sim, obs, na.rm=TRUE, FUN,  ..., 
                     epsilon=c("Pushpalatha2012", "otherFactor", "otherValue"), 
                     epsilon.value=NA) { 

   # FUN ?
   fun.exists <- FALSE
   if (!missing(FUN)) {
     fun.exists <- TRUE
     FUN        <- match.fun(FUN)
   } # IF end

   # epsilon.value ?
   epsilon <- match.arg(epsilon)

   if (epsilon %in% c("otherFactor", "otherValue") )  {
     if (is.na(epsilon.value))
       stop("Missing argument: you need to provide 'epsilon.value' !")

     if ( !is.numeric(epsilon.value) )
       stop("Invalid argument: 'epsilon.value' must be numeric !")
   } # IF end
 
   if (epsilon=="Pushpalatha2012") {    
       epsilon.value <- (1/100)*mean(obs, na.rm=na.rm)
   } else if (epsilon=="otherFactor") {
       epsilon.value <- epsilon.value*mean(obs, na.rm=na.rm)
     } # ELSE (epsilon="otherValue"): epsilon.value=epsilon.value

   # Adding epsilon, before applying FUN
   obs <- obs + epsilon.value
   sim <- sim + epsilon.value

   # using FUN (and 'epsilon.value')
   if (fun.exists) {
     obs.bak <- obs
     sim.bak <- sim

     obs <- FUN( obs, ...)     
     sim <- FUN( sim, ...)

     if (length(obs) != length(obs.bak))
        stop("Invalid argument: 'FUN' returns an object with a length different from 'obs' or 'sim' !")
   } # IF 'fun.exists' end
     
   out <- list(sim=sim, obs=obs)
     
   return(out)

} # 'preproc' END
