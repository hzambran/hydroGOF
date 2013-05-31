# File valindex.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'valindex': index of the elements that belongs to both vectors               #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 19-Jan-2009                                                         #
# Updates: 08-May-2012                                                         #
#          22-Mar-2013 ; 15-Apr-2013                                           #
################################################################################
# 'x'     : vector (numeric, xts, zoo)
# 'y'     : vector (numeric, xts, zoo)
# 'Result': index containing the position in 'x' and 'y' where both vectors 
#           have valid elements (NON- NA)

valindex <- function(sim, obs, ...) UseMethod("valindex")

valindex.default <- function(sim, obs, ...) {  

   if ( length(obs) != length(sim) ) {
	  stop( "Invalid argument: 'length(sim) != length(obs)' !! (", length(sim), "!=", length(obs), ") !!" )
   } else { 
       index <- which(!is.na(sim) & !is.na(obs))
       if (length(index)==0) warning("'sim' and 'obs' are empty or they do not have any common pair of elements with data !!")
       return( index  )
     } # ELSE end
     
} # 'valindex' END


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 25-Jul-2011                                                         #
# Updates: 08-May-2012                                                         #
################################################################################
valindex.matrix <- function(sim, obs, ...) { 

  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE ) {
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
          paste(dim(sim), collapse=" "), "] != [", 
          paste(dim(obs), collapse=" "), "] )", sep="") )
   } else  
       return ( !is.na( sim) & !is.na(obs) )
 
} # 'valindex.matrix' END
