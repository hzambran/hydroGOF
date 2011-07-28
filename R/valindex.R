#######################################################################
# 'valindex': index of the elements that belongs to both vectors      #
#######################################################################
#     19-Jan-2009   #
#####################
# 'x'     : vector (numeric, xts, zoo)
# 'y'     : vector (numeric, xts, zoo)
# 'Result': index containing the position in 'x' and 'y' where both vectors 
#           have valid elements (NON- NA)

valindex <- function(sim, obs, ...) UseMethod("valindex")

valindex.default <- function(sim, obs, ...) {  

   if ( length(obs) != length(sim) ) 
	  stop( paste("Invalid argument: 'sim' & 'obs' doesn't have the same length (", length(obs), "!=", length(sim), ") !",  sep="" ) )
	  
   valid.index.obs <- which( !is.na(obs) ) 
   valid.index.sim <- which( !is.na(sim) ) 
 
   return( .intersect(valid.index.obs, valid.index.sim) )
     
} # 'valindex' END

#####################
#     25-Jul-2011   #
#####################
valindex.matrix <- function(sim, obs, ...) { 

 !is.na( sim) & !is.na(obs)
 
} # 'valindex.matrix' END
