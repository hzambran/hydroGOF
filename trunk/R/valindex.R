#######################################################################
# 'valindex': index of the elements that belongs to both vectors      #
#######################################################################
#     19-Jan-2009   #
#####################
# 'x'     : vector (numeric, xts, zoo)
# 'y'     : vector (numeric, xts, zoo)
# 'Result': index containing the position in 'x' and 'y' where both vectors 
#           have valid elements (NON- NA)
valindex <- function(obs, sim) {  

   if ( length(obs) != length(sim) ) 
	  stop("Invalid argument: 'sim' & 'obs' doesn't have the same length !") 
	  
   valid.index.obs <- which( !is.na(obs) ) 
   valid.index.sim <- which( !is.na(sim) ) 
 
   return( .intersect(valid.index.obs, valid.index.sim) )
     
} # 'valindex' END
