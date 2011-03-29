##########################################################################
# pfactor: % of observations that are within the given uncertainty bounds#
##########################################################################
#	                       Date: 24-Jan-2010                             #
##########################################################################
# 'x'        : ts or 'zoo' object with the simulated values
# 'lband'    : ts or 'zoo' object with the values of the lower uncertainty bound
# 'uband'    : ts or 'zoo' object with the values of the upper uncertainty bound

# Ideally, i.e., with a combination of model structure and parameter values 
# that perfectly represents the catchment under study, and in absence of 
# measurement errors and other additional sources of uncertainty, all the 
# simulated values should be in a perfect match with the observations, 
# leading to a P-factor equal to 1, and an R-factor equal to zero. 
# However, in real-world applications we aim at encompassing as much 
# observations as possible within the given uncertainty bounds 
# (P-factor close to 1) while keeping the width of the uncertainty bounds 
# as small as possible (R-factor close to 0), in order to avoid obtaining 
# a good bracketing of observations at expense of uncertainty bounds too 
# wide to be informative for the decision-making process.

# Refs:
# Abbaspour, K. C., M. Faramarzi, S. S. Ghasemi, and H. Yang (2009), 
# Assessing the impact of climate change on water resources in Iran, 
# Water Resour. Res., 45(10), W10,434, doi:10.1029/2008WR007615. \cr

# Abbaspour, K. C., J. Yang, I. Maximov, R. Siber, K. Bogner, J. Mieleitner, 
# J. Zobrist, and R. Srinivasan (2007), Modelling hydrology and water quality 
# in the pre-alpine/alpine Thur watershed using SWAT, Journal of Hydrology, 
# 333(2-4), 413–430, doi:10.1016/j.jhydrol.2006.09.014.

# Schuol, J., K. Abbaspour, R. Srinivasan, and H. Yang (2008b), 
# Estimation of freshwater availability in the West African sub-continent 
# using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30, 
# doi:10.1016/j.jhydrol.2007.12.025. \cr

# Abbaspour, C., Karim (2007), User manual for SWAT-CUP, SWAT calibration 
# and uncertainty analysis programs, 93pp, Eawag: Swiss Fed. Inst. of Aquat. Sci. and 
# Technol. Dubendorf, Switzerland, Available at http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN

pfactor <- function(x, ...) UseMethod("pfactor")

pfactor.default <- function(x, lband, uband, na.rm=TRUE, ...)  {

    # Just in case 'some' of them be ts or 'zoo'
    x     <- as.numeric(x)
    lband <- as.numeric(lband)
    uband <- as.numeric(uband)
        
    # Getting the row index in 'q95' of all the observations that are within L95PPU and U95PPU
    within.index <- which((lband <= x) & (x <= uband) )
     
    # Getting the best simulated streamflows (skipping days withoud measurements)
    pfactor <- length( within.index ) / length( x ) 
    
    return(pfactor)

} # 'pfactor.default' end


pfactor.matrix <- function (x, lband, uband, na.rm=TRUE, ...){

    pfactor <- rep(NA, ncol(x))       
          
    pfactor <- sapply(1:ncol(x), function(i,x,l,u) { 
                 pfactor[i] <- pfactor.default( x[,i], l[,i], u[i], na.rm=na.rm, ... )
            }, x=x, l=lband, u=uband )            
           
    return(pfactor)  
     
  } # 'pfactor.matrix' end
  

pfactor.data.frame <- function (x, lband, uband, na.rm=TRUE, ...){ 
 
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
   
  pfactor.matrix(x, lband, uband, na.rm=na.rm, ...)
     
} # 'pfactor.data.frame' end 
