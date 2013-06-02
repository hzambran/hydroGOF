# File plotbandsonly.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2010-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# plotbandsonly: Plots a polygon representing uncertainty bounds               #
################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: Date: 24-Nov-2010                                                   #
################################################################################
# Updates: 15-Apr-2013                                                         #
################################################################################
      
plotbandsonly <- function(lband, uband,
                      
                          dates,
                          date.fmt="%Y-%m-%d", 
                        
                          bands.col="lightblue",
                          border= NA,               
                      
                          ...) {
                    
    # Checking  the class of 'x', 'lband', 'uband, and 'sim' (if provided)
    valid.class <- c("xts", "zoo", "numeric", "integer")
    if ( length(which((class(lband) %in% valid.class) == TRUE)) == 0) 
      stop("Invalid argument: 'class(lband)' must be in c('xts', 'zoo', 'numeric', 'integer')")
    if ( length(which((class(uband) %in% valid.class) == TRUE)) == 0) 
      stop("Invalid argument: 'class(uband)' must be in c('xts', 'zoo', 'numeric', 'integer')")         

    # Checking that the lenghts of 'lband' and 'uband' are equal 
    if ( length(lband) != length(uband) )
      stop("Invalid argument: 'length(lband)' is different from 'length(uband)'")    
    
    # For easier reading
    x <- lband
    
    # If the user didn't provided the dates, but 'x' is a zoo object
    # dates are taken from 'x'
    if ( missing(dates) ) {
    
      if ( zoo::is.zoo(x) | xts::is.xts(x) ) {
        # class(time(x))== "Date" for 'daily' and 'monthly' time series
        # class(time(x))== "character" for 'annual' time series
        if ( class(time(x)) == "Date" ) { dates <- time(x) 
        } else if ( class(time(x)) == "character" ) {  
             dates <- as.Date(time(x), format="%Y") 
          }  
      } else # If there is no way to obtain the dates
          message("[Note: You didn't provide dates, so only a numeric index will be used in the time axis.]")  
          
      # Checking that the dates of 'x', 'lband', 'uband' and 'sim' are equal ,
      # when they are zoo objects    
      if ( zoo::is.zoo(lband) & zoo::is.zoo(uband) ) 
        if  ( !all.equal( time(lband), time(uband) ) )
         stop("Invalid argument: time(lband) is different from time(uband)")       
          
    } # IF end
    
    # If the user provided 'dates', 
    # its length is checked against 'length(x)', and
    # the values of 'dates' are set to 'x', 'lband', 'uband' and 'sim' 
    # when they are zoo objects 
    if ( !missing(dates) )  { 
  
      # Checking that 'dates' have the same length than 'lband' ( and 'uband')      
      if ( length(dates) != length(lband) )  
         stop("Invalid argument: 'dates' and 'lband' must have the same length")
  
      # Checking that 'dates' have the right class
      if (is.na(match(class(dates), c("character", "factor", "Date")))) 
        stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date')")
        
      # If 'dates' is a factor or character , it have to be converted into 'Date' class, 
      # using the date format  specified by 'date.fmt'
      if ( !is.na( match(class(dates), c("factor", "character") ) ) ) 
        dates <- as.Date(dates, format= date.fmt)   
    
      # If 'lband', 'uband'  (when provided) are 'zoo' 
      # and the user provides 'dates' (probably new dates), 
      # the dates of the objects are changed to the new date
      if ( zoo::is.zoo(lband) ) { time(lband) <- dates } 
      if ( zoo::is.zoo(uband) ) { time(uband) <- dates }  
        
      # If the class of 'x' 'lband', 'uband' and 'sim' (when provided) 
      # are not 'zoo' and the user provides the dates, 
      # then we turn them into a zoo objects
      if ( !zoo::is.zoo(lband) )  lband <- vector2zoo(x=lband, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo
      if ( !zoo::is.zoo(uband) )  uband <- vector2zoo(x=uband, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo 
    
    }  # IF end       

    # Getting the position of the possible NA's
    na.index <- which(is.na(x))

    # Avoiding plotting the uncertainty bounds for the Na's
    if (length(na.index) > 0) {
      uband[na.index] <- uband[na.index-1]
      lband[na.index] <- lband[na.index+1]
    } # IF end

    #uband[na.index] <- .5*( uband[na.index+1] + uband[na.index-1] )
    #lband[na.index] <- .5*( lband[na.index+1] + lband[na.index-1] )

    if ( length(which((class(lband) %in% c("xts")) == TRUE)) > 0) {
      lband.coords <- xy.coords(xts::.index(lband), lband[, 1])
      uband.coords <- xy.coords(xts::.index(uband), uband[, 1])

      t     <- c( lband.coords$x, rev(lband.coords$x) )
      bands <- c( uband.coords$y, rev(lband.coords$y) )              
    } else {
       # Length of the observed values and all the vectors provided
       L <- length(lband) 
    
       # Creating the 'x' values of the polygons of the bands
       if ( zoo::is.zoo(x) ) {
         t <- c( time(lband), rev(time(uband)) )
       } else t <- c( 1:L, L:1)
         # Creating the 'y' values of the polygons of the bands
         bands <- c(as.numeric(lband), rev(as.numeric(uband)) )             
      } # ELSE end    
    
    # Plotting the polygons between the lower and upper bands
    polygon(t, bands, col=bands.col, border=border)

} # 'plotbandsonly' END
