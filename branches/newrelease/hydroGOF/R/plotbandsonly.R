########################################################################
# plotbandsonly: Plots a polygon representing uncertainty bounds       #
########################################################################
#	   Date: 24-Nov-2010                                           #
########################################################################
# 'lband'    : ts or 'zoo' object with the values of the lower band
# 'uband'    : ts or 'zoo' object with the values of the upper band

# 'bands.col': color to be used for filling th area between the lower and upper band
# 'border'   : see '?polygon'. The color to draw the border of the uncertainty bands.  The default 'NA' omits the borders.
#             Use 'border' = 'NULL', to  use 'par("fg")'
# 'cal.ini'   : OPTIONAL. Character with the date in which the calibration period started.
#               ONLY used for drawing a vertical red line at this date.
# 'val.ini'   : OPTIONAL. Character with the date in which the validation period started.
#               ONLY used for drawing a vertical red line at this date.
# 'date.fmt'  : character indicating the format in which the dates entered are stored in 'cal.ini' adn 'val.ini'. Default value is "\%Y-\%m-\%d"
# 'leg.cex'   : OPTIONAL. numeric. Used for the GoF legend. Character expansion factor *relative* to current
#               'par("cex")'.  Used for text, and provides the default
#               for 'pt.cex' and 'title.cex'. Default value = 1

# Example:
      
plotbandsonly <- function(lband, uband,
                      
                          dates,
                          date.fmt="%Y-%m-%d", 
                      
                          legend="95PPU",
                          leg.cex=1,
                        
                          bands.col="lightblue",
                          border= NA,               
                      
                          ...) {
                    
    # requesting 'hydroTSM' package: 'sfreq', 'vector2zoo', 'drawxaxis'
    require(hydroTSM)

    # Checking  the class of 'x', 'lband', 'uband, and 'sim' (if provided)
    if ( is.na( match(class(lband), c("zoo", "numeric", "integer") ) ) )
      stop("Invalid argument: 'class(lband)' must be in c('zoo', 'numeric', 'integer')")
    if ( is.na( match(class(uband), c("zoo", "numeric", "integer") ) ) )
      stop("Invalid argument: 'class(uband)' must be in c('zoo', 'numeric', 'integer')")         

    # Checking that the lenghts of 'lband' and 'uband' are equal 
    if ( length(lband) != length(uband) )
      stop("Invalid argument: 'length(lband)' is different from 'length(uband)'")  

    # Length of the observed values and all the vectors provided
    L <- length(lband)
   
    
    # Requiring the Zoo Library (Zoo's ordered observations): 'is.zoo', 'as.zoo', and 'plot.zoo' functions
    require(zoo)

    # For easier reading
    x <- lband
    
    # If the user didn't provided the dates, but 'x' is a zoo object
    # dates are taken from 'x'
    if ( missing(dates) ) {
    
      if ( is.zoo(x) ) {
        # class(time(x))== "Date" for 'daily' and 'monthly' time series
        # class(time(x))== "character" for 'annual' time series
        if ( class(time(x)) == "Date" ) { dates <- time(x) 
        } else if ( class(time(x)) == "character" ) {  
             dates <- as.Date(time(x), format="%Y") 
          }  
      } else # If there is no way to obtain the dates
          message("Note: You didn't provide dates, so only a numeric index will be used in the time axis.")  
          
      # Checking that the dates of 'x', 'lband', 'uband' and 'sim' are equal ,
      # when they are zoo objects    
      if ( is.zoo(lband) & is.zoo(uband) ) 
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
      if ( is.zoo(lband) ) { time(lband) <- dates } 
      if ( is.zoo(uband) ) { time(uband) <- dates }  
        
      # If the class of 'x' 'lband', 'uband' and 'sim' (when provided) 
      # are not 'zoo' and the user provides the dates, 
      # then we turn them into a zoo objects
      if ( !is.zoo(lband) )  lband <- vector2zoo(x=lband, dates=dates, date.fmt=date.fmt) 
      if ( !is.zoo(uband) )  uband <- vector2zoo(x=uband, dates=dates, date.fmt=date.fmt)       
    
    }  # IF end
       

    # Getting the position of the possible NA's
    na.index <- which(is.na(x))

    # Avoiding plotting the uncertainty bounds for the Na's
    uband[na.index] <- uband[na.index-1]
    lband[na.index] <- lband[na.index+1]

    #uband[na.index] <- .5*( uband[na.index+1] + uband[na.index-1] )
    #lband[na.index] <- .5*( lband[na.index+1] + lband[na.index-1] )

    # Creating the 'x' values of the polygons of the bands
    if ( is.zoo(x) ) {
      t <- c( time(lband), rev(time(uband)) )
    } else t <- c( 1:L, L:1)

    # Creating the 'y' values of the polygons of the bands
    bands <- c(as.numeric(lband), rev(as.numeric(uband)) )

    # Plotting the polygons between the lower and upper bands
    polygon(t, bands, col=bands.col, border=border)


} # 'plotbandsonly' END
