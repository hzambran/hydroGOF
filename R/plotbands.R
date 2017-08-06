# File plotbands.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2009-2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# plotbands: Plot a ts with simulated values and two confidence bands          #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
# Started: 13-Oct-2009                                                         #
# Updates: 30-Jun-2010; 28-Oct-2010; 28-Nov-2010 ;                             # 
#          15-Apr-2011 ; 17-May-2011                                           #
#          15-Apr-2013                                                         #
#          06-Aug-2017                                                         #
################################################################################
plotbands <- function(x, lband, uband, sim,
                      
                      dates,
                      date.fmt="%Y-%m-%d", 

                      gof.leg= TRUE, 
                      gof.digits=2,    
                      
                      legend=c("Obs", "Sim", "95PPU"),
                      leg.cex=1,
                      
                      bands.col="lightblue",
                      border= NA,

                      tick.tstep= "auto", 
                      lab.tstep= "auto",
                      lab.fmt=NULL,
                      
                      cal.ini=NA, 
                      val.ini=NA, 
                      
                      main="Confidence Bounds for 'x'",
                      xlab="Time",
                      ylab="Q, [m3/s]",

                      ylim,
                      
                      col= c("black", "blue"),
                      type= c("lines", "lines"),

                      cex= c(0.5, 0.5),
                      cex.axis=1.2,
                      cex.lab=1.2,
                      
                      lwd=c(0.6, 1),
                      lty=c(3, 4),    
                      pch=c(1, 9),   
                      
                      ...) {
                    
    # Checking  the class of 'x', 'lband', 'uband, and 'sim' (if provided)
    valid.class <- c("xts", "zoo", "numeric", "integer")
    if ( is.na( match(class(x), valid.class ) ) )
      stop("Invalid argument: 'class(x)' must be in c('xts', 'zoo', 'numeric', 'integer')")
    if ( is.na( match(class(lband), valid.class ) ) )
      stop("Invalid argument: 'class(lband)' must be in c('xts', 'zoo', 'numeric', 'integer')")
    if ( is.na( match(class(uband), valid.class ) ) )
      stop("Invalid argument: 'class(uband)' must be in c('xts', 'zoo', 'numeric', 'integer')")      
    if ( !missing(sim) ) {
      if ( is.na( match(class(sim), valid.class ) ) )
        stop("Invalid argument: 'class(sim)' must be in c('xts', 'zoo', 'numeric', 'integer')")
    } # IF end    

    # Checking that the lenghts of 'lband' and 'uband' are equal between 
    # them, an equal to the lenght of 'x' and 'sim' (if provided)
    if ( length(lband) != length(uband) )
      stop("Invalid argument: 'length(lband)' is different from 'length(uband)'")
    if ( length(x) != length(uband) )
      stop("Invalid argument: 'length(x)' is different from the lengths of the bands")      
    if ( !missing(sim) ) {
      if ( length(x) != length(sim) )
      stop("Invalid argument: 'length(sim)' is different from 'length(x)'")
    } # IF end 
    
    # Length of the observed values and all the vectors provided
    L <- length(x)      
      
    # Checking 'type'
    if ( length( which( !is.na( match((type), c("lines", "points") ) ) ) ) < 2)
      stop("Invalid argument: 'type' elements must be in c('lines', 'points')")
      
    # If the user provided a value for 'legend' here we verify that it has 3 elements
    if ( !missing(legend) & (length(legend) > 1) )
        if ( length(legend) != 3 ) stop("Invalid argument: 'legend' must have 3 elements. e.g, c('obs', 'sim', 'PPU')")

    # If the user provided a value for 'cal.ini', it is transformed into a Date class
    if ( !missing(cal.ini) ) cal.ini <- as.Date(cal.ini, format=date.fmt)
    
    # If the user provided a value for 'val.ini', it is transformed into a Date class
    if ( !missing(val.ini) ) val.ini <- as.Date(val.ini, format=date.fmt)      
     
    # If the user didn't provided the dates, but 'x' is a zoo object
    # dates are taken from 'x'
    if ( missing(dates) ) {
    
      if ( zoo::is.zoo(x) ) {
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
      if ( zoo::is.zoo(x) & zoo::is.zoo(uband) ) 
        if  ( !all.equal( time(x), time(uband) ) )
          stop("Invalid argument: time(x) is different from the time of the bands")      
      if ( !missing(sim) ) {
        if ( zoo::is.zoo(x) & zoo::is.zoo(sim) ) 
          if  ( !all.equal( time(x), time(sim) ) )
            stop("Invalid argument: time(x) is different from the time of 'sim'")    
      } # IF end
          
    } # IF end
    
    # If the user provided 'dates', 
    # its length is checked against 'length(x)', and
    # the values of 'dates' are aplyied to 'x', 'lband', 'uband' and 'sim' 
    # when they are zoo objects 
    if ( !missing(dates) )  { 
  
      # Checking that 'dates' have the same length than 'sim' ( and 'obs')      
      if ( length(dates) != length(x) )  
         stop("Invalid argument: 'dates' and 'x' must have the same length")
  
      # Checking that 'dates' have the right class
      if (is.na(match(class(dates), c("character", "factor", "Date")))) 
        stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date')")
        
      # If 'dates' is a factor or character , it have to be converted into 'Date' class, 
      # using the date format  specified by 'date.fmt'
      if ( !is.na( match(class(dates), c("factor", "character") ) ) ) 
        dates <- as.Date(dates, format= date.fmt)   
    
      # If 'x', 'lband', 'uband' and 'sim' (when provided) are 'zoo' 
      # and the user provides 'dates' (probably new dates), 
      # the dates of the objects are changed to the new date
      if ( zoo::is.zoo(x) )     { time(x)     <- dates }  
      if ( zoo::is.zoo(lband) ) { time(lband) <- dates } 
      if ( zoo::is.zoo(uband) ) { time(uband) <- dates }  
      if ( !missing(sim) ) 
        if ( is.zoo(sim) ) { time(sim)   <- dates }  
        
      # If the class of 'x' 'lband', 'uband' and 'sim' (when provided) 
      # are not 'zoo' and the user provides the dates, 
      # then we turn them into a zoo objects
      if ( !zoo::is.zoo(x) )      x     <- vector2zoo(x=x, dates=dates, date.fmt=date.fmt)     # hydroTSM::vector2zoo
      if ( !zoo::is.zoo(lband) )  lband <- vector2zoo(x=lband, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo
      if ( !zoo::is.zoo(uband) )  uband <- vector2zoo(x=uband, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo
      if ( !missing(sim) ) {
        if ( !zoo::is.zoo(sim) )  {
           sim <- vector2zoo(x=sim, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo
	   message("[Note: 'sim'  was transformed into a zoo object, with 'time(sim)' equal to 'time(obs)']") 
	} # IF end
      }  # IF end 
    }  # IF end       

    # Getting the position of the possible NA's
    na.index <- which(is.na(x))

    # Avoiding plotting the uncertainty bands for the Na's
    uband[na.index] <- uband[na.index-1]
    lband[na.index] <- lband[na.index+1]

    #uband[na.index] <- .5*( uband[na.index+1] + uband[na.index-1] )
    #lband[na.index] <- .5*( lband[na.index+1] + lband[na.index-1] )
    
    # Computing 'ylim', if it was not provided
    if ( missing(ylim) ) {
      if ( missing(sim) ) {
        ylim <- range(lband, uband, x, na.rm=TRUE)
      } else ylim <- range(lband, uband, x, sim, na.rm=TRUE)
    } # IF end
    
    # Creating the plot, but without anything on it, for allowign the call to polygon
    if ( zoo::is.zoo(x) ) {
      #if ( !xts::is.xts(x) ) x <- xts::as.xts(x)   
      # Creating the plot, but without anything on it, for allowign the call to polygon
      zoo::plot.zoo(x, xaxt = "n", yaxt = "n", type="n", main=main, 
                    xlab=xlab, ylab=ylab, ylim=ylim, 
                    cex.axis=cex.axis, cex.lab=cex.lab, ...) 
      axis(2, cex.axis=cex.axis, cex.lab=cex.lab)  
    } else plot(x, type="n", xaxt = "n", main=main, xlab=xlab, ylab=ylab, ylim=ylim, 
                cex.axis=cex.axis, cex.lab=cex.lab, ...)

    #if ( zoo::is.zoo(lband) & !xts::is.xts(lband) )  lband <- xts::as.xts(lband)
    #if ( zoo::is.zoo(uband) & !xts::is.xts(uband) )  uband <- xts::as.xts(uband)
    
    # Plotting the uncertainty bounds (polygon)
    plotbandsonly(lband=lband, uband=uband, dates=dates, date.fmt=date.fmt, 
                  bands.col=bands.col, border= border, ...)

    # Draws custom ticks and labels on the X axis
    if ( zoo::is.zoo(x) | xts::is.xts(x) ) {
      drawTimeAxis(x, tick.tstep=tick.tstep, lab.tstep=lab.tstep, lab.fmt=lab.fmt, cex.axis=cex.axis) # hydroTSM::drawTimeAxis
    } else axis(side = 1, labels = TRUE)

    # Plotting the OBSERVED time series, over the polygons
    if (type[1] == "lines") {
      lines(x, cex= cex[1], col=col[1], lty=lty[1], lwd=lwd[1], pch=pch[1], ... )
    } else if (type[1] == "points") {
      points(x, cex= cex[1], col=col[1], lty=lty[1], lwd=lwd[1], pch=pch[1], ... )
      } # IF end
      
    # Plotting the SIMULATED time series, over the polygons
    if ( !missing(sim) ) {
        if ( (zoo::is.zoo(x)) & (!xts::is.xts(sim)) ) sim <- xts::as.xts(sim)
        # Plotting the SIMULATED time series, over the polygons
        if (type[2] == "lines") {
          lines(sim, cex= cex[2], col=col[2], lty=lty[2], lwd=lwd[2], pch=pch[2], ... )
        } else if (type[2] == "points") {
          points(sim, cex= cex[2], col=col[2], lty=lty[2], lwd=lwd[2], pch=pch[2], ... )
          } # IF end
    } # IF end

    # If the user provided a value for 'cal.ini', a vertical line is drawn
    if ( !missing(cal.ini) ) abline(v=cal.ini, col="red", lty=3, lwd=2)
    # If the user provided a value for 'val.ini', a vertical line is drawn
    if ( !missing(val.ini) ) abline(v=val.ini, col="red", lty=3, lwd=2)

    # Legend with 'Obs', 'Sim'  & 95PPU is drawn, when 'legend' is not FALSE
    if ( !identical(legend, FALSE) ) {                        
      if ( missing(sim) ) {
        #legend <- c("Obs", "95PPU")
        legend <- c( legend[1], legend[3] ) 
        legend("topright", legend,  inset=0.03,
               bty="n", cex =0.9, col=c(col[1], bands.col), lwd=c(lwd[1], 0), lty=c(lty[1],0), pch=c(NA,15), pt.cex=3)
      } else {
        #legend <- c("Obs", "Sim", "95PPU") 
        legend("topright", legend, inset=0.03,
               bty="n", cex =0.9, col=c(col, bands.col), lwd=c(lwd, 0), lty=c(lty, 0), pch=c(NA,NA,15), pt.cex=3)
        } # ELSE end
    } # IF end
    
    # Legend with the p-factor and r-factor
    if (gof.leg) {
        legend("topleft",  y.intersp=1.2, cex =leg.cex, bty="n", #inset=0.01,
              c( paste( "p-factor =", round(pfactor(x, lband=lband, uband=uband), gof.digits), sep=" "),
                 paste( "r-factor =", round(rfactor(x, lband=lband, uband=uband), gof.digits), sep=" ") )
               )
    } # IF end

} # 'plotbands' END
