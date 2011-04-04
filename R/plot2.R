############################################################
# 'plot2':     Plots 2 time series on the same graph       #
#              It is a wrapper for the 'plot.zoo' function #
############################################################
# Started: March 04, 2009    #
# Updates: May 2009          #
#          2010              #
#          21-Jan-2011       #
##############################
 
# 'x', 'y'     : time series that will be plotted.
#                class(x) & class(y) must be 'ts' or 'zoo'
#                If gof.leg=TRUE, then 'x' is considered as simulated and 'y'
#                as observed values (for some gof functions this is important)
# 'plot.type'  : String that indicates if the 2 ts have to be ploted in the 
#                same window or in two different vertical ones
#                Valid values are:
#                -) "single"  : (default) superimposes the 2 ts on a single plot
#                -) "multiple": plots the 2 series on 2 multiple vertical plots 
# 'pt.style'   : String that indicates if the 2 ts have to be plotted as lines or bars
#                Valid values are:
#                -) "ts" : (default) each ts is ploted as a lines along the 'x' axis
#                -) "bar": the 2 series are plotted as a barplot. 
# 'var.names'  : string vector with the types (names) of variables being plotted, 
#                e.g, "Precipitation", "Temperature" or "Flow"
#                Only used for labelling the axes 
# 'var.units'  : string representing the measurement unit of the variable 
#                being plotted, e.g., "mm" for precipitation, "C" for temperature, 
#                and "m3/s" for flow. 
# 'tick.tstep': string indicating the time step that have to be used for 
#               putting the ticks ont he time axis. 
#               Possible values are: 'days', 'months', 'years' 
# 'lab.tstep' : string indicating the time step that have to be used for 
#               putting the labels ont he time axis. 
#               Possible values are: 'days', 'months', 'years' 
# 'col'       : vector with the colors of 'x' and 'y'
# 'lwd'       : vector with the line width of 'x' and 'y'
# 'lty'       : vector with the line type of 'x' and 'y'
# 'pch'       : vector with the type of symbol for 'x' and 'y'. 
#                1: whithe circle; 9: white rhombus with a cross inside
# 'cex'       : vector with the values controlling the size of text and 
#                symbols of 'x' and 'y' with respect to the default
# 'add'        : logical indicating if other plots will be added in further calls
#                to this function.
#                -) 'add=FALSE' => the plot and the legend are plotted on the same graph
#                -) 'add=TRUE'  => the legend is plotted in a new graph, usually
#                                  when called from another function (e.g.: 'ggof')
# 'xlab'       : label for the 'x' axis
# 'ylab'       : label for the 'y' axis 
# 'gof.leg'    : boolean indicating if several goodness of fit have to be 
#                computed between both ts, and ploted as legends on the graph.
#                If gof.leg=TRUE, then 'x' is considered as observed and 'y'
#                as simulated values (for some gof functions this is important)
# 'digits'     : OPTIONAL, only used when 'gof.leg=TRUE'. Decimal places used for rounding the goodness-of-fit indexes
# 'leg.cex'    : OPTIONAL. Used for the GoF legend. Character expansion factor *relative* to current
#                'par("cex")'.  Used for text, and provides the default 
#                for 'pt.cex' and 'title.cex'. Default value = 1
# cal.ini      : OPTIONAL. Character with the date in which the calibration period started.
#                ONLY used for drawing a vertical red line at this date. 
# val.ini      : OPTIONAL. Character with the date in which the validation period started.
#                ONLY used for drawing a vertical red line at this date. 
# date.fmt     : character indicating the format in which the dates entered are stored in 'cal.ini' adn 'val.ini'. Default value is "\%Y-\%m-\%d"
# 'cex.axis'   : magnification of axis annotation relative to 'cex'. See '?par'.
# 'cex.lab'    : Magnification to be used for x and y labels relative to the current setting of 'cex'. See '?par'.
                  
                
plot2 <- function (x, y, 
                   plot.type = "multiple", 
                   
                   tick.tstep= "months", 
                   lab.tstep= "years", 
                   lab.fmt,
                   
                   main, 
                   xlab="Time", 
                   ylab=c("x", "y"),
                   
                   cal.ini=NA, 
                   val.ini=NA, 
                   date.fmt="%Y-%m-%d",                   
                   
                   gof.leg = FALSE, 
                   gof.digits=2, 
                   
                   legend=ylab,
                   leg.cex=1,                       
                        
                   col = c("black","blue"),
                   
                   cex=c(0.5,0.5),
                   cex.axis=1.2,
                   cex.lab=1.2,
                   
                   lwd= c(1,1), 
                   lty= c(1,3), 
                   pch= c(1,9),   
                   
                   pt.style = "ts",
                   add=FALSE,                   
                   
                    ...) {
                   
  require(zoo)

  # requesting 'hydroTSM' package:'vector2zoo', 'drawxaxis'
  require(hydroTSM)

  # Checking that the user provided 'x'
  if ( missing(x) ) 
         stop("Missing argument: 'x'")
         
  # Checking that the user provided 'y'
  if ( missing(y) ) 
         stop("Missing argument: 'y'")
  
  # Checking that the user provided a valid argument for 'x'       
  if (is.na(match(class(x), c("integer", "numeric","ts", "zoo") ) ) ) 
         stop("Invalid argument: 'class(x)' must be in c('integer', 'numeric', 'ts', 'zoo')")
         
  # Checking that the user provided a valid argument for 'y'   
  if (is.na(match(class(y), c("integer", "numeric", "ts", "zoo") ) ) ) 
         stop("Invalid argument: 'class(y)' must be in c('integer', 'numeric', 'ts', 'zoo')")
         
  # Checking that the user provided a valid argument for 'plot.type'       
  if (is.na(match(plot.type, c("single", "multiple") ) ) ) 
         stop("Invalid argument: 'plot.type' must be in c('single', 'multiple')")
         
  # If the user wants to draw a legned, it checks that the type of plot is 'single'
  if (gof.leg & (plot.type == "multiple") )
    stop("Invalid argument: For drawing a legend, 'plot.type' must be 'single'")
         
  # Checking that the user provided a valid argument for 'pt.style'       
  if (is.na(match(pt.style, c("ts", "bar") ) ) ) 
         stop("Invalid argument: 'pt.style' must be in c('ts', 'bar')")
         
  # Checking that the user provided a valid argument for 'tick.tstep'       
  if (is.na(match(tick.tstep, c("days", "months", "years") ) ) ) 
         stop("Invalid argument: 'tick.tstep' must be in c('days', 'months', 'years')")
         
  # Checking that the user provided a valid argument for 'lab.tstep'       
  if (is.na(match(lab.tstep, c("days", "months", "years") ) ) ) 
         stop("Invalid argument: 'lab.tstep' must be in c('days', 'months', 'years')")
         
  # If 'x' is 'ts' or 'zoo' and 'y' is only a vector, y is transformed into 
  # the same class of 'x', with the same times
  if ( !is.na(match(class(x), c("ts", "zoo") ) ) & 
       !is.na(match(class(y), c("integer", "numeric") ) ) ) {  
  
    # class(time(x))== "Date" for 'daily' and 'monthly' time series
    # class(time(x))== "character" for 'annual' time series
    if ( class(time(x)) == "Date" ) {
        y <- vector2zoo(y, dates=time(x))
    } else if ( class(time(x)) == "character" ) {
        y <- vector2zoo(y, dates=time(x), date.fmt="%Y")
        time(x) <- time(y) #'annual' time series
    } # ELSE END
    
  } # IF END

  
  # If the user didn't provide a value for 'lab.fmt', default values are used
  if (missing(lab.fmt)) {   
    if (lab.tstep == "days") { 
      lab.fmt <- "%Y-%m-%d"
    } else if (lab.tstep == "months") {
        lab.fmt <- "%b"   
      } else if (lab.tstep == "years") {
        lab.fmt <- "%Y"   
        } 
  } # IF end 
         
  # Checking that the user provied the same length for 'sim' and 'obs'      
  #if ( length(x) != length(y) )  
  #       stop("Invalid argument: 'obs' and 'sim' must have the same length")
         
  # If the user didn't provide a title for the plot, the default is used 
  if ( missing(main) ) main <- "Observed vs Simulated"   
  
  # 'x' axis title: If the user didn't provide a title for the 'x' axis, a default string is used 
  if ( missing(xlab) ) { xlab <- "Time" }   
  
  # If the user provided a value for 'cal.ini', it is transformed into a Date class
  if ( !missing(cal.ini) ) 
    cal.ini <- as.Date(cal.ini, format=date.fmt)
    
  # If the user provided a value for 'val.ini', it is transformed into a Date class
  if ( !missing(val.ini) ) 
    val.ini <- as.Date(val.ini, format=date.fmt)

  
  # If the legend has to be plotted AND no other plots will be added
  # IF 'add' is TRUE, the layout of the screen is set up by the calling procedure (usually 'ggof')
  if (gof.leg & add==FALSE) {  
            def.par <- par(no.readonly = TRUE) # save default, for resetting...     
            #par(mar=c(5, 2, 4, 0.5) + 0.1)
            # Setting up the screen with 1 rows and 2 columns
            layout( matrix( c(1,1,1,1,1,1,1,1,1,2,2), ncol=11, byrow=TRUE) ) 
            par(mar=c(5, 4, 4, 0) + 0.1)    
            on.exit(par(def.par))      
  } # ELSE end    
  
  # If the legend will not be plotted, the marginns are set to 'almost' the default values
  if (!gof.leg) {  
        par(mar=c(5, 4, 4, 2) + 0.1) # default values are par(mar=c(5, 4, 4, 4) + 0.1)
  } # ELSE end    
  
  
  # If the type of plot is "time series"
  if (pt.style=="ts") {
  
    # If both time series have to be ploted int he same plot area
    if (plot.type == "single") {
    
      # Plotting the Observed Time Series
      # xaxt = "n": is for avoiding drawing the x axis
      # cbind(x, y) is a multivariate time series
      # 'screens = 1' can be used instead of 'plot.type="single"'
      # 'screens = c(1,2)' can be used for plotting each ts in a separate screen with the same time axis 
      plot.zoo( cbind(x, y), plot.type=plot.type, xaxt = "n", type=c("o","o"), 
               lwd=lwd, lty= lty, col= col, pch= pch, 
               cex = cex, cex.axis=cex.axis, cex.lab=cex.lab,
               main=main, xlab=xlab, ylab= ylab, ... )
               
      # If the user provided a value for 'cal.ini', a vertical line is drawn
      if ( !missing(cal.ini) ) {
        abline(v=cal.ini, col="red", lty=1, lwd=2)
      } # IF end
      
      # If the user provided a value for 'cal.ini', a vertical line is drawn
      if ( !missing(val.ini) ) {
        abline(v=val.ini, col="red", lty=1, lwd=2)
      } # IF end
               
      # Drawing a legend with 'Obs' vs 'Sim' 
      # y.intersp=0.5, is for the vertical spacin in the legend
      # bty="n" => no box around the legend
      # 'inset=0.03' is usefult when plot.type= "multiple" for having a nice margin to the legend
      legend("topright", legend=legend, y.intersp=0.8, inset=0.03,
             bty="n", cex = leg.cex, col = col, lwd= lwd, lty= lty, pch=pch )  
      
      # Drawing the 'x' axis
      # If the user provided, in some way, valid values for being used as dates, 
      # they will be used, if not, only a numeric index will be used
      if ( !is.na(match(class(x), c("ts", "zoo") ) ) | !is.na(match(class(y), c("ts", "zoo") ) ) ) {
  
        if ( !is.na(match(class(x), c("ts", "zoo") ) ) ) { 
          z <- x
        } else z <- y
    
        # Draws monthly ticks in the X axis, but labels only in years
        drawxaxis(z, tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt, cex.axis=cex.axis, cex.lab=cex.lab) 
    
      } else Axis(side = 1, labels = TRUE, cex.axis=cex.axis, cex.lab=cex.lab)
               
    } else  #plot.type == "multiple"  
          {       
            # all the work (mainly Time axis) is made automatically be the 'plot.zoo' function 
            plot.zoo( cbind(x, y), plot.type=plot.type, type=c("o","o"), 
                       lwd=lwd, lty= lty, col= col, pch= pch, 
                       cex = cex, cex.axis=cex.axis, cex.lab=cex.lab,
                       main=main, xlab=xlab, ylab= ylab,...)
                         
      } # ELSE end 
      
  } else if (pt.style=="bar") {
    
        # Creation of the table that will be plotted as barplot
        b <- rbind(coredata(x),coredata(y))
        
        # Giving the as name to each bar the YEAR, because the 
        # bar plot is thought for being used ONLY for annual time series
        colnames(b) <- format( time(x), "%Y")
        
        # Barplot  
        barplot(b, beside=TRUE, axis.lty=1, col=col, density=25, angle=c(45,-45), 
                main=main, xlab=xlab, ylab= ylab, legend.text=legend, 
                cex.axis=cex.axis, cex.lab=cex.lab, ...)
       
        # Index of the bar corresponding to 'cal.ini'.
        # It is necessary to multiply it by 3 because for each year there are 3 vertical lines
        # It is necessary to substract 2, for shifting the line form the 3 line to the first one
        cal.index <- 3*which(colnames(b) == format( cal.ini, "%Y")) - 2
        # If the user provided a value for 'cal.ini', a vertical line is drawn
        if ( !missing(cal.ini) ) {
         abline(v=cal.index, col="red", lty=1, lwd=2)
        } # IF end
       
        # Index of the bar corresponding to 'cal.ini'.
        # It is necessary to multiply it by 3 because for each year there are 3 vertical lines
        # It is necessary to substract 2, for shifting the line form the 3 line to the first one
        val.index <- 3*which(colnames(b) == format( val.ini, "%Y")) - 2
        # If the user provided a value for 'cal.ini', a vertical line is drawn
        if ( !missing(val.ini) ) {
          abline(v=val.index, col="red", lty=1, lwd=2)
        } # IF end
      
    }  # ELSE end        
  
  
  # If the Goodness of Fit indexes have to be computed and plotted:
  if (gof.leg & plot.type == "single" ) {
  
   gof.xy <- gof(sim=as.numeric(x), obs=as.numeric(y), do.spearman=FALSE, do.pbfdc=FALSE, digits=gof.digits, ...)
   
   legend.position <- "center"
   par( mar=c(0.5, 0.5, 0.5, 0.5) ) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)
   plot.new() 
          
   # 'inset':  The optional 'inset' argument specifies how far the legend is inset from the plot margins.  
   #           If a single value is given, it is used for both margins; 
   #           if two values are given, the first is used for 'x'-distance, the second for 'y'-distance.
	 
   legend(legend.position,  y.intersp=1.2, cex =leg.cex, # bty="n", #inset=0.01,   
          c( paste( "ME =", gof.xy["ME", 1], sep=" "),
             paste( "MAE =", gof.xy["MAE", 1], sep=" "),
             #paste( "MSE =", gof.xy["MSE", 1], sep=" "),
             paste( "RMSE =", gof.xy["RMSE", 1], sep=" "),
             paste( "NRMSE% =", gof.xy["NRMSE %", 1], sep=" "),
             paste( "PBIAS% =", gof.xy["PBIAS %", 1], sep=" "),
             #paste( "pbiasFDC% =", gof.xy["pbiasFDC %", 1], sep=" "),
             paste( "RSR =", gof.xy["RSR", 1], sep=" "),
             paste( "rSD =", gof.xy["rSD", 1], sep=" "),             
             paste( "NSeff =", gof.xy["NSeff", 1], sep=" "),
             paste( "mNSeff =", gof.xy["mNSeff", 1], sep=" "),
             paste( "rNSeff =", gof.xy["rNSeff", 1], sep=" "),
             paste( "d =", gof.xy["d", 1], sep=" "),
             paste( "md =", gof.xy["md", 1], sep=" "),
             paste( "rd =", gof.xy["rd", 1], sep=" "),
             #paste( "cp =", gof.xy["cp", 1], sep=" "),
             paste( "r =", gof.xy["r", 1], sep=" "),
             paste( "R2 =", gof.xy["R2", 1], sep=" "), 
             paste( "bR2 =", gof.xy["bR2", 1], sep=" "),
             paste( "KGE =", gof.xy["KGE", 1], sep=" ")               
            ), title="GoF's:", title.col="darkblue",
             bg="azure"
           )
         
  } #IF END
  
} # 'plot2' end
