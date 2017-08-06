# File plot2.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2017 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'plot2':     Plots 2 time series on the same graph                           #
#              It is a wrapper for the 'plot.zoo'                              #
################################################################################
# Author : Mauricio Zambrano-Bigiarini                                         #
################################################################################
# Started: March 04, 2009                                                      #
# Updates: May 2009                                                            #
#          2010                                                                #
#          21-Jan-2011 ; 15-Apr-2011 ;                                         #
#          25-Aug-2011 ; 31-Aug-2011 ; 14-Sep-2011                             #
#          23-Jan-2012                                                         #
#          15-Apr-2013 ; 15-May-2013                                           #
#          06-Aug-2017                                                         #
################################################################################
                
plot2 <- function (x, y, 
                   plot.type="multiple", 
                   
                   tick.tstep="auto", 
                   lab.tstep="auto", 
                   lab.fmt=NULL,
                   
                   main, 
                   xlab="Time", 
                   ylab,
                   
                   cal.ini=NA, 
                   val.ini=NA, 
                   date.fmt="%Y-%m-%d",                   
                   
                   gof.leg= FALSE, 
                   gof.digits=2, 
                   gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", 
                          "NSE", "mNSE", "rNSE", "d", "md", "rd", "r", "R2", 
                          "bR2", "KGE", "VE"),
                   
                   legend,
                   leg.cex=1,                       
                        
                   col = c("black","blue"),
                   
                   cex=c(0.5, 0.5),
                   cex.axis=1.2,
                   cex.lab=1.2,
                   
                   lwd= c(1,1), 
                   lty= c(1,3), 
                   pch= c(1,9),   
                   
                   pt.style = "ts",
                   add=FALSE,                   
                   
                    ...) {
                   
  # Checking that the user provided 'x'
  if ( missing(x) ) stop("Missing argument: 'x'")
         
  # Checking that the user provided 'y'
  if ( missing(y) ) stop("Missing argument: 'y'")

  # Checking 'gofs'
  gofs.all=c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", 
             "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")  
  if (length(noNms <- gofs[!gofs %in% gofs.all])) 
    warning("[Unknown names in 'gofs': ", paste(noNms, collapse = ", "), " (not used) !]")		   

  # 'xname' and 'yname' values
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  # 'legend' value
  if (missing(legend)) legend <- c(xname, yname)

  # 'ylab' value
  if (missing(ylab)) ylab <- c(xname, yname)
  
  # Checking that the user provided a valid argument for 'x' and 'y'
  valid.class <- c("xts", "zoo", "ts", "numeric", "integer")       
  if (length(which(!is.na(match(class(x), valid.class )))) <= 0) 
         stop("Invalid argument: 'class(x)' must be in c('integer', 'numeric', 'ts', 'zoo', 'xts')")
  if (length(which(!is.na(match(class(y), valid.class )))) <= 0)
         stop("Invalid argument: 'class(y)' must be in c('integer', 'numeric', 'ts', 'zoo', 'xts')")
         
  # Checking that the user provided a valid argument for 'plot.type'       
  if (is.na(match(plot.type, c("single", "multiple") ) ) ) 
         stop("Invalid argument: 'plot.type' must be in c('single', 'multiple')")
         
  # If the user wants to draw a legend, it checks that the type of plot is 'single'
  if (gof.leg & (plot.type == "multiple") )
    stop("Invalid argument: For drawing a legend, 'plot.type' must be 'single'")
         
  # Checking that the user provided a valid argument for 'pt.style'       
  if (is.na(match(pt.style, c("ts", "bar") ) ) ) 
         stop("Invalid argument: 'pt.style' must be in c('ts', 'bar')")
         
  # If 'x' is 'ts' or 'zoo' and 'y' is only a vector, y is transformed into 
  # the same class of 'x', with the same times
  if ( (length(which(!is.na(match(class(x), c("ts", "zoo", "xts") )))) <= 0) & 
       (length(which(!is.na(match(class(y), c("integer", "numeric") )))) <= 0) ) {  
  
    # class(time(x))== "Date" for 'daily' and 'monthly' time series
    # class(time(x))== "character" for 'annual' time series
    if ( class(time(x)) == "Date" ) {
        y <- vector2zoo(y, dates=time(x)) # hydroTSM::vector2zoo
    } else if ( class(time(x)) == "character" ) {
        y <- vector2zoo(y, dates=time(x), date.fmt="%Y") # hydroTSM::vector2zoo
        time(x) <- time(y) #'annual' time series
    } # ELSE END
    
  } # IF END

         
  # Checking that the user provied the same length for 'sim' and 'obs'      
  #if ( length(x) != length(y) )  
  #       stop("Invalid argument: 'obs' and 'sim' must have the same length")
         
  # If the user didn't provide a title for the plot, the default is used 
  if ( missing(main) ) main <- "Observed vs Simulated"   
  
  # If the user provided a value for 'cal.ini', it is transformed into a Date class
  if ( !missing(cal.ini) ) cal.ini <- as.Date(cal.ini, format=date.fmt)
    
  # If the user provided a value for 'val.ini', it is transformed into a Date class
  if ( !missing(val.ini) ) val.ini <- as.Date(val.ini, format=date.fmt)
  
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
  
  # If the plot type is "time series"
  if (pt.style=="ts") {
  
    # If both time series have to be ploted in the same plot area
    if (plot.type == "single") {

      if (length(which(!is.na(match(class(x), c("ts", "zoo", "xts") )))) > 0) x <- zoo::as.zoo(x) 
      if (length(which(!is.na(match(class(y), c("ts", "zoo", "xts") )))) > 0) y <- zoo::as.zoo(y) 

      # Y axis limits
      if (!hasArg(ylim)) ylim <- range( range(x[is.finite(x)]), range(y[is.finite(y)]) )

      # Plotting the Observed Time Series. 
      # It calls to 'plot' or 'plot.zoo'  depending on 'x' class
      if (!hasArg(ylim)) {
         zoo::plot.zoo(x, xaxt = "n", yaxt = "n",
                       type="o", lwd=lwd[1], lty= lty[1], col= col[1], pch= pch[1], 
                       cex = cex[1], cex.axis=cex.axis, cex.lab=cex.lab,
                       main=main, xlab=xlab, ylab= ylab, ylim=ylim, ... )
      } else zoo::plot.zoo(x, xaxt = "n", yaxt = "n", 
                      type="o", lwd=lwd[1], lty= lty[1], col= col[1], pch= pch[1], 
                      cex = cex[1], cex.axis=cex.axis, cex.lab=cex.lab,
                      main=main, xlab=xlab, ylab= ylab, ... )
      axis(2, cex.axis=cex.axis, cex.lab=cex.lab)
     
      lines(y, type="o", lwd=lwd[2], lty= lty[2], col= col[2], pch= pch[2], cex = cex[2])      
               
      # If the user provided a value for 'cal.ini', a vertical line is drawn
      if ( !missing(cal.ini) ) abline(v=as.POSIXct(cal.ini), col="red", lty=1, lwd=2)
      
      # If the user provided a value for 'val.ini', a vertical line is drawn
      if ( !missing(val.ini) ) abline(v=as.POSIXct(val.ini), col="red", lty=1, lwd=2)
               
      # Drawing a legend with 'Obs' vs 'Sim' 
      # y.intersp=0.5, is for the vertical spacin in the legend
      # bty="n" => no box around the legend
      # 'inset=0.03' is usefult when plot.type= "multiple" for having a nice margin to the legend
      legend("topright", legend=legend, y.intersp=0.8, inset=0.03,
             bty="n", cex = leg.cex, col = col, lwd= lwd, lty= lty, pch=pch )  
      
      # Drawing the 'x' axis
      # If the user provided, in some way, valid values for being used as dates, 
      # they will be used, if not, only a numeric index will be used
      if ( (length(which(!is.na(match(class(x), c("ts", "zoo", "xts") )))) > 0) | 
           (length(which(!is.na(match(class(y), c("ts", "zoo", "xts") )))) > 0) ) {
    
        if ( (length(which(!is.na(match(class(x), c("ts", "zoo", "xts") )))) > 0) ) { 
          z <- x
        } else z <- y
    
        # Draws ticks in the X axis, but labels only in years
        drawTimeAxis(z, tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt, cex.axis=cex.axis, cex.lab=cex.lab) # hydroTSM::drawTimeAxis

        # manually adding a grid
        grid(nx=NA, ny=NULL)
        abline(v=time(x)[axTicksByTime(x)], col = "lightgray", lty = "dotted")
      
      } else { # When 'numeric' or 'integer' values (not 'zoo' or 'xts') are plotted
             Axis(side = 1, labels = TRUE, cex.axis=cex.axis, cex.lab=cex.lab)
             box()
             # manually adding a grid
             grid()
             }
               
    } else  #plot.type == "multiple"  
          {       
            # all the work (mainly Time axis) is made automatically be the 'plot.zoo' function 
            zoo::plot.zoo(cbind(x, y), plot.type=plot.type, type=c("o","o"), 
                         lwd=lwd, lty= lty, col= col, pch= pch, 
                         cex = cex, 
                         cex.axis=cex.axis, 
                         cex.lab=cex.lab,
                         main=main, xlab=xlab, ylab= ylab,...)
                         
            # If the user provided a value for 'cal.ini', a vertical line is drawn
            if ( !missing(cal.ini) ) abline(v=as.POSIXct(cal.ini), col="red", lty=1, lwd=2)
      
            # If the user provided a value for 'val.ini', a vertical line is drawn
            if ( !missing(val.ini) ) abline(v=as.POSIXct(val.ini), col="red", lty=1, lwd=2)
                         
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
                args.legend=list(x="topleft", y.intersp=0.8, inset=0.00, bty="n", 
                                 cex= cex.axis, col = col), # lwd= lwd, lty= lty, pch=pch),
                cex.axis=cex.axis, cex.lab=cex.lab, cex = cex.lab, ...)  
       
        if ( !missing(cal.ini) ) {
          # Index of the bar corresponding to 'cal.ini'.
          # It is necessary to multiply it by 3 because for each year there are 3 vertical lines
          # It is necessary to substract 2, for shifting the line form the 3 line to the first one
          cal.index <- 3*which(colnames(b) == format( cal.ini, "%Y")) - 2
          # If the user provided a value for 'cal.ini', a vertical line is drawn
          if ( !missing(cal.ini) ) {
           abline(v=cal.index, col="red", lty=1, lwd=2)
          } # IF end
        } # IF end
       
        if ( !missing(val.ini) ) {
          # Index of the bar corresponding to 'val.ini'.
          # It is necessary to multiply it by 3 because for each year there are 3 vertical lines
          # It is necessary to substract 2, for shifting the line form the 3 line to the first one
          val.index <- 3*which(colnames(b) == format( val.ini, "%Y")) - 2
          # If the user provided a value for 'val.ini', a vertical line is drawn
          if ( !missing(val.ini) ) {
            abline(v=val.index, col="red", lty=1, lwd=2)
          } # IF end
        } # IF end
      
    }  # ELSE end        
  
  
  # If the Goodness-of-fit indexes have to be computed and plotted:
  if (gof.leg & plot.type == "single" ) {
  
   # If the user provided 'cal.ini' and 'x' & 'y' are zoo objects, 
   # the warming up period is removed from the computation of the goodness-of-fit
   # measures => all the values before 'cal.ini' are ignored but plotted
   if ( is.zoo(x) & is.zoo(y) ) {
     if ( !is.na(cal.ini) ) {
       x <- window(x, start=cal.ini)
       y <- window(y, start=cal.ini)
     } # IF end 
   } # IF end

   gof.index <- pmatch(gofs, gofs.all)
   gof.index <- gof.index[!is.na(gof.index)]  
    
   gof.xy <- gof(sim=as.numeric(x), obs=as.numeric(y), do.spearman=FALSE, do.pbfdc=FALSE, digits=gof.digits, ...)

   gofs.stg  <- gofs.all[gof.index] 
   gofs.num  <- gof.xy[gof.index, 1] 

   leg.text <- paste(gofs.stg, " = ", gofs.num, sep="")
   
   legend.position <- "center"
   par( mar=c(0.5, 0.5, 0.5, 0.5) ) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)
   plot.new() 
          
   # 'inset':  The optional 'inset' argument specifies how far the legend is inset from the plot margins.  
   #           If a single value is given, it is used for both margins; 
   #           if two values are given, the first is used for 'x'-distance, the second for 'y'-distance.
	 
   legend(legend.position,  y.intersp=1.2, cex =leg.cex, # bty="n", #inset=0.01,  
          leg.text, 
#          c( paste( "ME =", gof.xy["ME", 1], sep=" "),
#             paste( "MAE =", gof.xy["MAE", 1], sep=" "),
#             #paste( "MSE =", gof.xy["MSE", 1], sep=" "),
#             paste( "RMSE =", gof.xy["RMSE", 1], sep=" "),
#             paste( "NRMSE% =", gof.xy["NRMSE %", 1], sep=" "),
#             paste( "PBIAS% =", gof.xy["PBIAS %", 1], sep=" "),
#             #paste( "pbiasFDC% =", gof.xy["pbiasFDC %", 1], sep=" "),
#             paste( "RSR =", gof.xy["RSR", 1], sep=" "),
#             paste( "rSD =", gof.xy["rSD", 1], sep=" "),             
#             paste( "NSE =", gof.xy["NSE", 1], sep=" "),
#             paste( "mNSE =", gof.xy["mNSE", 1], sep=" "),
#             paste( "rNSE =", gof.xy["rNSE", 1], sep=" "),
#             paste( "d =", gof.xy["d", 1], sep=" "),
#             paste( "md =", gof.xy["md", 1], sep=" "),
#             paste( "rd =", gof.xy["rd", 1], sep=" "),
#             #paste( "cp =", gof.xy["cp", 1], sep=" "),
#             paste( "r =", gof.xy["r", 1], sep=" "),
#             paste( "R2 =", gof.xy["R2", 1], sep=" "), 
#             paste( "bR2 =", gof.xy["bR2", 1], sep=" "),
#             paste( "KGE =", gof.xy["KGE", 1], sep=" "), 
#             paste( "VE =", gof.xy["VE", 1], sep=" ")               
#            ), 
            title="GoF's:", title.col="darkblue",
            bg="azure"
           )
         
  } #IF END
  
} # 'plot2' end
