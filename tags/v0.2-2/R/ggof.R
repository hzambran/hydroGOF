###########################################################################
# 'ggof': Graphical comparision bewtween two vectors (numeric, ts or zoo),#
#         with several numerical goodness of fit printed as a legend      #
###########################################################################
# External package required: 'zoo'                              #
# External library required: 'lib_TSM_in_Hydrological_modelling #
################################################################# 
#  Started:  03 Mar 2009                #
#  Updates:  Apr, May 2009              #
#            2010                       #
#                                       # 
#########################################     


# 'var.types'	    : string representing the type of variable being plotted 
#                     (e.g., "Precipitation", "Temperature", "Flow",...). 
#                     ONLY used for labelling the axes 
# 'var.units'	    : string representing the measurement unit of the variable 
#                     being plotted (e.g., "mm" for precipitation, "C" for temperature,
#                     "m3/s" for flow,...).
#                     ONLY used for labelling the axes
# 'main'            : string representing the main title of the plot
# 'xlab'            : label for the 'x' axis
# 'ylab'            : label for the 'y' axis 
# 'ts.col'          : vector with the colors of 'sim' and 'obs'
# 'ts.lwd'          : vector with the line width of sim' and 'obs'
# 'ts.lty'          : vector with the line type of sim' and 'obs'
# 'ts.pch'          : vector with the type of symbol for 'x' and 'y'. 
#                     1: whithe circle; 9: white rhombus with a cross inside
# 'ts.cex'          : vector with the values controlling the size of text and 
#                     symbols of 'x' and 'y' with respect to the default
# 'ftype'           : string indicating the time frequency of the plots desired by the user. 
#                     Valid values are:
#                     -) 'o'  : only the original 'sim' and 'obs' time series are plotted
#                     -) 'dm' : it assumes that 'sim' and 'obs' are daily time series
#                               and Daily and Monthly values are plotted  
#                     -) 'ma' : it assumes that 'sim' and 'obs' are monthly time series
#                               and Monthly and Annual values are plotted
#                     -) 'dma': it assumes that 'sim' and 'obs' are daily time series
#                               and Daily, Monthly and Annual values are plotted 
# 'pt.style'        : String that indicates if the 2 ts have to be plotted as lines or bars
#                     When 'ftype' is NOT 'o', it only applies for the annual values
#                     Valid values are:
#                     -) "ts" : (default) each ts is ploted as a lines along the 'x' axis
#                     -) "bar": the 2 series are plotted as a barplot. 
# 'tick.tstep'      : string indicating the time step that have to be used for 
#                     putting the ticks ont he time axis. 
#                     Possible values are: 'days', 'months', 'years' 
# 'lab.tstep'       : string indicating the time step that have to be used for 
#                     putting the labels ont he time axis. 
#                     Possible values are: 'days', 'months', 'years'
# 'gof.leg'         : boolean indicating if several goodness of fit have to be 
#                     computed between both ts, and ploted as legends on the graph.
#                     If gof.leg=TRUE, then 'x' is considered as observed and 'y'
#                     as simulated values (for some gof functions this is important)
# 'digits'          : OPTIONAL, only used when 'gof.leg=TRUE'. Decimal places used for rounding the goodness-of-fit indexes
# 'leg.cex'         : Used for the GoF legend. Character expansion factor *relative* to current
#                     'par("cex")'.  Used for text, and provides the default 
#                     for 'pt.cex' and 'title.cex'. Default value = 0.7
# 'FUN'             : ONLY required when 'ftype' is in c('dm', 'ma', 'dma')
#                     Function that have to be applied for transforming from daily to monthly or annual time step
#                     For precipitation FUN MUST be "sum"
#                     For temperature and flow time series, FUN MUST be "mean"#             
# 'na.rm'           : Logical. ONLY matters when 'step.out' is "monthly' or 'annual'
#                     TRUE : the annual mean  value is computed considering only those values different from NA
#                     FALSE: if there is AT LEAST one NA within a year, the monthly mean value is NA 
# cal.ini           : OPTIONAL. Character with the date in which the calibration period started.
#                     ONLY used for drawing a vertical red line at this date. 
# val.ini           : OPTIONAL. Character with the date in which the validation period started.
#                     ONLY used for drawing a vertical red line at this date. 
# 'dates'           : Dates for the correponding values in the 'sim' and 'obs' time series
#                     If 'dates' is a factor, it have to be converted into 'Date' class, 
#                     using the date format  specified by 'date.fmt'
#                     If 'dates' is already of Date class, the number of dates
#                     must be equal to the number of elements in 'sim' and 'obs'
# date.fmt          : character indicating the format in which the dates entered are stored in 'cal.ini' adn 'val.ini'. Default value is "\%Y-\%m-\%d"
# 'cex.axis'        : magnification of axis annotation relative to 'cex'.
# 'cex.lab'         : Magnification to be used for x and y labels relative to the current setting of 'cex'. See '?par'.
                                               
      
ggof <- function (sim, obs, 
                  na.rm=TRUE, 
                  dates, 
                  date.fmt="%Y-%m-%d",

                  pt.style="ts",
                  ftype="o", 
                  FUN,
                  
                  gof.leg = TRUE, 
                  digits=2, 
                  
                  legend=c("Sim", "Obs"),
                  leg.cex=1,
                  
                  tick.tstep= "months", 
                  lab.tstep= "years",  
                  lab.fmt,
                  
                  cal.ini=NA, 
                  val.ini=NA,                
                  
                  main, 
                  xlab="Time", 
                  ylab=c("Q, [m3/s]"),  
                  
                  col= c("blue", "black"), 
                  
                  cex= c(0.5,0.5),
                  cex.axis=1.2,
                  cex.lab=1.2,
                  
                  lwd= c(1,1), 
                  lty= c(1,3), 
                  pch= c(1,9),                
                   
                   ...) {

  # requesting 'hydroTSM' package: 'sfreq', 'vector2zoo', 'daily2monthly', 'monthly2annual', 'daily2annual'
  require(hydroTSM)

  # Checking that the user provied a valid argument for 'sim'       
  if (is.na(match(class(sim), c("zoo", "numeric", "integer") ) ) ) 
         stop("Invalid argument: 'class(sim)' must be in c('zoo', 'numeric', 'integer')")
         
  # Checking that the user provied a valid argument for 'obs'       
  if (is.na(match(class(obs), c("zoo", "numeric", "integer") ) ) ) 
         stop("Invalid argument: 'class(obs)' must be in c('zoo', 'numeric', 'integer')")
         
  # Checking that the user provied the same length for 'sim' and 'obs'      
  if ( length(sim) != length(obs) )  
         stop(paste("Invalid argument: 'obs' and 'sim' must have the same length ! (", 
                   length(obs), "vs", length(sim), ")"  ,sep=" ") )
                   
  require(hydroTSM) # for using the 'sfreq' function
  # Checking that the user provied the same sampling frequency for 'sim' and 'obs',
  # when 'sim' and 'obs' are 'zoo' objects      
  if ( !is.na(match(class(obs), c("zoo") ) ) ) {
      if ( sfreq(sim) != sfreq(obs) ) {
         stop(paste("Invalid argument: 'obs' and 'sim' have different sampling frequency ! (", 
                   sfreq(obs), "vs", sfreq(sim), ")"  ,sep=" ") ) }
  } # IF end
         
  # Checking that the user provied a valid argument for 'ftype'       
  if (is.na(match(ftype, c("o", "dm", "ma", "dma") ) ) ) 
         stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma')")
         
  # Checking that the user provied a valid argument for FUN when 'ftype' involves monthly or annual values     
  if (!is.na(match(ftype, c("dm", "ma", "dma") ) ) & missing(FUN) ) 
         stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma')")
         
  # If the user didn't provide a title for the plot, the default is used 
  if ( missing(main) ) main <- "Observations vs Simulations"
         
         
  # Requiring the Zoo Library (Zoo's ordered observations): 'is.zoo', 'as.zoo', and 'plot.zoo' functions
  require(zoo)
  
  # If the user provided values 'for 'dates'
  if (!missing(dates)) {
  
    # Checking that 'dates' have the same length than 'sim' ( and 'obs')      
    if ( length(dates) != length(sim) )  
         stop("Invalid argument: 'dates' and 'sim' must have the same length")
  
    # Checking that 'dates' have the right class
    if (is.na(match(class(dates), c("character", "factor", "Date")))) 
        stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date')")
        
    # If 'dates' is a factor or character, it have to be converted into 'Date' class, 
    # using the date format  specified by 'date.fmt'
     if ( !is.na( match(class(dates), c("factor", "character") ) ) ) 
        dates <- as.Date(dates, format= date.fmt)    
    
    # If 'obs' is 'zoo' and the user provides the dates (probably new dates)
    if ( is.zoo(obs) ) { time(obs) <- dates }  
    # If 'sim' is 'zoo' and the user provides the dates  (probably new dates)
    if ( is.zoo(sim) ) { time(sim) <- dates }  
    
  } else if (!is.zoo(obs)) message("[Note: You didn't provide dates, so only a numeric index will be used in the time axis.]")      
 
  
  require(hydroTSM) # for using the 'vector2zoo' function 
  
  # If 'class(obs)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !is.zoo(obs) & !missing(dates) ) { 
    obs <- vector2zoo(x=obs, dates=dates, date.fmt=date.fmt)        
  } # If 'class(obs)' is 'zoo' and 'dates' are missing, dates are extracted from 'obs'
    else if ( is.zoo(obs) & missing(dates) ) {  
      # class(time(x))== "Date" for 'daily' and 'monthly' time series
      # class(time(x))== "character" for 'annual' time series
      if ( class(time(obs)) == "Date" ) { dates <- time(obs) 
      } else if ( class(time(obs)) == "character" ) {  
             dates <- as.Date(time(obs), format="%Y") }      
    } #ELSE END
  
  # If 'class(sim)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !is.zoo(sim) & !missing(dates) ) { 
    sim <- vector2zoo(x=sim, dates=dates, date.fmt=date.fmt) 
  # If 'class(sim)' is 'zoo' and 'dates' are missing, dates are extracted from 'sim'
  } else if ( is.zoo(sim) & is.zoo(obs) & missing(dates) ) {
      # class(time(x))== "Date" for 'daily' and 'monthly' time series
      # class(time(x))== "character" for 'annual' time series
      if ( class(time(sim)) == "Date" ) { dates <- time(obs) 
      } else if ( class(time(sim)) == "character" ) {  
             dates <- as.Date(time(sim), format="%Y") }
    } #ELSE END  


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
  
  
  #Plotting acoording to the 'ftype' value:  
  if (ftype == "o") {
        
   # Drawing the original time series against time
   plot2(x=sim, y=obs, plot.type="single",
         main= main, 
         col= col, lwd= lwd, lty=lty, pch=pch,
         xlab= xlab, ylab= ylab, pt.style= pt.style,
         add= FALSE,
         tick.tstep, lab.tstep, lab.fmt=lab.fmt,
         cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
         gof.leg = gof.leg, gof.digits=digits,
         legend=legend, leg.cex=leg.cex,
         cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ...)
         
  } else if (ftype=="dm") {
    
      if (sfreq(sim) != "daily") {      
        stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")       
      } else {
          # Generating a Monthly time series (Monthly mean of daily values):
          obs.monthly <- daily2monthly(obs, FUN, na.rm)
          sim.monthly <- daily2monthly(sim, FUN, na.rm)
          
          def.par <- par(no.readonly = TRUE) # save default, for resetting... 
          on.exit(par(def.par)) 
          
          # If the user wants a legend, the screen is splitted into 2 rows and 2 colums, 
          # where the proportion of width of the 1st column to the 2nd one is 9:2
          if (gof.leg) {           
            layout( matrix( c(1,1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,3,3,3,4,4), ncol=11, byrow=TRUE) ) 
          } else {
             # Setting up the screen with 2 rows and 1 column
             par(mfrow=c(2,1))
            } #ELSE end
          
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)  
          # Drawing the original daily time series against time
          plot2(x=sim, y=obs, plot.type="single",
                main=paste("Daily", main, sep=" "), 
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch,  
                xlab= xlab, ylab= ylab, 
                pt.style= "ts", 
                add= TRUE,  
                gof.leg = gof.leg, gof.digits=digits,
                legend=legend, leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
          
          # It is necessay to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)           
          # Drawing the Monthly time series against time
          plot2(x=sim.monthly, y=obs.monthly, plot.type="single",
                main=paste("Monthly", main, sep=" "), 
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch, 
                xlab= xlab, ylab= ylab, 
                pt.style= "ts", 
                add= TRUE, 
                gof.leg = gof.leg, gof.digits=digits,
                legend=legend, leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
                   
            
        } # ELSE end
  } # ELSE if (ftype=="dm") END
  
  else if (ftype=="ma") {
  
    if  ( is.na( match( sfreq(sim), c("daily", "monthly") ) ) ) {      
      stop("Invalid argument: the sampling frequency of 'sim' has to be in c('daily', 'monthly'")       
    } else {
        if ( sfreq(sim) == "daily" ) {
           # Generating a Monthly time series (Monthly mean of daily values):
           obs <- daily2monthly(obs, FUN, na.rm)
           sim <- daily2monthly(sim, FUN, na.rm)
        } # IF end
        
        # Generating Annual time series (Annual mean of daily values)
        obs.annual <- monthly2annual(obs, FUN, na.rm, out.fmt="%Y-%m-%d")
        sim.annual <- monthly2annual(sim, FUN, na.rm, out.fmt="%Y-%m-%d")
        
        def.par <- par(no.readonly = TRUE) # save default, for resetting... 
        on.exit(par(def.par)) 
        
        # If the user wants a legend, the screen is splitted into 2 rows and 2 colums, 
        # where the proportion of width of the 1st column to the 2nd one is 9:2
        if (gof.leg) {     
          layout( matrix( c(1,1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,3,3,3,4,4), ncol=11, byrow=TRUE) )
        } else {
           # Setting up the screen with 2 rows and 1 column
           par(mfrow=c(2,1))
          } #ELSE end
        
        par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)   
        # Drawing the Monthly time series against time
        plot2(x=sim, y=obs, plot.type="single",
              main=paste("Monthly", main, sep=" "), 
              tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
              cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
              col = col, lwd= lwd, lty=lty, pch=pch,
              xlab= xlab, ylab= ylab, pt.style= "ts", 
              add= TRUE, 
              gof.leg = gof.leg, gof.digits=digits,
              legend=legend, leg.cex=leg.cex,
              cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
        
        # It is necessay to set up the margins again, after the previous call to plot2
        par(mar=c(5, 4, 4, 0) + 0.1)                
        # Drawing the Annual time series against time
        plot2(x=sim.annual, y=obs.annual, plot.type="single",
              main=paste("Annual", main, sep=" "), 
              tick.tstep="years", lab.tstep= "years", 
              cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, lab.fmt=lab.fmt,
              col = col, lwd= lwd, lty=lty, pch=pch, 
              xlab= xlab, ylab= ylab, pt.style= pt.style, 
              add= TRUE, 
              gof.leg = gof.leg, gof.digits=digits,
              legend=legend, leg.cex=leg.cex,
              cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
      } # ELSE end      
   
  } # ELSE if (ftype=="ma") END
  
  else if (ftype=="dma") {
        
    if (sfreq(sim) != "daily") {      
      stop("Invalid argument: the 'sim' has to have a 'Daily' sampling frequency")  
           
    } else {       
          # Generating Monthly time series (Monthly mean of daily values):
          obs.monthly <- daily2monthly(obs, FUN, na.rm)
          sim.monthly <- daily2monthly(sim, FUN, na.rm)
          
          # Generating Annual time series (Annual mean of daily values)
          obs.annual <- daily2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d")
          sim.annual <- daily2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d")
          
          def.par <- par(no.readonly = TRUE) # save default, for resetting... 
          on.exit(par(def.par)) 
          
          # If the user wants a legend, the screen is splitted into 2 rows and 2 colums, 
          # where the proportion of width of the 1st column to the 2nd one is 9:2
          if (gof.leg) {   
            # Setting up a screen with 3 rows and 2 columns 
            layout( matrix( c(1,1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,3,3,3,4,4,5,5,5,5,5,5,5,5,5,6,6), ncol=11, byrow=TRUE) ) 
          } else {
             # Setting up the screen with 3 rows and 1 column
             par(mfrow=c(3,1))
            } #ELSE end  
          
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1) 
          # Drawing the original daily time series against time
          plot2(x=sim, y=obs, plot.type="single",
                main=paste("Daily", main, sep=" "), 
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch,
                xlab= xlab, ylab= ylab, pt.style= "ts", 
                add= TRUE, 
                gof.leg = gof.leg, gof.digits=digits,
                legend=legend, leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
          
          # It is necessay to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)        
          # Drawing the Monthly time series against time
          plot2(x=sim.monthly, y=obs.monthly, plot.type="single",  
                main=paste("Monthly", main, sep=" "), 
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch, 
                xlab= xlab, ylab= ylab, pt.style= "ts", 
                add= TRUE, 
                gof.leg = gof.leg, gof.digits=digits,
                legend=legend, leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
           
          # It is necessay to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)        
          # Drawing the Annual time series against time
          plot2(x=sim.annual, y=obs.annual, plot.type="single",
                  main=paste("Annual", main, sep=" "), 
                  tick.tstep="years", lab.tstep= "years", lab.fmt=lab.fmt,
                  cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                  col = col, lwd= lwd, lty=lty, pch=pch,
                  xlab= xlab, ylab= ylab, pt.style= pt.style, 
                  add= TRUE, 
                  gof.leg = gof.leg, gof.digits=digits,
                  legend=legend, leg.cex=leg.cex,
                  cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
      } # ELSE end
            
  } # ELSE if (ftype=="dma") END
  
} # 'ggof' end
