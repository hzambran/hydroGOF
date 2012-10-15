# File ggof.Rd
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2012 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# 'ggof': Graphical comparison between two vectors (numeric, ts, zoo, xts),    #
#         with several numerical goodness-of-fit measures as a legend          #
################################################################################
#  Started:  03 Mar 2009                                                       #
#  Updates:  Apr, May 2009                                                     #
#            2010                                                              #
#            17-Apr-2011                                                       # 
#            15-Oct-2012                                                       #
################################################################################     
                                          
      
ggof <- function (sim, obs, 
                  na.rm=TRUE, 
                  dates, 
                  date.fmt="%Y-%m-%d",

                  pt.style="ts",
                  ftype="o", 
                  FUN,
                  
                  gof.leg = TRUE, 
                  digits=2, 
                  
                  legend,
                  leg.cex=1,
                  
                  tick.tstep= "auto", 
                  lab.tstep= "auto",  
                  lab.fmt=NULL,
                  
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

  # Checking class 'sim' &'obs'   
  valid.class <- c("xts", "zoo", "numeric", "integer")    
  if (length(which(!is.na(match(class(sim), valid.class )))) <= 0)  
         stop("Invalid argument: 'class(sim)' must be in c('xts', 'zoo', 'numeric', 'integer')")
  if (length(which(!is.na(match(class(obs), valid.class )))) <= 0)
         stop("Invalid argument: 'class(obs)' must be in c('xts', 'zoo', 'numeric', 'integer')")
         
  # Checking length
  if ( length(sim) != length(obs) )  
     stop("Invalid argument: 'obs' and 'sim' must have the same length ! (", 
          length(obs), "vs", length(sim), ")")
                
  # 'xname' and 'yname' values
  sim.name <- deparse(substitute(sim))
  obs.name <- deparse(substitute(obs))

  # 'legend' value
  if (missing(legend)) legend <- c(sim.name, obs.name)
                   
  # Checking same sampling frequency
  if ( zoo::is.zoo(obs) & zoo::is.zoo(sim)) {
#      sim.freq <- xts::periodicity(sim)$scale
#      obs.freq <- xts::periodicity(obs)$scale
#      if ( sim.freq != obs.freq )
#         stop("Invalid argument: 'obs' and 'sim' have different sampling frequency ! (", 
#              obs.freq, " vs ", sim.freq, ")" )
      if (all.equal(time(obs), time(sim)) != TRUE)
        stop("Invalid argument: 'obs' and 'sim' have different time stamps !")
  } # IF end    
          
  # If the user provided values 'for 'dates'
  if (!missing(dates)) {
  
    # Checking that 'dates' have the same length than 'sim' ( and 'obs')      
    if ( length(dates) != length(sim) )  
        stop("Invalid argument: 'dates' and 'sim' must have the same length")
  
    # Checking that 'dates' have the right class
    if (is.na(match(class(dates), c("character", "factor", "Date", "POSIXct")))) 
        stop("Invalid argument: 'class(dates)' must be in c('character', 'factor', 'Date', 'POSIXct')")
        
    # If 'dates' is a factor or character, it have to be converted into 'Date' class, 
    # using the date format  specified by 'date.fmt'
     if ( class(dates)[1] %in% c("factor", "character") ) {
        ifelse ( grepl("%H", date.fmt, fixed=TRUE) | grepl("%M", date.fmt, fixed=TRUE) |
             grepl("%S", date.fmt, fixed=TRUE) | grepl("%I", date.fmt, fixed=TRUE) |
             grepl("%p", date.fmt, fixed=TRUE) | grepl("%X", date.fmt, fixed=TRUE),
             subdaily <- TRUE, subdaily <- FALSE )
        ifelse(subdaily, dates <- as.POSIXct(dates, format= date.fmt), 
                         dates <- as.Date(dates, format= date.fmt) )  
     } # IF end  
    
    # If 'obs' is 'zoo' and the user provides the dates (probably new dates)
    if ( zoo::is.zoo(obs) ) time(obs) <- dates
    # If 'sim' is 'zoo' and the user provides the dates  (probably new dates)
    if ( zoo::is.zoo(sim) ) time(sim) <- dates   
    
  } else if (!zoo::is.zoo(obs)) 
            message("[ Note: You did not provide dates, so only a numeric index will be used in the time axis ]")    
  
  # If 'class(obs)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !zoo::is.zoo(obs) & !missing(dates) ) { 
    obs <- hydroTSM::vector2zoo(x=obs, dates=dates, date.fmt=date.fmt)        
  } # If 'class(obs)' is 'zoo' and 'dates' are missing, dates are extracted from 'obs'
    else if ( zoo::is.zoo(obs) & missing(dates) ) {  
      # class(time(x))== "POSIXct" for sub-daily time series
      # class(time(x))== "Date" for 'daily' and 'monthly' time series
      # class(time(x))== "character" for 'annual' time series
      if ( class(time(obs))[1] %in% c("Date", "POSIXct") ) { 
         dates <- time(obs) 
      } else if ( class(time(obs))[1] == "character" )  
                 dates <- as.Date(time(obs), format="%Y")      
    } #ELSE END
  
  # If 'class(sim)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !zoo::is.zoo(sim) & !missing(dates) ) { 
    sim <- hydroTSM::vector2zoo(x=sim, dates=dates, date.fmt=date.fmt) 
  # If 'class(sim)' is 'zoo' and 'dates' are missing, dates are extracted from 'sim'
  } else if ( zoo::is.zoo(sim) & zoo::is.zoo(obs) & missing(dates) ) {
      # class(time(x))== "POSIXct" for sub-daily time series
      # class(time(x))== "Date" for 'daily' and 'monthly' time series
      # class(time(x))== "character" for 'annual' time series
      if ( class(time(sim))[1]  %in% c("Date", "POSIXct") ) { 
         dates <- time(obs) 
      } else if ( class(time(sim))[1] == "character" ) {  
             dates <- as.Date(time(sim), format="%Y") }
    } #ELSE END    
    
  # Checking 'ftype'       
  if (is.na(match(ftype, c("o", "dm", "ma", "dma") ) ) ) 
      stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma')")
  
  # If 'obs' and 'sim' are not zoo objects, the only possible value for 'ftype' is 'o'     
  if ( !zoo::is.zoo(sim) & !zoo::is.zoo(sim) ) {
     if (!is.na(match(ftype, c("dm", "ma", "dma") ) ) ) 
      message("[ Note: 'sim' & 'obs' are not zoo objects => 'ftype' was changed to 'o' ]")
      ftype <- "o"
  } else if ( zoo::is.zoo(sim) ) 
          sim.freq <- xts::periodicity(sim)$scale
         
  # Checking FUN, when 'ftype' involves monthly or annual values     
  if (!is.na(match(ftype, c("dm", "ma", "dma") ) ) & missing(FUN) ) 
         stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma')")

  # If the user did not provide a title for the plot, the default is used 
  if ( missing(main) ) main <- "Observations vs Simulations"     
  
  #Plotting according to the 'ftype' value:  
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
    
      if (sim.freq != "daily") {      
        stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")       
      } else {
          # Generating a Monthly time series
          obs.monthly <- hydroTSM::daily2monthly(obs, FUN, na.rm)
          sim.monthly <- hydroTSM::daily2monthly(sim, FUN, na.rm)
          
          def.par <- par(no.readonly = TRUE) # save default, for resetting... 
          on.exit(par(def.par)) 
          
          # If the user wants a legend, the screen is split into 2 rows and 2 columns, 
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
          
          # It is necessary to set up the margins again, after the previous call to plot2
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
  
    if  ( is.na( match( sim.freq, c("daily", "monthly") ) ) ) {      
      stop("Invalid argument: the sampling frequency of 'sim' has to be in c('daily', 'monthly'")       
    } else {
        if ( sim.freq == "daily" ) {
           # Generating a Monthly time series 
           obs <- hydroTSM::daily2monthly(obs, FUN, na.rm)
           sim <- hydroTSM::daily2monthly(sim, FUN, na.rm)
        } # IF end
        
        # Generating Annual time series
        obs.annual <- hydroTSM::monthly2annual(obs, FUN, na.rm, out.fmt="%Y-%m-%d")
        sim.annual <- hydroTSM::monthly2annual(sim, FUN, na.rm, out.fmt="%Y-%m-%d")
        
        def.par <- par(no.readonly = TRUE) # save default, for resetting... 
        on.exit(par(def.par)) 
        
        # If the user wants a legend, the screen is split into 2 rows and 2 columns, 
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
        
        # It is necessary to set up the margins again, after the previous call to plot2
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
        
    if (sim.freq != "daily") {      
      stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")  
           
    } else {       
          # Generating Monthly time series 
          obs.monthly <- hydroTSM::daily2monthly(obs, FUN, na.rm)
          sim.monthly <- hydroTSM::daily2monthly(sim, FUN, na.rm)
          
          # Generating Annual time series 
          obs.annual <- hydroTSM::daily2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d")
          sim.annual <- hydroTSM::daily2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d")
          
          def.par <- par(no.readonly = TRUE) # save default, for resetting... 
          on.exit(par(def.par)) 
          
          # If the user wants a legend, the screen is split into 2 rows and 2 columns, 
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
          
          # It is necessary to set up the margins again, after the previous call to plot2
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
           
          # It is necessary to set up the margins again, after the previous call to plot2
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
