# File ggof.Rd
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2009-2013 Mauricio Zambrano-Bigiarini
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
#            15-Apr-2013 ; 15-May-2013                                         #
################################################################################     
                                          
      
ggof <- function (sim, obs, 
                  na.rm=TRUE, 
                  dates, 
                  date.fmt="%Y-%m-%d",

                  pt.style="ts",
                  ftype="o", 
                  FUN,

                  stype="default", 
                  season.names=c("Winter", "Spring", "Summer", "Autumn"),
                  
                  gof.leg = TRUE, 
                  digits=2, 
                  gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", 
                          "NSE", "mNSE", "rNSE", "d", "md", "rd", "r", "R2", 
                          "bR2", "KGE", "VE"),
                  
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
          length(obs), " vs ", length(sim), ")")
                
  # 'xname' and 'yname' values
  sim.name <- deparse(substitute(sim))
  obs.name <- deparse(substitute(obs))

  # 'legend' value
  if (missing(legend)) legend <- c(sim.name, obs.name)
                   
  # Checking same sampling frequency
  if ( zoo::is.zoo(obs) & zoo::is.zoo(sim)) {
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
    obs <- vector2zoo(x=obs, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo       
  } # If 'class(obs)' is 'zoo' and 'dates' are missing, dates are extracted from 'obs'
    else if ( zoo::is.zoo(obs) & missing(dates) ) {  
      if ( class(time(obs))[1] %in% c("Date", "POSIXct") ) { 
         dates <- time(obs) 
      } else if ( class(time(obs))[1] == "character" )  
                 dates <- as.Date(time(obs), format="%Y")      
    } #ELSE END
  
  # If 'class(sim)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !zoo::is.zoo(sim) & !missing(dates) ) { 
    sim <- vector2zoo(x=sim, dates=dates, date.fmt=date.fmt) # hydroTSM::vector2zoo
  # If 'class(sim)' is 'zoo' and 'dates' are missing, dates are extracted from 'sim'
  } else if ( zoo::is.zoo(sim) & zoo::is.zoo(obs) & missing(dates) ) {
      if ( class(time(sim))[1]  %in% c("Date", "POSIXct") ) { 
         dates <- time(obs) 
      } else if ( class(time(sim))[1] == "character" ) {  
             dates <- as.Date(time(sim), format="%Y") }
    } #ELSE END    
    
  # Checking 'ftype'       
  if (is.na(match(ftype, c("o", "dm", "ma", "dma", "seasonal") ) ) ) 
      stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma', 'seasonal')")
  
  # If 'obs' and 'sim' are not zoo objects, the only possible value for 'ftype' is 'o'     
  if ( !zoo::is.zoo(sim) & !zoo::is.zoo(sim) ) {
     if (!is.na(match(ftype, c("dm", "ma", "dma", "seasonal") ) ) ) 
      message("[ Note: 'sim' & 'obs' are not zoo objects => 'ftype' was changed to 'o' ]")
      ftype <- "o"
  } else if ( zoo::is.zoo(sim) ) 
          sim.freq <- xts::periodicity(sim)$scale
         
  # Checking FUN, when 'ftype' involves monthly or annual values     
  if (!is.na(match(ftype, c("dm", "ma", "dma", "seasonal") ) ) & missing(FUN) ) 
         stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma', 'seasonal')")

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
         gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
         legend=legend, leg.cex=leg.cex,
         cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ...)
         
  } else if (ftype=="dm") {
    
      if (sim.freq != "daily") {      
        stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")       
      } else {
          # Generating a Monthly time series
          obs.monthly <- daily2monthly(obs, FUN, na.rm) # hydroTSM::daily2monthly
          sim.monthly <- daily2monthly(sim, FUN, na.rm) # hydroTSM::daily2monthly
          
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
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
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
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
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
           obs <- daily2monthly(obs, FUN, na.rm) # hydroTSM::daily2monthly
           sim <- daily2monthly(sim, FUN, na.rm) # hydroTSM::daily2monthly
        } # IF end
        
        # Generating Annual time series
        obs.annual <- monthly2annual(obs, FUN, na.rm, out.fmt="%Y-%m-%d") # hydroTSM::monthly2annual
        sim.annual <- monthly2annual(sim, FUN, na.rm, out.fmt="%Y-%m-%d") # hydroTSM::monthly2annual
        
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
              gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
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
              gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
              legend=legend, leg.cex=leg.cex,
              cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
      } # ELSE end      
   
  } # ELSE if (ftype=="ma") END
  
  else if (ftype=="dma") {
        
    if (sim.freq != "daily") {      
      stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")  
           
    } else {       
          # Generating Monthly time series 
          obs.monthly <- daily2monthly(obs, FUN, na.rm) # hydroTSM::daily2monthly
          sim.monthly <- daily2monthly(sim, FUN, na.rm) # hydroTSM::daily2monthly
          
          # Generating Annual time series 
          obs.annual <- daily2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d") # hydroTSM::daily2annual
          sim.annual <- daily2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d") # hydroTSM::daily2annual
          
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
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
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
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
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
                  gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
                  legend=legend, leg.cex=leg.cex,
                  cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
      } # ELSE end
            
  } else if (ftype=="seasonal") {

     gofs.all     <- c("ME", "RMSE", "PBIAS", "RSR", "NSE", "d", "R2", "KGE", "VE") 
     gofs.default <- c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE")
     if (all.equal(gofs, gofs.default)) gofs <- gofs.all

     # Checking 'gofs'       
     if (length(noNms <- gofs[!gofs %in% gofs.all])) 
       warning("[ftype=='seasonal': Unknown names in 'gofs': ", paste(noNms, collapse = ", "), " (not used) !]")

     gof.index <- pmatch(gofs, gofs.all)
     gof.index <- gof.index[!is.na(gof.index)]  
     gofs      <- gofs.all[gof.index] 
        
    if (sim.freq %in% c("quarterly", "yearly")) {      
      stop("Invalid argument: 'sim' has to have a 'sub-daily', 'daily' or 'monthly' ts. However, 'sim' is a '", sim.freq, "' ts !")  
           
    } else {       
          # Checking that the user provied a valid value for 'stype'   
          valid.types <- c("default", "FrenchPolynesia")    
          if (length(which(!is.na(match(stype, valid.types )))) <= 0)  
            stop("Invalid argument: 'stype' must be in c('default', 'FrenchPolynesia')")

          # Labels for the seasons
          if (stype=="default") { 
            seasons.lab <- c("DJF",  "MAM", "JJA", "SON")
          } else if (stype=="FrenchPolynesia") { 
              seasons.lab <- c("DJFM", "AM",  "JJAS", "ON")
            } # ELSE end

          # Computing the seasonal values
          sim.winter <- dm2seasonal(sim, season=seasons.lab[1], FUN=FUN, out.fmt="%Y-%m-%d")
          sim.spring <- dm2seasonal(sim, season=seasons.lab[2], FUN=FUN, out.fmt="%Y-%m-%d")
          sim.summer <- dm2seasonal(sim, season=seasons.lab[3], FUN=FUN, out.fmt="%Y-%m-%d")
          sim.autumm <- dm2seasonal(sim, season=seasons.lab[4], FUN=FUN, out.fmt="%Y-%m-%d")
          
          obs.winter <- dm2seasonal(obs, season=seasons.lab[1], FUN=FUN, out.fmt="%Y-%m-%d")
          obs.spring <- dm2seasonal(obs, season=seasons.lab[2], FUN=FUN, out.fmt="%Y-%m-%d")
          obs.summer <- dm2seasonal(obs, season=seasons.lab[3], FUN=FUN, out.fmt="%Y-%m-%d")
          obs.autumm <- dm2seasonal(obs, season=seasons.lab[4], FUN=FUN, out.fmt="%Y-%m-%d")

          # Transforming the seasonal values into xts objects
          sim.winter <- as.xts(sim.winter)
          sim.spring <- as.xts(sim.spring)
          sim.summer <- as.xts(sim.summer)
          sim.autumm <- as.xts(sim.autumm)
          
          obs.winter <- as.xts(obs.winter)
          obs.spring <- as.xts(obs.spring)
          obs.summer <- as.xts(obs.summer)
          obs.autumm <- as.xts(obs.autumm)
    
          def.par <- par(no.readonly = TRUE) # save default, for resetting... 
          on.exit(par(def.par)) 
          
          # If the user wants a legend, the screen is split into 2 rows and 2 columns, 
          # where the proportion of width of the 1st column to the 2nd one is 9:2
          if (gof.leg) {   
            # Setting up a screen with 4 rows and 2 columns 
            layout( matrix( c(1,1,1,1,1,1,1,1,1,2,2,3,3,3,3,3,3,3,3,3,4,4,5,5,5,5,5,5,5,5,5,6,6,7,7,7,7,7,7,7,7,7,8,8), ncol=11, byrow=TRUE) ) 
          } else {
             # Setting up the screen with 3 rows and 1 column
             par(mfrow=c(4,1))
            } #ELSE end  
          
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1) 
          # Drawing the 'winter' time series against time
          plot2(x=sim.winter, y=obs.winter, plot.type="single",
                main=paste(season.names[1], " (", seasons.lab[1], ")", sep=""),
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch,
                xlab= xlab, ylab= ylab, pt.style= "ts", 
                add= TRUE, 
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
                legend=legend, leg.cex=0.75, # leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
          
          # It is necessary to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)        
          # Drawing the 'spring' time series against time
          plot2(x=sim.spring, y=obs.spring, plot.type="single",  
                main=paste(season.names[2], " (", seasons.lab[2], ")", sep=""),
                tick.tstep=tick.tstep, lab.tstep= lab.tstep, lab.fmt=lab.fmt,
                cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                col = col, lwd= lwd, lty=lty, pch=pch, 
                xlab= xlab, ylab= ylab, pt.style= "ts", 
                add= TRUE, 
                gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
                legend=legend, leg.cex=0.75, # leg.cex=leg.cex,
                cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
           
          # It is necessary to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)        
          # Drawing the 'summer' time series against time
          plot2(x=sim.summer, y=obs.summer, plot.type="single",
                  main=paste(season.names[3], " (", seasons.lab[3], ")", sep=""),
                  tick.tstep="years", lab.tstep= "years", lab.fmt=lab.fmt,
                  cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                  col = col, lwd= lwd, lty=lty, pch=pch,
                  xlab= xlab, ylab= ylab, pt.style= pt.style, 
                  add= TRUE, 
                  gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
                  legend=legend, leg.cex=0.75, # leg.cex=leg.cex,
                  cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )

          # It is necessary to set up the margins again, after the previous call to plot2
          par(mar=c(5, 4, 4, 0) + 0.1) # mar=c(bottom, left, top, right). Default values are: mar=c(5,4,4,2) + 0.1)        
          # Drawing the 'autumm' time series against time
          plot2(x=sim.autumm, y=obs.autumm, plot.type="single",
                  main=paste(season.names[4], " (", seasons.lab[4], ")", sep=""),
                  tick.tstep="years", lab.tstep= "years", lab.fmt=lab.fmt,
                  cex = cex, cex.axis=cex.axis, cex.lab=cex.lab, 
                  col = col, lwd= lwd, lty=lty, pch=pch,
                  xlab= xlab, ylab= ylab, pt.style= pt.style, 
                  add= TRUE, 
                  gof.leg = gof.leg, gof.digits=digits, gofs=gofs,
                  legend=legend, leg.cex=0.75, # leg.cex=leg.cex,
                  cal.ini=cal.ini, val.ini=val.ini, date.fmt=date.fmt, ... )
      } # ELSE end
            
  } # ELSE if (ftype=="seasonal")
  
} # 'ggof' end
