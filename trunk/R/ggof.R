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
#            17-Apr-2011                # 
#########################################     
                                          
      
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

  # Checking that the user provied a valid argument for 'sim' &'obs'   
  valid.class <- c("xts", "zoo", "numeric", "integer")    
  if (length(which(!is.na(match(class(sim), valid.class )))) <= 0)  
         stop("Invalid argument: 'class(sim)' must be in c('xts', 'zoo', 'numeric', 'integer')")
  if (length(which(!is.na(match(class(obs), valid.class )))) <= 0)
         stop("Invalid argument: 'class(obs)' must be in c('xts', 'zoo', 'numeric', 'integer')")
         
  # Checking that the user provied the same length for 'sim' and 'obs'      
  if ( length(sim) != length(obs) )  
     stop(paste("Invalid argument: 'obs' and 'sim' must have the same length ! (", 
                length(obs), "vs", length(sim), ")"  ,sep=" ") )
                
  # 'xname' and 'yname' values
  sim.name <- deparse(substitute(sim))
  obs.name <- deparse(substitute(obs))

  # 'legend' value
  if (missing(legend)) legend <- c(sim.name, obs.name)
                   
  # Checking that the user provied the same sampling frequency for 'sim' and 'obs',
  # when 'sim' and 'obs' are 'zoo' objects      
  if ( zoo::is.zoo(obs) & zoo::is.zoo(sim)) {
      if ( hydroTSM::sfreq(sim) != hydroTSM::sfreq(obs) ) {
         stop(paste("Invalid argument: 'obs' and 'sim' have different sampling frequency ! (", 
                   hydroTSM::sfreq(obs), "vs", hydroTSM::sfreq(sim), ")"  ,sep=" ") ) }
  } # IF end
         
  # Checking 'ftype'       
  if (is.na(match(ftype, c("o", "dm", "ma", "dma") ) ) ) 
         stop("Invalid argument: 'ftype' must be in c('o', 'dm', 'ma, 'dma')")
         
  # Checking FUN, when 'ftype' involves monthly or annual values     
  if (!is.na(match(ftype, c("dm", "ma", "dma") ) ) & missing(FUN) ) 
         stop("Missing argument: 'FUN' must be provided when 'ftype' is in c('dm', 'ma, 'dma')")
         
  # If the user didn't provide a title for the plot, the default is used 
  if ( missing(main) ) main <- "Observations vs Simulations"         
          
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
    if ( zoo::is.zoo(obs) ) { zoo::time(obs) <- dates }  
    # If 'sim' is 'zoo' and the user provides the dates  (probably new dates)
    if ( zoo::is.zoo(sim) ) { zoo::time(sim) <- dates }  
    
  } else if (!zoo::is.zoo(obs)) message("[Note: You didn't provide dates, so only a numeric index will be used in the time axis.]")      
 
  
  #require(hydroTSM) # for using the 'vector2zoo' function 
  
  # If 'class(obs)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !zoo::is.zoo(obs) & !missing(dates) ) { 
    obs <- hydroTSM::vector2zoo(x=obs, dates=dates, date.fmt=date.fmt)        
  } # If 'class(obs)' is 'zoo' and 'dates' are missing, dates are extracted from 'obs'
    else if ( zoo::is.zoo(obs) & missing(dates) ) {  
      # class(zoo::time(x))== "Date" for 'daily' and 'monthly' time series
      # class(zoo::time(x))== "character" for 'annual' time series
      if ( class(zoo::time(obs)) == "Date" ) { dates <- zoo::time(obs) 
      } else if ( class(zoo::time(obs)) == "character" ) {  
             dates <- as.Date(time(obs), format="%Y") }      
    } #ELSE END
  
  # If 'class(sim)' is not 'zoo' and the user provides the dates, then we turn it into a zoo class
  if ( !zoo::is.zoo(sim) & !missing(dates) ) { 
    sim <- hydroTSM::vector2zoo(x=sim, dates=dates, date.fmt=date.fmt) 
  # If 'class(sim)' is 'zoo' and 'dates' are missing, dates are extracted from 'sim'
  } else if ( zoo::is.zoo(sim) & zoo::is.zoo(obs) & missing(dates) ) {
      # class(zoo::time(x))== "Date" for 'daily' and 'monthly' time series
      # class(zoo::time(x))== "character" for 'annual' time series
      if ( class(zoo::time(sim)) == "Date" ) { dates <- zoo::time(obs) 
      } else if ( class(zoo::time(sim)) == "character" ) {  
             dates <- as.Date(zoo::time(sim), format="%Y") }
    } #ELSE END    
  
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
    
      if (hydroTSM::sfreq(sim) != "daily") {      
        stop("Invalid argument: 'sim' has to have a 'daily' sampling frequency")       
      } else {
          # Generating a Monthly time series (Monthly mean of daily values):
          obs.monthly <- hydroTSM::daily2monthly(obs, FUN, na.rm)
          sim.monthly <- hydroTSM::daily2monthly(sim, FUN, na.rm)
          
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
  
    if  ( is.na( match( hydroTSM::sfreq(sim), c("daily", "monthly") ) ) ) {      
      stop("Invalid argument: the sampling frequency of 'sim' has to be in c('daily', 'monthly'")       
    } else {
        if ( hydroTSM::sfreq(sim) == "daily" ) {
           # Generating a Monthly time series (Monthly mean of daily values):
           obs <- hydroTSM::daily2monthly(obs, FUN, na.rm)
           sim <- hydroTSM::daily2monthly(sim, FUN, na.rm)
        } # IF end
        
        # Generating Annual time series (Annual mean of daily values)
        obs.annual <- hydroTSM::monthly2annual(obs, FUN, na.rm, out.fmt="%Y-%m-%d")
        sim.annual <- hydroTSM::monthly2annual(sim, FUN, na.rm, out.fmt="%Y-%m-%d")
        
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
        
    if (hydroTSM::sfreq(sim) != "daily") {      
      stop("Invalid argument: the 'sim' has to have a 'Daily' sampling frequency")  
           
    } else {       
          # Generating Monthly time series (Monthly mean of daily values):
          obs.monthly <- hydroTSM::daily2monthly(obs, FUN, na.rm)
          sim.monthly <- hydroTSM::daily2monthly(sim, FUN, na.rm)
          
          # Generating Annual time series (Annual mean of daily values)
          obs.annual <- hydroTSM::daily2annual(obs, FUN, na.rm, out.fmt = "%Y-%m-%d")
          sim.annual <- hydroTSM::daily2annual(sim, FUN, na.rm, out.fmt = "%Y-%m-%d")
          
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
