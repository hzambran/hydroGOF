NEWS/ChangeLog for hydroGOF

--------------------------
# Changes in version 0.5-4  21-Jan-2024

## Bug fixes

        o CITATION file: citEntry was replaced by bibentry, after CRAN note in v0.5-3.
        
--------------------------
# Changes in version 0.5-3  21-Jan-2024

## Bug fixes
        o 'ssq'       : Tiny change. Space in documentation file removed after CRAN notes in v0.5-0.
        o 'wNSE'      : Tiny change. Space in documentation file removed after CRAN notes in v0.5-0.
        
--------------------------
# Changes in version 0.5-0  20-Jan-2024

## New functions

        o 'KGElf'    : to compute the Kling-Gupta efficiency focused on low flows proposed by García et al. (2017).
        o 'sKGE'     : to compute the Split Kling-Gupta efficiency, which was proposed by Fowler et al. (2018) for calibration in a drying climate.
        o 'KGEnp'    : to compute the non-parametric Kling-Gupta efficiency proposed by Pool et al. (2018).
        o 'dr'       : to compute the Refined Index of Agreement proposed by Willmott et al. (2012).
        o 'ubRMSE'   : to compute the unbiased Root mean Squared Error proposed by Entekhabi et al. (2010).
        o 'wNSE'     : to compute the weighted Nash-Sutcliffe efficiency proposed by Hundecha and Bardossy (2004) to put special focus on high values (Thanks to 'sluedtke' for the pull request !)
        o 'rSpearman': to compute the Spearman's rank correlation coefficient (previously it was part of the 'gof' function only).
        o 'R2'       : to compute the coefficient of determination (previously it was part of the 'gof' function only).

## New features    
        o 'KGE'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of KGE (e.g., log, sqrt, 1/x)   
        o 'mNSE'    : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of mNSE (e.g., log, sqrt, 1/x) 
        o 'rNSE'    : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of rNSE (e.g., log, sqrt, 1/x)                                                                                           
        o 'd'       : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of d (e.g., log, sqrt, 1/x)                                                                                         
        o 'rd'      : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of rd (e.g., log, sqrt, 1/x)  
        o 'VE'      : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of VE (e.g., log, sqrt, 1/x)  
        o 'cp'      : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of cp (e.g., log, sqrt, 1/x)  
        o 'mae'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of mae (e.g., log, sqrt, 1/x)  
        o 'me'      : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of me (e.g., log, sqrt, 1/x)
        o 'mse'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of mse (e.g., log, sqrt, 1/x) 
        o 'rmse'    : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of nrmse (e.g., log, sqrt, 1/x)     
        o 'nrmse'   : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of nrmse (e.g., log, sqrt, 1/x)     
        o 'pbias'   : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of pbias (e.g., log, sqrt, 1/x)  
        o 'pbiasfdc': three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of pbiasfdc (e.g., log, sqrt, 1/x)  
        o 'rSD'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of rSD (e.g., log, sqrt, 1/x)  
        o 'rsr'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of rsr (e.g., log, sqrt, 1/x)  
        o 'br2'     : three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of br2 (e.g., log, sqrt, 1/x)  
        o 'rPearson': three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of rPearson (e.g., log, sqrt, 1/x)  
        o 'br2'     : new argument 'dec' to allow the user to specify the number of decimals shown in the output object. Default value set to 1 for compatibility with previous versions. Thanks to Ken Newman !
        o 'gof'     : four new arguments (start.month, fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of all goodness-of-fit.                                                                                                                                                                      
        o 'NSE'     : possible values of 'epsilon' argument were changed to c('Pushpalatha2012', 'otherFactor', 'otherValue'), to give the user more flexibility in the computation of the constant value added to 'sim' and 'obs' when using transformed values to compute the NSE (e.g., when focusing on low flows)

## Bug fixes
        o 'br2'     : R2 is now correctly computed as 1 - SSres/SStot. Before it was computed as rPearson^2, which was only correct for linear models. Thanks to eugenioLR (https://github.com/hzambran/hydroGOF/issues/16#issue-1736556320)


# Changes in version 0.4-0  11-Mar-2020
        o Package tested against R Under development (unstable) (2020-03-10 r77920) -- "Unsuffered Consequences", following an imperative request made by CRAN.
        o Citation file changed, following CRAN comments.
        o Vignette on Goodness-of-fit Measures to Compare Observed and Simulated Values was moved from Sweave to Knitr.
        o New references were added for 'KGE' (Santos et. al, 2018; Knoben et al., 2019; Mizukami et al., 2019)
        o New reference was added for 'me' (Hill et al., 2006) Thanks to Erli Pinto dos Santos !.
        o 'br2'     : new argument 'use.abs=FALSE', to allow the user to use 'abs(b)' as condition to decide whether using abs(b)*r2 or [1/abs(b)]*r2 in equation (5) in Krausse et al. (2005). Thanks to Ellie White !


# Changes in version 0.3-9  07-Ago-2017
        o repository management moved from SVN to GIT, including rforge.
        o citation with DOI is now possible (and new CITATION file).
        o 'NSE'         : -) three new arguments (fun, epsilon, epsilon.value), added to allow pre-processing simulated and observed values before computation of NSE (e.g., log, sqrt, 1/x)                                              
        o 'KGE'         : -) now works correctly when 'out.type="full"', even if 'sim' and 'obs' do not have a single element in common (i.e., results in labelled NAs)
        o 'd'           : -) now it returns NA when it is not possible to compute the index of agreement (sum((abs(sim-Om)+abs(obs-Om))^2)=0)
        o 'ggof'        : -) legend and axis fonts are preserved when 'pt.style' is set to bar
                          -) fixed error raised for xts >= 0.10-0
        o 'plot2'       : -) legend and axis fonts are preserved when 'pt.style' is set to bar
                          -) fixed error raised for xts >= 0.10-0
        o 'plotbands    : -) fixed error raised for xts >= 0.10-0
        o 'rNSE'        : -) fixed typo in the formula presented in the documentation (thanks to James Ward!)
        o 'rPearson'    : -) now it is exported in the NASPECE and visible to the users.
        o 'md'          : -) correct small typo in documentation (thanks to Raphael Felber !)
        o 'rSD'         : -) missing values in 'obs' or 'sim' are now removed before the computations (thanks to Colin Hansen !)
        o 'nrmse'       : -) missing values in 'obs' or 'sim' are now removed before the computations (thanks to Colin Hansen !)
        o NAMESPACE file:
                          -) Ninety five (95) S3 methods are now registered.
                          -) Web page of the package updated to the new github repository.
                          -) CRAN website removed from URL field in DESCRIPTION file (requested by CRAN).
                          -) 'plot.xts' is no longer imported from 'xts' package (to avoid error with xts >= 0.10-0)
                          -) 'axTicksByTime' is now imported from 'xts'


# Changes in version  0.3-8	04-Feb-2014
        o 'nrmse'         : -) added formula representing the computation done when 'norm="sd"' (default). Thanks to Markus Eichhoff !.
        o 'KGE'           : -) fixed error raised when 's != c(1,1,1)'
        o 'pbiasfdc'      : -) now does not need to load 'hydroTSM' package before being used.
                               In previous versions, when this function was applied to numeric vectors, it raised the following error:
                               "Error in UseMethod("fdc") : no applicable method for 'fdc' applied to an object of class c('double', 'numeric')"
        o 'Author' field was removed from the DESCRIPTION file
        o Imports from the 'zoo' package were made explicit in the NAMESPACE file (due related changes in hydroTSM)
        o 'WhatsNew.txt' file was renamed to 'ChangeLog'
        o 'require(zoo)' (before loading the 'EgaEnEstellaQts' dataset) was removed from all the examples
 

# Changes in version  0.3-7	03-Jun-2013
        o 'valindex'     : false warning fixed. 
                           This warning raised the message "'sim' and 'obs' are empty or they do not have any common pair of elements with data !!",
                           even when 'sim' and 'obs' were fully compatible.
        o 'ggof'         : -) new argument 'gofs', which allows to select which GoFs are shown when 'gof.leg=TRUE'
                           -) new argument 'stype'
                           -) new argument 'season.names'
                           -) 'ftype' argument now allows 'seasonal' value as input
                           -) internal: 'hydroTSM::daily2monthly' was replaced by 'daily2monthly' 
                           -) internal: 'hydroTSM::daily2annual' was replaced by 'daily2annual' 
                           -) internal: 'hydroTSM::vector2zoo' was replaced by 'vector2zoo' 
        o 'pbiasfdc'     : -) internal: 'hydroTSM::fdc' was replaced by 'fdc' 
        o 'plotbandsonly': -) internal: 'hydroTSM::vector2zoo' was replaced by 'vector2zoo' 
        o 'plot2'        : -) new argument 'gofs', which allows to select which GoFs are shown when 'gof.leg=TRUE'
                           -) internal: 'hydroTSM::vector2zoo' was replaced by 'vector2zoo' 
                           -) internal: 'hydroTSM::drawTimeAxis' was replaced by 'drawTimeAxis' 
        o 'plotbands'    : -) internal: 'hydroTSM::vector2zoo' was replaced by 'vector2zoo' 
                           -) internal: 'hydroTSM::drawTimeAxis' was replaced by 'drawTimeAxis' 


# Changes in version  0.3-6	22-Mar-2013
        o 'gof'         : -) now it works with matrices/data.frames of zoo/xts class. Thanks to L. Pagliero !
                          -) new arguments 's' and 'method' which are passed to the 'KGE' function ('out.type' is fixed to "single")
                          -) new argument 'j', which is passed to the 'mNSE' function
                          -) new argument 'norm', which is passed to the 'nrmse' function                          
                          -) new arguments 'lQ.thr' and 'hQ.thr', which are passed to the (optional) 'pbiasfdc' function       
        o 'NSE'         : -) now it works with matrices/data.frames of zoo class.
        o 'me'          : -) now it works with matrices/data.frames of zoo class.
        o 'mae'         : -) now it works with matrices/data.frames of zoo class.
        o 'mse'         : -) now it works with matrices/data.frames of zoo class.
        o 'rmse'        : -) now it works with matrices/data.frames of zoo class.     
        o 'rsr'         : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'rSD'         : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'pbias'       : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'mNSE'        : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'rNSE'        : -) now it works with matrices/data.frames of zoo class.                                                                                                                    
        o 'md'          : -) now it works with matrices/data.frames of zoo class.                                                                 
        o 'cp'          : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'rPearson'    : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'br2'         : -) now it works with matrices/data.frames of zoo class.                                                          
        o 'KGE'         : -) now it works with matrices/data.frames of zoo class.      
        o 'VE'          : -) now it works with matrices/data.frames of zoo class.  
        o 'd'           : -) now it works with matrices/data.frames of zoo class.         
                          -) fixed typo in the formula presented in the pdf manual (a square was missing). Thanks to T. Ladson and  Insa Thiele-Eich !
        o 'rd'          : -) now it works with matrices/data.frames of zoo class. 
                          -) fixed typo in the formula presented in the pdf manual (a square was missing). Thanks to Insa Thiele-Eich !                                                   
        o 'nrmse'       : -) now it works with matrices/data.frames of zoo class.                          
        o 'nrmse.matrix': fixed error in checking the 'norm' argument.
        o 'valindex'    : a warning was added when 'sim' and 'obs' do not have any pair of elements with data in both variables.
        o e-mail address of maintainer was changed from the @ing.unit.it to the @gmail.com domain
	

# Changes in version  0.3-5	19-Oct-2012
        o now it depends on 'hydroTSM' >= 0.3-6, in order to allow handling sub-daily values in 'ggof'
        o 'ggof'      : -) now it handles sub-daily values in 'sim' and 'obs' (aggregation to daily/monthly/annual values not yet available)
                        -) 'dates' argument accepts 'POSIXct' objects
        o 'KGE'       : -) two new arguments: 'method', to allow KGE-2009 and KGE-2012 formulas, and 'out.type', in order to allow look at the
                           individual elements of the KGE (correlation, bias, and variability)
                        -) Added reference to Kling et al. 2012 and improved documentation
                        -) Added example to show difference between KGE-2009 and KGE-2012
                        -) now it returns NA instead of an error when there are no pairs of 'sim' and 'obs' without missing values 
                           (thanks to Giovanni Forzieri !)
        o DESCRIPTION : 'Author@R' field was replaced by 'Authors@R'                        
        

# Changes in version  0.3-4	03-Jul-2012
        o new function 'VE', which computes the volumetric efficiency proposed by Criss and Winston 2008
         (thanks to Eric Morway for giving the final push to include it !)
        o 'gof'   : VE is now included in the computed goodness-of-fit measures
        o 'ggof'  : VE is now included in the computed goodness-of-fit measures
        o 'plot2' : VE is now included in the computed goodness-of-fit measures (when 'leg.gof=TRUE')
        o function '.intersect' was removed and function 'valindex' was improved in order to replace it
        o 'zoo' package was moved from 'Imports' to 'Depends'
        o 'methods' package is now imported, in order to avoid problems with Rscript, which currently does not load 'methods'
        

# Changes in version  0.3-3	02-May-2012
        o 'gof'   : added new example and description of the output class (thanks to Kris Jaeger !)
        o 'br2'   : the new example included in version 0.3-2 was improved
        o 'ggof'  : fixed problem with 'ylim' when some elements in 'sim' or 'obs' were NA (the issue was in the 'plot2' function)
        o 'plot2' : fixed problem with 'ylim' when some elements in 'x' or 'y' were NA
	            

# Changes in version  0.3-2	07-Oct-2011
        o 'br2'   : fixed documentation error in its formula (thanks to Jan Corluy !), and a new example was added.
                    Its value was already computed in the correct way.
	

# Changes in version  0.3-1	14-Sep-2011
        o 'ggof'  : fixed problem when 'ylim' is specified by the user (the issue was in the 'plot2' function)
        o 'plot2' : fixed problem when 'ylim' is specified by the user


# Changes in version  0.3-0	01-Sep-2011
        o NAMESPACE created, which leads to reduced dependency on the package hydroTSM.
        o hydroGOF now depends on xts >= 0.8-2 (=> zoo >= 1.7-2), in order to make use of improved plotting capabilities for ts objects.
          The first release of 'xts' after the 0.8-0 (which was set as the default building block for plotting time series in hydroTSM) 
          was the 0.8-2, and appeared on CRAN on August 9th, 2011       
        o now it depends on 'hydroTSM' >= 0.3-0, in order to use the re-written 'drawxaxis/drawTimeAxis' function.	
        o a vignette is now included with the package, in order to illustrate some basic functionality of hydroGOF
        o 'plot2'        : -) better display of time series objects, when 'plot.type' is 'single'.
		                      The (nicer) 'plot.xts' function is used instead of the previous 'plot.zoo'.
		                   -) 'tick.tstep' and 'lab.tstep' now accept the values: "auto", "weeks", "hours", "minutes", 
		                       "seconds", which should work with sub-daily time series
		                   -) when 'cal.ini' is provided, all the values in 'obs' and 'sim' with dates previous to 'cal.ini' are SKIPPED 
		                      from the computation of the goodness-of-fit measures (when 'gof.leg=TRUE'), but their values are still plotted, 
		                      in order to examine if the warming up period was too short, acceptable or too long for the chosen calibration
		                      period.
		                   -) fixed problem with the 'cex.axis' argument when 'plot.type="multiple"' (this was a zoo problem, and it was 
		                      fixed in zoo_1.7-1).		           
	o 'ggof'         :     -) better display of time series objects (by using the new 'plot2' and 'drawxaxis/drawTimeAxis')
		                   -) it works with xts objects as well.
		                   -) 'tick.tstep' and 'lab.tstep' now accept the values: "auto", "quarters", "weeks", "hours", 
	 	                      "minutes", and "seconds", and its default value was changed to "auto". 
	 	                   -) the name of 'sim' and 'obs' are now used as default for the legend.
	 	                   -) when 'cal.ini' is provided, all the values in 'obs' and 'sim' with dates previous to 'cal.ini' are SKIPPED 
		                      from the computation of the goodness-of-fit measures (when 'gof.leg=TRUE'), but their values are still plotted, 
		                      in order to examine if the warming up period was too short, acceptable or too long for the chosen calibration
		                      period.
		                   -) added error checking for 'ftype' when 'sim' and 'obs' are not time series.
	o 'plotbands'    :     -) better display of time series objects. The (nicer) 'plot.xts' function is used 
			                  instead of the previous 'plot.zoo', and it uses the new 'drawTimeAxis' function.
		                   -) 'tick.tstep' and 'lab.tstep' now accept the values: "auto", "quarters", "weeks", "hours", 
	 		                  "minutes", and "seconds", and its default value was changed to "auto". 
	o 'plotbandsonly':     -) it works with xts objects as well
			               -) 'legend' argument was removed (it was not used)
			               -) 'leg.cex' argument was removed (it was not used)
	o 'NSeff'        :     -) its name was changed to 'NSE'. The old name 'NSeff' is deprecated but is still working for backwards compatibility.
	                       -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'rNSeff'       :     -) its name was changed to 'rNSE'. The old name 'rNSeff' is deprecated but is still working for backwards compatibility.
	                       -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'mNSeff'       :     -) its name was changed to 'mNSE'. The old name 'mNSeff' is deprecated but is still working for backwards compatibility.
	                       -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'rd'           :     -) a warning message is created when -at least one of- the observed values is/are zero,
	                          which makes impossible to compute this index (thanks to Tom Purucker !)
	                       -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'rNSE'         :     -) a warning message is created when -at least one of- the observed values is/are zero,
	                          which makes impossible to compute this index
	                       -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'md'           :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'pbias'        :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'pbiasfdc'     :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'pfactor'      :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'rfactor'      :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'rSD'          :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'RSR'          :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'NRMSE'        :     -) now it returns NA instead of an error when the denominator of its formula is equal to zero.
	o 'KGE'          :     -) now it returns NA instead of '-Inf' when the denominator of Alpha or Beta were equal to zero.	
	o 'rd'           :     -) fixed documentation error in its formula (thanks to Tom Purucker !). Its value was already computed in the correct way.
	o New dimension checking (number of columns and rows) for all the functions that are applied over a matrix or data.frame




-- Previous Releases: see old file 'ChangeLog' (see old file 'WhatsNew.txt)'

# version 0.2-2. Release Date: 14-Apr-2011
# version 0.2-1. Release Date: 30-Nov-2010
# version 0.2-0. Release Date: 07-Oct-2010
# version 0.1.3. Release Date: 01-Dec-2009
# version 0.1.2. Release Date: 29-Oct-2009
# version 0.1.1. Release Date: 05-Oct-2009
# version 0.1.0. Release Date: 07-Sep-2009 (but the functions have been developed since 2008)
