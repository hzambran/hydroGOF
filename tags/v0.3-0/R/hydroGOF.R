######################################################################
#                'lib_GoodnessOfFit'                                 #
######################################################################
#  Library with several functions for assessing the goodness of fit  #
#  between observed and simulated values, the most commonly used in  #
#  Hydrological Modelling                                            #
#                                                                    #
#  Author      : Mauricio Zambrano Bigiarini                         #
#  Stared      : September 2008                                      #
#  Further dev.: Dec 2008, Jan, Feb, March, Sep, Oct 2009            # 
#  Version     : 0.1.0 : 07-Sep-2009                                 #
#  Version     : 0.1.1 : 06-Oct-2009                                 #
#  Version     : 0.1.2 : 29-Oct-2009                                 #
#  Version     : 0.1.3 : 01-Dec-2009                                 #
#  Version     : 0.2-0 : 07-Oct-2010                                 #
#  Version     : 0.2-1 : ongoing...                                  #
#  Last Update : 07-Jul-2010                                         #
######################################################################
# At the begining, only numerical vectors, matrix and data.frame were# 
# allowed as arguments, but on March 04th, 2009, the 'ts' and 'zoo'  #
# classes were added            

# Citations:
# 1) Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resour. Res., 36(12), 3663–3674.                                      #
# 2) Krause, P., Boyle, D. P., and Bäse, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005.
# 3) Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233–241. 
# 4) Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic quantification of accuracy in watershed simulations. Transactions of the ASABE. 50(3):885-900. 
# 5) Kitanidis, P. K., and R. L. Bras (1980), Real-Time Forecasting With a Conceptual Hydrologic Model 2. Applications and Results, Water Resour. Res., 16(6), 1034–1044. 
# 6) J.E. Nash and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282–290.
# 7) Yapo P. O., Gupta H. V., Sorooshian S., 1996. Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48


######################################################################## 
# External packages required:                                          #
# 1) 'zoo', by the 'ggof' function                                     #
# 2) 'hydroTSM', by the 'ggof' and 'plotbands'                         #
######################################################################## 

########################################################################
# Included functions:
# 01) .intersect    : elements that  belongs to 2 vectors
# 02) valindex : index of the elements that belongs to both vectors
# 03) ssq           : Sum of Squared Residuals
# 04) me            : Mean Error
# 05) mae           : Mean Absolute Error			
# 06) rmse          : Root Square Mean Error
# 07) nrmse         : Normalized Root Square Mean Error
# 08) rSD           : Ratio of Standard Deviations 
# 09) NSeff         : Nash-sutcliffe Efficiency
# 10) mNSeff        : Modified Nash-sutcliffe Efficiency (without the squares)
# 11) IoA           : Index of Agreement
# 12) PI            : Persistence Index
# 13) Pbias         : Percent Bias  
# 14) gof           : Several numerical performance indexes for comparing two vectors, matrix or data.frames
#                     It computes all the previous mentioned gof functions, 
#                     with the exception of: 'ssq', 'r.SD'
# 15) ggof          : Graphical performance comparison between two vectors (numeric, ts or zoo)   
# 16) plot2         : It plots 2 time series on the same graph. It is a wrapper for the 'plot.zoo' function  
# 17) br2           : weighted coef. of determination
# 18) mse           : Mean Squared Error 
# 19) rsr           : Ratio of RMSE to the Standard Deviation of the Observations   
# 20) plotbands     : It plots a ts of simulated values and two confidence bands, with optional plot of observations 
# 21) pfactor       : % of observations that are within the given uncertainty bounds
# 22) rfactor       : Average width of the given uncertainty bounds divided by the standard deviation of the observations   
# 23) pbiasfdc      : PBIAS in the slope of the midsegment of the Flow Duration Curve  
# 
########################################################################


####################################################
# 'intersect': elements that  belongs to 2 vectors #
####################################################
#     19-Jan-2009   #
#####################
# 'x'     : vector (numerical or character)
# 'y'     : vector (numerical or character)
# 'Result': intersection between 'x' and 'y', i.e., only those elements that are
#           present both in 'x' and 'y'.
.intersect <- function(x, y) { 
 
 return( y[match(x, y, nomatch = 0)] )

} # '.intersect' END

