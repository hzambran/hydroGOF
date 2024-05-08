%% File ggof.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2011-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{ggof}
\Rdversion{1.1}
\alias{ggof}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical Goodness of Fit
}
\description{
Graphical comparison between two vectors (numeric, ts or zoo), with several numerical goodness of fit printed as a legend. \cr
Missing values in observed and/or simulated values can removed before the computations. \cr
}
\usage{
ggof(sim, obs, na.rm = TRUE, dates, date.fmt = "\%Y-\%m-\%d", 
     pt.style = "ts", ftype = "o",  FUN, 
     stype="default", season.names=c("Winter", "Spring", "Summer", "Autumn"),
     gof.leg = TRUE,  digits=2, 
     gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", 
             "rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE"),
     legend, leg.cex=1,
     tick.tstep = "auto", lab.tstep = "auto", lab.fmt=NULL,
     cal.ini=NA, val.ini=NA,
     main, xlab = "Time", ylab=c("Q, [m3/s]"),  
     col = c("blue", "black"), 
     cex = c(0.5, 0.5), cex.axis=1.2, cex.lab=1.2,
     lwd = c(1, 1), lty = c(1, 3), pch = c(1, 9), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{sim}{
numeric or zoo object with with simulated values
}
  \item{obs}{
numeric or zoo object with observed values
}
  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{dates}{
character, factor, Date or POSIXct object indicating how to obtain the dates for the corresponding values in the \code{sim} and \code{obs} time series \cr
If \code{dates} is a character or factor, it is converted into Date/POSIXct class, using the date format specified by \code{date.fmt}
}
  \item{date.fmt}{
OPTIONAL. character indicating the format in which the dates are stored in \code{dates}, \code{cal.ini} and \code{val.ini}. See \code{format} in \code{\link[base]{as.Date}}. Default value is \kbd{\%Y-\%m-\%d} \cr
ONLY required when \code{class(dates)=="character"} or \code{class(dates)=="factor"} or when \code{cal.ini} and/or \code{val.ini} is provided.
}
  \item{pt.style}{
Character indicating if the 2 ts have to be plotted as lines or bars. When \code{ftype} is NOT \kbd{o}, it only applies to the annual values. Valid values are: \cr
-) \kbd{ts} : (default) each ts is plotted as a lines along the 'x' axis \cr
-) \kbd{bar}: both series are plotted as barplots. 
}
  \item{ftype}{
Character indicating how many plots are desired by the user. Valid values are: \cr
-) \kbd{o}       : only the original \code{sim} and \code{obs} time series are plotted \cr
-) \kbd{dm}      : it assumes that \code{sim} and \code{obs} are daily time series and Daily and Monthly values are plotted  \cr
-) \kbd{ma}      : it assumes that \code{sim} and \code{obs} are daily or monthly time series and Monthly and Annual values are plotted \cr
-) \kbd{dma}     : it assumes that \code{sim} and \code{obs} are daily time series and Daily, Monthly and Annual values are plotted \cr
-) \kbd{seasonal}: seasonal values are plotted. See \code{stype} and \code{season.names}
}
  \item{FUN}{
OPTIONAL, ONLY required when \code{ftype} is in \code{c('dm', 'ma', 'dma', 'seasonal')}. Function that have to be applied for transforming teh original ts into monthly, annual or seasonal time step (e.g., for precipitation FUN MUST be \code{sum}, for temperature and flow time series, FUN MUST be \code{mean})
}
  \item{stype}{
 OPTIONAL, only used when \code{ftype=seasonal}. \cr
character, indicating whath weather seasons will be used for computing the output. Possible values are: \cr
-) \kbd{default}          =>  "winter"= DJF  = Dec, Jan, Feb;      "spring"= MAM = Mar, Apr, May;  "summer"= JJA  = Jun, Jul, Aug;       "autumn"= SON = Sep, Oct, Nov  \cr
-) \kbd{FrenchPolynesia}  =>  "winter"= DJFM = Dec, Jan, Feb, Mar; "spring"= AM  = Apr, May;       "summer"= JJAS = Jun, Jul, Aug, Sep;  "autumn"= ON  = Oct, Nov 
}
  \item{season.names}{
 OPTIONAL, only used when \code{ftype=seasonal}. \cr
character of length 4 indicating the names of each one of the weather seasons defined by \code{stype}.These names are only used for plotting purposes
}
  \item{gof.leg}{
logical, indicating if several numerical goodness of fit have to be computed between \code{sim} and \code{obs}, and plotted as a legend on the graph. If \code{leg.gof=TRUE}, then \code{x} is considered as observed and \code{y} as simulated values (for some gof functions this is important).
}
  \item{digits}{
OPTIONAL, only used when \code{leg.gof=TRUE}. Numeric, representing the decimal places used for rounding the goodness-of-fit indexes.
}
  \item{gofs}{
character, with one or more strings indicating the goodness-of-fit measures to be shown in the legend of the plot when  \code{gof.leg=TRUE}. \cr
Possible values when \code{ftype!='seasonal'} are in \code{c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")} \cr
Possible values when \code{ftype='seasonal'} are in c("ME", "RMSE", "PBIAS", "RSR", "NSE", "d", "R2", "KGE", "VE")
}
  \item{legend}{
character of length 2 to appear in the legend.  
}
  \item{leg.cex}{
OPTIONAL. ONLY used when \code{leg.gof=TRUE}. Character expansion factor for drawing the legend, *relative*  to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex'. Default value = 1
}
  \item{tick.tstep}{
character, indicating the time step that have to be used for putting the ticks on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
 \item{lab.tstep}{
character, indicating the time step that have to be used for putting the labels on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
  \item{lab.fmt}{
Character indicating the format to be used for the label of the axis. See \code{lab.fmt} in \code{\link[hydroTSM]{drawTimeAxis}}.
}
  \item{cal.ini}{
OPTIONAL. Character, indicating the date in which the calibration period started. \cr
When \code{cal.ini} is provided, all the values in \code{obs} and \code{sim} with dates previous to \code{cal.ini} are SKIPPED from the computation of the goodness-of-fit measures (when \code{gof.leg=TRUE}), but their values are still plotted, in order to examine if the warming up period was too short, acceptable or too long for the chosen calibration period.
In addition, a vertical red line in drawn at this date. 
}
  \item{val.ini}{
OPTIONAL. Character, the date in which the validation period started. \cr
ONLY used for drawing a vertical red line at this date. 
}
   \item{main}{
character representing the main title of the plot.
}
  \item{xlab}{
label for the 'x' axis.
}
  \item{ylab}{
label for the 'y' axis.
}
  \item{col}{
character, representing the colors of \code{sim} and \code{obs}
}
  \item{cex}{
numeric, representing the values controlling the size of text and symbols of 'x' and 'y' with respect to the default
}
  \item{cex.axis}{
numeric, representing the magnification to be used for the axis annotation relative to 'cex'. See \code{\link[graphics]{par}}.
}
  \item{cex.lab}{
numeric, representing the magnification to be used for x and y labels relative to the current setting of 'cex'. See \code{\link[graphics]{par}}.
}
  \item{lwd}{
vector with the line width of \code{sim} and \code{obs}
}
  \item{lty}{
numeric with the line type of \code{sim} and \code{obs}
}
  \item{pch}{
numeric with the type of symbol for \code{x} and \code{y}. (e.g., 1: white circle; 9: white rhombus with a cross inside)
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
Plots observed and simulated values in the same graph.

If \code{gof.leg=TRUE}, it computes the numerical values of: \cr
'me', 'mae', 'rmse', 'nrmse', 'PBIAS',  'RSR, 'rSD', 'NSE', 'mNSE', 'rNSE', 'd', 'md, 'rd', 'cp', 'r', 'r.Spearman', 'R2', 'bR2', 'KGE', 'VE'
}
\value{
The output of the \code{gof} function is a matrix with one column only, and the following rows: 

\item{ME}{Mean Error}
\item{MAE}{Mean Absolute Error}
\item{MSE}{Mean Squared Error}
\item{RMSE}{Root Mean Square Error}
\item{ubRMSE}{Unbiased Root Mean Square Error}
\item{NRMSE}{Normalized Root Mean Square Error  ( -100\% <= NRMSE <= 100\% )}
\item{PBIAS}{Percent Bias ( -Inf <= PBIAS <= Inf [\%] )}
\item{RSR}{Ratio of RMSE to the Standard Deviation of the Observations, RSR = rms / sd(obs). ( 0 <= RSR <= +Inf )}
\item{rSD}{Ratio of Standard Deviations, rSD = sd(sim) / sd(obs)}
\item{NSE}{Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )}
\item{mNSE}{Modified Nash-Sutcliffe Efficiency ( -Inf <= mNSE <= 1 )}
\item{rNSE}{Relative Nash-Sutcliffe Efficiency ( -Inf <= rNSE <= 1 )}
\item{wNSE}{Weighted Nash-Sutcliffe Efficiency ( -Inf <= wNSE <= 1 )}
\item{wsNSE}{Weighted Seasonal Nash-Sutcliffe Efficiency ( -Inf <= wsNSE <= 1 )}
\item{d}{Index of Agreement ( 0 <= d <= 1 )}
\item{dr}{Refined Index of Agreement ( -1 <= dr <= 1 )}
\item{md}{Modified Index of Agreement ( 0 <= md <= 1 )}
\item{rd}{Relative Index of Agreement ( 0 <= rd <= 1 )}
\item{cp}{Persistence Index ( 0 <= cp <= 1 ) }
\item{r}{Pearson Correlation coefficient ( -1 <= r <= 1 )}
\item{R2}{Coefficient of Determination ( 0 <= R2 <= 1 )}
\item{bR2}{R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} \cr ( 0 <= bR2 <= 1 )}
\item{VE}{Volumetric efficiency between \code{sim} and \code{obs} \cr ( -Inf <= VE <= 1)}
\item{KGE}{Kling-Gupta efficiency between \code{sim} and \code{obs} \cr ( -Inf <= KGE <= 1 )}
\item{KGElf}{Kling-Gupta Efficiency for low values between \code{sim} and \code{obs} \cr ( -Inf <= KGElf <= 1 )}
\item{KGEnp}{Non-parametric version of the Kling-Gupta Efficiency between \code{sim} and \code{obs} \cr ( -Inf <= KGEnp <= 1 )}
\item{KGEkm}{Knowable Moments Kling-Gupta Efficiency between \code{sim} and \code{obs} \cr ( -Inf <= KGEnp <= 1 )}

The following outputs are only produced when both \code{sim} and \code{obs} are zoo objects: 

\item{sKGE}{Split Kling-Gupta Efficiency between \code{sim} and \code{obs} \cr ( -Inf <= sKGE <= 1 ). Only computed when both \code{sim} and \code{obs} are zoo objects}
\item{APFB}{Annual Peak Flow Bias ( 0 <= APFB <= Inf )}
\item{HBF}{High Flow Bias ( 0 <= HFB <= Inf )}
\item{r.Spearman}{Spearman Correlation coefficient ( -1 <= r.Spearman <= 1 ). Only computed when \code{do.spearman=TRUE}}
\item{pbiasfdc}{PBIAS in the slope of the midsegment of the Flow Duration Curve}

%% ...
}
\references{
\cite{Abbaspour, K.C.; Faramarzi, M.; Ghasemi, S.S.; Yang, H. (2009), Assessing the impact of climate change on water resources in Iran, Water Resources Research, 45(10), W10,434, doi:10.1029/2008WR007615.}

\cite{Abbaspour, K.C., Yang, J. ; Maximov, I.; Siber, R.; Bogner, K.; Mieleitner, J. ; Zobrist, J.; Srinivasan, R. (2007), Modelling hydrology and water quality in the pre-alpine/alpine Thur watershed using SWAT, Journal of Hydrology, 333(2-4), 413-430, doi:10.1016/j.jhydrol.2006.09.014.}

\cite{Box, G.E. (1966). Use and abuse of regression. Technometrics, 8(4), 625-629. doi:10.1080/00401706.1966.10490407.}

\cite{Barrett, J.P. (1974). The coefficient of determination-some limitations. The American Statistician, 28(1), 19-20. doi:10.1080/00031305.1974.10479056.}

\cite{Chai, T.; Draxler, R.R. (2014). Root mean square error (RMSE) or mean absolute error (MAE)? - Arguments against avoiding RMSE in the literature, Geoscientific Model Development, 7, 1247-1250. doi:10.5194/gmd-7-1247-2014.}

\cite{Cinkus, G.; Mazzilli, N.; Jourde, H.; Wunsch, A.; Liesch, T.; Ravbar, N.; Chen, Z.; and Goldscheider, N. (2023). When best is the enemy of good - critical evaluation of performance criteria in hydrological models. Hydrology and Earth System Sciences 27, 2397-2411, doi:10.5194/hess-27-2397-2023.}

\cite{Criss, R. E.; Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi:10.1002/hyp.7072.}

\cite{Entekhabi, D.; Reichle, R.H.; Koster, R.D.; Crow, W.T. (2010). Performance metrics for soil moisture retrievals and application requirements. Journal of Hydrometeorology, 11(3), 832-840. doi: 10.1175/2010JHM1223.1.}

\cite{Fowler, K.; Coxon, G.; Freer, J.; Peel, M.; Wagener, T.; Western, A.; Woods, R.; Zhang, L. (2018). Simulating runoff under changing climatic conditions: A framework for model improvement. Water Resources Research, 54(12), 812-9832. doi:10.1029/2018WR023989.}

\cite{Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. Hydrological sciences journal, 62(7), 1149-1166. doi:10.1080/02626667.2017.1308511.}

\cite{Garrick, M.; Cunnane, C.; Nash, J.E. (1978). A criterion of efficiency for rainfall-runoff models. Journal of Hydrology 36, 375-381. doi:10.1016/0022-1694(78)90155-5.}

\cite{Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.}

\cite{Gupta, H.V.; Kling, H. (2011). On typical range, sensitivity, and normalization of Mean Squared Error and Nash-Sutcliffe Efficiency type metrics. Water Resources Research, 47(10). doi:10.1029/2011WR010962.}

\cite{Hahn, G.J. (1973). The coefficient of determination exposed. Chemtech, 3(10), 609-612. Aailable online at: \url{https://www2.hawaii.edu/~cbaajwe/Ph.D.Seminar/Hahn1973.pdf}.}

\cite{Hodson, T.O. (2022). Root-mean-square error (RMSE) or mean absolute error (MAE): when to use them or not, Geoscientific Model Development, 15, 5481-5487, doi:10.5194/gmd-15-5481-2022.}

\cite{Hundecha, Y., Bardossy, A. (2004). Modeling of the effect of land use changes on the runoff generation of a river basin through parameter regionalization of a watershed model. Journal of hydrology, 292(1-4), 281-295. doi:10.1016/j.jhydrol.2004.01.002.}

\cite{Kitanidis, P.K.; Bras, R.L. (1980). Real-time forecasting with a conceptual hydrologic model. 2. Applications and results. Water Resources Research, Vol. 16, No. 6, pp. 1034:1044. doi:10.1029/WR016i006p01034.} \cr

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019.}

\cite{Krause, P.; Boyle, D.P.; Base, F. (2005). Comparison of different efficiency criteria for hydrological model assessment, Advances in Geosciences, 5, 89-97. doi:10.5194/adgeo-5-89-2005.} \cr

\cite{Krstic, G.; Krstic, N.S.; Zambrano-Bigiarini, M. (2016). The br2-weighting Method for Estimating the Effects of Air Pollution on Population Health. Journal of Modern Applied Statistical Methods, 15(2), 42. doi:10.22237/jmasm/1478004000}

\cite{Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241. doi:10.1029/1998WR900018.} 

\cite{Ling, X.; Huang, Y.; Guo, W.; Wang, Y.; Chen, C.; Qiu, B.; Ge, J.; Qin, K.; Xue, Y.; Peng, J. (2021). Comprehensive evaluation of satellite-based and reanalysis soil moisture products using in situ observations over China. Hydrology and Earth System Sciences, 25(7), 4209-4229. doi:10.5194/hess-25-4209-2021.}

\cite{Mizukami, N.; Rakovec, O.; Newman, A.J.; Clark, M.P.; Wood, A.W.; Gupta, H.V.; Kumar, R.: (2019). On the choice of calibration metrics for "high-flow" estimation using hydrologic models, Hydrology Earth System Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019.}

\cite{Moriasi, D.N.; Arnold, J.G.; van Liew, M.W.; Bingner, R.L.; Harmel, R.D.; Veith, T.L. (2007). Model evaluation guidelines for systematic quantification of accuracy in watershed simulations. Transactions of the ASABE. 50(3):885-900}

\cite{Nash, J.E. and Sutcliffe, J.V. (1970). River flow forecasting through conceptual models. Part 1: a discussion of principles, Journal of Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6.}

\cite{Pearson, K. (1920). Notes on the history of correlation. Biometrika, 13(1), 25-45. doi:10.2307/2331722.}

\cite{Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature metrics for an improved overall performance evaluation of hydrological models. Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044.} 

\cite{Pizarro, A.; Jorquera, J. (2024). Advancing objective functions in hydrological modelling: Integrating knowable moments for improved simulation accuracy. Journal of Hydrology, 634, 131071. doi:10.1016/j.jhydrol.2024.131071.}

\cite{Pool, S.; Vis, M.; Seibert, J. (2018). Evaluating model performance: towards a non-parametric variant of the Kling-Gupta efficiency. Hydrological Sciences Journal, 63(13-14), pp.1941-1953. doi:/10.1080/02626667.2018.1552002.}

\cite{Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055.}  

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGE criterion. doi:10.5194/hess-22-4583-2018.}

\cite{Schaefli, B., Gupta, H. (2007). Do Nash values have value?. Hydrological Processes 21, 2075-2080. doi:10.1002/hyp.6825.}

\cite{Schober, P.; Boer, C.; Schwarte, L.A. (2018). Correlation coefficients: appropriate use and interpretation. Anesthesia and Analgesia, 126(5), 1763-1768. doi:10.1213/ANE.0000000000002864.}

\cite{Schuol, J.; Abbaspour, K.C.; Srinivasan, R.; Yang, H. (2008b), Estimation of freshwater availability in the West African sub-continent using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30, doi:10.1016/j.jhydrol.2007.12.025}

\cite{Sorooshian, S., Q. Duan, and V. K. Gupta. (1993). Calibration of rainfall-runoff models: Application of global optimization to the Sacramento Soil Moisture Accounting Model, Water Resources Research, 29 (4), 1185-1194, doi:10.1029/92WR02617}.

\cite{Spearman, C. (1961). The Proof and Measurement of Association Between Two Things. In J. J. Jenkins and D. G. Paterson (Eds.), Studies in individual differences: The search for intelligence (pp. 45-58). Appleton-Century-Crofts. doi:10.1037/11491-005}

\cite{Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). SC-earth: a station-based serially complete earth dataset from 1950 to 2019. Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.}

\cite{Yapo P.O.; Gupta H.V.; Sorooshian S. (1996). Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48. doi:10.1016/0022-1694(95)02918-4}

\cite{Yilmaz, K.K., Gupta, H.V. ; Wagener, T. (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resources Research, 44, W09417, doi:10.1029/2007WR006716.}

\cite{Willmott, C.J. (1981). On the validation of models. Physical Geography, 2, 184--194. doi:10.1080/02723646.1981.10642213.}

\cite{Willmott, C.J. (1984). On the evaluation of model performance in physical geography. Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460. doi:10.1007/978-94-017-3048-8_23.}

\cite{Willmott, C.J.; Ackleson, S.G. Davis, R.E.; Feddema, J.J.; Klink, K.M.; Legates, D.R.; O'Donnell, J.; Rowe, C.M. (1985), Statistics for the Evaluation and Comparison of Models, J. Geophys. Res., 90(C5), 8995-9005. doi:10.1029/JC090iC05p08995.}

\cite{Willmott, C.J.; Matsuura, K. (2005). Advantages of the mean absolute error (MAE) over the root mean square error (RMSE) in assessing average model performance, Climate Research, 30, 79-82, doi:10.3354/cr030079.}

\cite{Willmott, C.J.; Matsuura, K.; Robeson, S.M. (2009). Ambiguities inherent in sums-of-squares-based error statistics, Atmospheric Environment, 43, 749-752, doi:10.1016/j.atmosenv.2008.10.005.}

\cite{Willmott, C.J.; Robeson, S.M.; Matsuura, K. (2012). A refined index of model performance. International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419.}

\cite{Willmott, C.J.; Robeson, S.M.; Matsuura, K.; Ficklin, D.L. (2015). Assessment of three dimensionless measures of model performance. Environmental Modelling & Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012}

\cite{Zambrano-Bigiarini, M.; Bellin, A. (2012). Comparing goodness-of-fit measures for calibration of models focused on extreme events. EGU General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1.}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
%%\note{
%%
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gof}}, \code{\link{plot2}}, 
\code{\link{ggof}}, \code{\link{me}}, \code{\link{mae}}, \code{\link{mse}}, \code{\link{rmse}}, \code{\link{ubRMSE}}, 
\code{\link{nrmse}}, \code{\link{pbias}}, \code{\link{rsr}}, \code{\link{rSD}}, \code{\link{NSE}}, \code{\link{mNSE}}, 
\code{\link{rNSE}}, \code{\link{wNSE}}, \code{\link{d}}, \code{\link{dr}}, \code{\link{md}}, \code{\link{rd}}, 
\code{\link{cp}}, \code{\link{rPearson}}, \code{\link{R2}}, \code{\link{br2}}, \code{\link{KGE}}, \code{\link{KGElf}},
 \code{\link{KGEnp}}, \code{\link{sKGE}}, \code{\link{VE}}, \code{\link{rSpearman}}, \code{\link{pbiasfdc}}
}
\examples{
obs <- 1:10
sim <- 2:11

\dontrun{
ggof(sim, obs)
}

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Getting the numeric goodness of fit for the "best" (unattainable) case
gof(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Getting the new numeric goodness-of-fit measures
gof(sim=sim, obs=obs)

# Getting the graphical representation of 'obs' and 'sim' along with the numeric 
# goodness-of-fit measures for the daily and monthly time series 
\dontrun{
ggof(sim=sim, obs=obs, ftype="dm", FUN=mean)
}

# Getting the graphical representation of 'obs' and 'sim' along with some numeric 
# goodness-of-fit measures for the seasonal time series 
\dontrun{
ggof(sim=sim, obs=obs, ftype="seasonal", FUN=mean)
}

# Computing the daily residuals 
# even if this is a dummy example, it is enough for illustrating the capability
r <- sim-obs

# Summarizing and plotting the residuals
\dontrun{
library(hydroTSM)

# summary
smry(r) 

# daily, monthly and annual plots, boxplots and histograms
hydroplot(r, FUN=mean)

# seasonal plots and boxplots
hydroplot(r, FUN=mean, pfreq="seasonal")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{math}% __ONLY ONE__ keyword per line
