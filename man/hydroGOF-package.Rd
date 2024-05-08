%% File hydroGOF-package.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2009-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{hydroGOF-package}
\Rdversion{1.1}
\alias{hydroGOF-package}
\alias{hydroGOF}
\docType{package}
\title{
Goodness-of-fit (GoF) functions for numerical and graphical comparison of simulated and observed time series, mainly focused on hydrological modelling.
}
\description{
S3 functions implementing both statistical and graphical goodness-of-fit measures between observed and simulated values, to be used during the calibration, validation, and application of hydrological models. \cr

Missing values in observed and/or simulated values can be removed before computations. \cr

}
\details{
\tabular{ll}{
Package: \tab hydroGOF\cr
Type: \tab Package\cr
Version: \tab 0.6-0\cr
Date: \tab 2024-05-08\cr
License: \tab GPL >= 2\cr
LazyLoad: \tab yes\cr
Packaged: \tab Wed 08 May 2024 05:13:53 PM -04 ; MZB \cr
BuiltUnder: \tab R version 4.4.0 (2024-04-24) -- "Puppy Cup" ;x86_64-pc-linux-gnu (64-bit) \cr
} 

Quantitative statistics included in this package are: 

  \tabular{ll}{
    \code{\link{me}}         Mean Error \cr
    \code{\link{mae}}        Mean Absolute Error \cr 
    \code{\link{mse}}        Mean Squared Error \cr
    \code{\link{rmse}}       Root Mean Square Error \cr
    \code{\link{ubRMSE}}     Unbiased Root Mean Square Error \cr
    \code{\link{nrmse}}      Normalized Root Mean Square Error \cr
    \code{\link{pbias}}      Percent Bias \cr
    \code{\link{rsr}}        Ratio of RMSE to the Standard Deviation of the Observations \cr
    \code{\link{rSD}}        Ratio of Standard Deviations \cr
    \code{\link{NSE}}        Nash-Sutcliffe Efficiency \cr
    \code{\link{mNSE}}       Modified Nash-Sutcliffe Efficiency \cr
    \code{\link{rNSE}}       Relative Nash-Sutcliffe Efficiency \cr
    \code{\link{wNSE}}       Weighted Nash-Sutcliffe Efficiency \cr
    \code{\link{wsNSE}}      Weighted Seasonal Nash-Sutcliffe Efficiency \cr
    \code{\link{d}}          Index of Agreement \cr
    \code{\link{dr}}         Refined Index of Agreement \cr
    \code{\link{md}}         Modified Index of Agreement  \cr
    \code{\link{rd}}         Relative Index of Agreement \cr
    \code{\link{cp}}         Persistence Index \cr
    \code{\link{rPearson}}   Pearson correlation coefficient  \cr
    \code{\link{R2}}         Coefficient of determination \cr
    \code{\link{br2}}        R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} \cr
    \code{\link{VE}}         Volumetric efficiency  \cr
    \code{\link{KGE}}        Kling-Gupta efficiency \cr
    \code{\link{KGElf}}      Kling-Gupta Efficiency for low values \cr
    \code{\link{KGEnp}}      Non-parametric version of the Kling-Gupta Efficiency \cr
    \code{\link{KGEkm}}      Knowable Moments Kling-Gupta Efficiency \cr
    \code{\link{sKGE}}       Split Kling-Gupta Efficiency \cr
    \code{\link{APFB}}       Annual Peak Flow Bias \cr
    \code{\link{HFB}}        High Flow Bias \cr
    \code{\link{rSpearman}}  Spearman's rank correlation coefficient  \cr
    \code{\link{ssq}}        Sum of the Squared Residuals \cr
    \code{\link{pbiasfdc}}   PBIAS in the slope of the midsegment of the flow duration curve \cr
    \code{\link{pfactor}}    P-factor \cr
    \code{\link{rfactor}}    R-factor \cr
----------------------------------------------------------------------------------------------------------\cr
}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>

Maintainer: Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
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
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{

\url{https://CRAN.R-project.org/package=hydroPSO} \cr
\url{https://CRAN.R-project.org/package=hydroTSM}
}
\examples{
obs <- 1:100
sim <- obs

# Numerical goodness of fit
gof(sim,obs)

# Reverting the order of simulated values
sim <- 100:1
gof(sim,obs)

\dontrun{
ggof(sim, obs)
}

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to observations
sim <- obs 

# Getting the numeric goodness-of-fit measures for the "best" (unattainable) case
gof(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal 
# distribution  with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Getting the new numeric goodness of fit
gof(sim=sim, obs=obs)

# Graphical representation of 'obs' vs 'sim', along with the numeric 
# goodness-of-fit measures
\dontrun{
ggof(sim=sim, obs=obs)
}
}
