%% File hydroGOF-package.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2009-2020 Mauricio Zambrano-Bigiarini
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

Quantitative statistics included are: Mean Error (\bold{me}), Mean Absolute Error (\bold{mae}), Root Mean Square Error (\bold{rms}), Normalized Root Mean Square Error (\bold{nrms}), Pearson product-moment correlation coefficient (\bold{r}), Spearman Correlation coefficient (\bold{r.Spearman}), Coefficient of Determination (\bold{R2}), Ratio of Standard Deviations (\bold{rSD}), Nash-Sutcliffe efficiency (\bold{NSE}), Modified Nash-Sutcliffe efficiency (\bold{mNSE}), Relative Nash-Sutcliffe efficiency (\bold{rNSE}), Index of Agreement (\bold{d}), Modified Index of Agreement (\bold{md}),  Relative Index of Agreement (\bold{rd}), Coefficient of Persistence (\bold{cp}), Percent Bias (\bold{pbias}), Kling-Gupta efficiency (\bold{KGE}), the coef. of determination multiplied by the slope of the linear regression between 'sim' and 'obs' (\bold{bR2}), and volumetric efficiency (\bold{VE}).

}
\details{
\tabular{ll}{
Type: \tab Package\cr
Version: \tab 0.4-0\cr
Date: \tab 2020-03-11\cr
License: \tab GPL >= 2\cr
LazyLoad: \tab yes\cr
Packaged: \tab Wed Mar 11 22:23:51 -03 2020; MZB \cr
BuiltUnder: \tab R version 3.6.3 (2020-02-29) -- "Holding the Windsock"; x86_64-pc-linux-gnu (64-bit)) \cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>

Maintainer: Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\references{

\cite{Bennett, N.D.; Croke, B.F.; Guariso, G.; Guillaume, J.H.; Hamilton, S.H.; Jakeman, A.J.; Marsili-Libelli, S.; Newham, L.T.; Norton, J.P.; Perrin, C.; Pierce, S.A. (2013). Characterising performance of environmental models. Environmental Modelling \& Software, 40, 1-20. doi:10.1016/j.envsoft.2012.09.011} \cr

\cite{Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resources Research, 36(12), 3663-3674. doi:10.1029/2000WR900207} \cr

\cite{Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi:10.1002/hyp.7072}

\cite{Fenicia, F., D. P. Solomatine, H. H. G. Savenije, and P. Matgen. (2007) Soft combination of local models in a multi-objective framework. Hydrological and Earth Systems Science, Vol. 4, pp. 91-123. doi:10.5194/hessd-4-91-2007} \cr

\cite{Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. Hydrological sciences journal, 62(7), 1149-1166. doi:10.1080/02626667.2017.1308511} \cr

\cite{Gupta, Hoshin V., Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. (2009). Journal of Hydrology, Volume 377, Issues 1-2, 20, Pages 80-91. doi:10.1016/j.jhydrol.2009.08.003} \cr

\cite{Harmel, R.D.; Smith, P.K.; Migliaccio, K.W.; Chaubey, I.; Douglas-Mankin, K.R.; Benham, B.; Shukla, S.; Munoz-Carpena, R.; Robson, B.J., 2014. Evaluating, interpreting, and communicating performance of hydrologic/water quality models considering intended use: A review and recommendations. Environmental modelling \& software, 57, 40-51. doi:10.1016/j.envsoft.2014.02.013} \cr

\cite{Kitanidis, P. K., and R. L. Bras (1980), Real-Time Forecasting With a Conceptual Hydrologic Model 2. Applications and Results, Water Resour. Res., 16(6), 1034-1044} \cr

\cite{Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, doi:10.1016/j.jhydrol.2012.01.011} \cr

\cite{Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019} \cr

\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005. doi:10.5194/adgeo-5-89-2005} \cr

\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241. doi:10.1029/1998WR900018} \cr

\cite{Mizukami, N.; Rakovec, O.; Newman, A. J.; Clark, M. P.; Wood, A. W.; Gupta, H. V.; Kumar, R. (2019). On the choice of calibration metrics for "high-flow" estimation using hydrologic models. doi:10.5194/hess-23-2601-2019} \cr

\cite{Moriasi, D.N.; Arnold, J.G.; Van Liew, M.W.; Bingner, R.L.; Harmel, R.D.; Veith, T.L. (2007). Model evaluation guidelines for systematic quantification of accuracy in watershed simulations \cr Transactions of the ASABE. 50(3):885-900} \cr

\cite{Nash, J.E. and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282-290. doi:10.1016/0022-1694(70)90255-6} \cr

\cite{Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature metrics for an improved overall performance evaluation of hydrological models. Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044} \cr

\cite{Pool, S., Vis, M. and Seibert, J. (2018). Evaluating model performance: towards a non-parametric variant of the Kling-Gupta efficiency. Hydrological Sciences Journal, 63(13-14), pp.1941-1953. doi:/10.1080/02626667.2018.1552002.} \cr

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andr\'{e}assian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055} \cr

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGElf criterion. doi:10.5194/hess-22-4583-2018} \cr

\cite{Willmott, C.J., Robeson, S.M. and Matsuura, K. (2012). A refined index of model performance. International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419.} \cr

\cite{Willmott, C.J., Robeson, S.M., Matsuura, K. and Ficklin, D.L. (2015). Assessment of three dimensionless measures of model performance. Environmental Modelling \& Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012} \cr

\cite{Willmott, C. J. (1981). On the validation of models. Physical Geography, 2, 184--194} \cr

\cite{Willmott, C. J. (1984). On the evaluation of model performance in physical geography. Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460} \cr

\cite{Willmott, C. J., S. G. Ackleson, R. E. Davis, J. J. Feddema, K. M. Klink, D. R. Legates, J. O'Donnell, and C. M. Rowe (1985), Statistics for the Evaluation and Comparison of Models, J. Geophys. Res., 90(C5), 8995-9005} \cr

\cite{Yapo, P. O.; Gupta, H. V.; Sorooshian S. (1996). Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48. doi:10.1016/0022-1694(95)02918-4} \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resources Research, 44, W09417, doi:10.1029/2007WR006716} \cr
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
