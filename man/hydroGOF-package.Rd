% File hydroGOF-package.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://CRAN.R-project.org/package=hydroGOF
%% Copyright 2009-2017 Mauricio Zambrano-Bigiarini
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
Package: \tab hydroGOF\cr
Type: \tab Package\cr
Version: \tab 0.3-10\cr
Date: \tab 2017-08-08\cr
License: \tab GPL >= 2\cr
LazyLoad: \tab yes\cr
Packaged: \tab Tue Aug  8 09:11:25 CLT 2017; MZB \cr
BuiltUnder: \tab R version 3.4.1 (2017-06-30) -- "Single Candle"; x86_64-pc-linux-gnu (64-bit)) \cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>

Maintainer: Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\references{
\cite{Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resour. Res., 36(12), 3663--3674} \cr

\cite{Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072}

\cite{Fenicia, F., D. P. Solomatine, H. H. G. Savenije, and P. Matgen, Soft combination of local models in a multi-objective framework. Hydrological and Earth Systems Science, Vol. 4, pp. 91-123, 2007. DOI:10.5194/hessd-4-91-2007} \cr

\cite{Gupta, Hoshin V., Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694} \cr

\cite{Kitanidis, P. K., and R. L. Bras (1980), Real-Time Forecasting With a Conceptual Hydrologic Model 2. Applications and Results, Water Resour. Res., 16(6), 1034--1044} \cr

\cite{Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, DOI:10.1016/j.jhydrol.2012.01.011}

\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89--97, 2005} \cr

\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233--241} \cr

\cite{Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic quantification of accuracy in watershed simulations \cr Transactions of the ASABE. 50(3):885-900} \cr

\cite{Nash, J.E. and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282--290} \cr

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. DOI: 10.1016/j.jhydrol.2011.11.055}  \cr 

\cite{Yapo P. O., Gupta H. V., Sorooshian S., 1996. Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23--48} \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716} \cr
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
