\name{hydroGOF-package}
\Rdversion{1.1}
\alias{hydroGOF-package}
\alias{hydroGOF}
\docType{package}
\title{
Goodness-of-fit (GOF) functions for numerical and graphical comparison of simulated and observed time series, focused on hydrological modelling.
}
\description{
S3 functions implementing both statistical and graphical goodness-of-fit measures between observed and simulated values, for being used during the calibration, validation, and application of hydrological models. \cr

Missing values in observed and/or simulated values can removed before the computations. \cr

Quantitative statistics included are: Mean Error (\bold{me}), Mean Absolute Error (\bold{mae}), Root Mean Square Error (\bold{rms}), Normalized Root Mean Square Error (\bold{nrms}), Pearson Correlation coefficient (\bold{r}), Spearman Correlation coefficient (\bold{r.Spearman}), Coefficient of Determination (\bold{R2}), Ratio of Standard Deviations (\bold{rSD}), Nash-Sutcliffe efficiency (\bold{NSeff}), Modified Nash-Sutcliffe efficiency (\bold{mNSeff}), Index of Agreement (\bold{d}), Modified Index of Agreement (\bold{md}),  Coefficient of Persistence (\bold{cp}), Percent Bias (\bold{pbias}), Kling-Gupta efficiency (\bold{KGE}), and the coef. of determination multiplied by the slope of the linear regression between 'sim' and 'obs' (\bold{bR2})

}
\details{
\tabular{ll}{
Package: \tab hydroGOF\cr
Type: \tab Package\cr
Version: \tab 0.3-2\cr
Date: \tab 2011-10-07\cr
License: \tab GPL >= 2\cr
LazyLoad: \tab yes\cr
Packaged: \tab Fri Oct  7 09:28:22 CEST 2011; MZB \cr
BuiltUnder: \tab R version 2.13.1 (2011-07-08); i686-pc-linux-gnu (32-bit) \cr
}
%%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>

Maintainer: Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\references{
\cite{Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resour. Res., 36(12), 3663--3674} \cr

\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89--97, 2005} \cr

\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233--241} \cr

\cite{Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic quantification of accuracy in watershed simulations \cr Transactions of the ASABE. 50(3):885-900} \cr

\cite{Kitanidis, P. K., and R. L. Bras (1980), Real-Time Forecasting With a Conceptual Hydrologic Model 2. Applications and Results, Water Resour. Res., 16(6), 1034--1044} \cr

\cite{J.E. Nash and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282--290} \cr

\cite{Yapo P. O., Gupta H. V., Sorooshian S., 1996. Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23--48} \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}

\cite{Fenicia, F., D. P. Solomatine, H. H. G. Savenije, and P. Matgen, Soft combination of local models in a multi-objective framework. Hydrological and Earth Systems Science, Vol. 4, pp. 91-123, 2007. DOI:10.5194/hessd-4-91-2007}

\cite{Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694}
}
%%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{

\url{http://www.rforge.net/hydroTSM/}. \cr

\url{http://cran.r-project.org/web/packages/hydroTSM/}. \cr

\url{http://rwiki.sciviews.org/doku.php?id=guides:tutorials:hydrological_data_analysis}
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

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Getting the numeric goodness-of-fit measures for the "best" (unattainable) case
gof(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Getting the new numeric goodness of fit
gof(sim=sim, obs=obs)

# Graphical representation of 'obs' vs 'sim', along with the numeric goodness-of-fit measures
\dontrun{
ggof(sim=sim, obs=obs)
}
}
