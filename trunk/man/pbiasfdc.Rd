% File pbiasfdc.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{pbiasfdc}
\Rdversion{1.1}
\alias{pbiasfdc}
\alias{pbiasfdc.default}
\alias{pbiasfdc.matrix}
\alias{pbiasfdc.data.frame}
\alias{pbiasfdc.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent Bias in the Slope of the Midsegment of the Flow Duration Curve
}
\description{
Percent Bias in the slope of the midsegment of the flow duration curve (FDC) [\%]. It is related to the vertical soil moisture redistribution. \cr
}
\usage{
pbiasfdc(sim, obs, ...)

\method{pbiasfdc}{default}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, ...)

\method{pbiasfdc}{data.frame}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, ...)

\method{pbiasfdc}{matrix}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, ...)
       
\method{pbiasfdc}{zoo}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{lQ.thr}{
numeric, used to classify low flows. All the streamflows with a probability of exceedence larger or equal to \code{lQ.thr} are classified as low flows
}
  \item{hQ.thr}{
numeric, used to classify high flows. All the streamflows with a probability of exceedence larger or equal to \code{hQ.thr} are classified as high flows
}
  \item{na.rm}{
a logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{plot}{
a logical value indicating if the flow duration curves corresponding to \code{obs} and \code{sim} have to be  plotted or not.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed 
}
  \item{\dots}{
further arguments passed to the \link[hydroTSM]{fdc} function of the \pkg{hydroTSM} package or from other methods.
}
}
%%\details{
%%
%%}
\value{
Percent Bias in the slope of the midsegment of the flow duration curve, between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Percent Bias in the slope of the midsegment of the flow duration curve, between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
The result is given in percentage (\%). \cr

It requires the \pkg{hydroTSM} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[hydroTSM]{fdc}, \code{\link{gof}}
}
\examples{
\dontrun{
sim <- 1:10
obs <- 1:10
pbiasfdc(sim, obs)

sim <- 2:11
obs <- 1:10
pbiasfdc(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the relative index of agreement for the "best" (unattainable) case
pbiasfdc(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new relative index of agreement
pbiasfdc(sim=sim, obs=obs, col=c("black", "blue"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
