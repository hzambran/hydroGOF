% File pbias.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{pbias}
\Rdversion{1.1}
\alias{pbias}
\alias{pbias.default}
\alias{pbias.matrix}
\alias{pbias.data.frame}
\alias{pbias.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent Bias
}
\description{
Percent Bias between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
pbias(sim, obs, ...)

\method{pbias}{default}(sim, obs, na.rm=TRUE, ...)

\method{pbias}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{pbias}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{pbias}{zoo}(sim, obs, na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
\deqn{PBIAS = 100 \frac{  \sum_{i=1}^N { \left( S_i - O_i \right) } } { \sum_{i=1}^N O_i} }{%
PBIAS = 100 * [ sum( sim - obs ) / sum( obs ) ] } 

Percent bias (PBIAS) measures the average tendency of the simulated values to be larger or smaller than their observed ones. \cr

The optimal value of PBIAS is 0.0, with low-magnitude values indicating accurate model simulation. Positive values indicate overestimation bias, whereas negative values indicate model underestimation bias
}
\value{
Percent bias between \code{sim} and \code{obs}. The result is given in percentage (\%) \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the percent bias between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Yapo P. O., Gupta H. V., Sorooshian S., 1996. Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48}\cr

\cite{Sorooshian, S., Q. Duan, and V. K. Gupta. 1993. Calibration of rainfall-runoff models: Application of global optimization to the Sacramento Soil Moisture Accounting Model, Water Resources Research, 29 (4), 1185-1194, doi:10.1029/92WR02617}.
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gof}}, \code{\link{ggof}}
}
\examples{
obs <- 1:10
sim <- 1:10
pbias(sim, obs)

obs <- 1:10
sim <- 2:11
pbias(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'pbias' for the "best" case
pbias(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'pbias'
pbias(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
