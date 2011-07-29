\name{NSE}
\Rdversion{1.1}
\alias{NSeff}
\alias{NSE}
\alias{NSE.default}
\alias{NSE.matrix}
\alias{NSE.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nash-Sutcliffe Efficiency
}
\description{
Nash-Sutcliffe efficiency between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
NSE(sim, obs, ...)

\method{NSE}{default}(sim, obs, na.rm=TRUE, ...)

\method{NSE}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{NSE}{matrix}(sim, obs, na.rm=TRUE, ...)
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
\deqn{ NSE = 1 -\frac { \sum_{i=1}^N { \left( S_i - O_i \right)^2 } } { \sum_{i=1}^N { \left( O_i - \bar{O} \right)^2 } }  }{%
NSE = 1 - ( sum( (obs - sim)^2 ) / sum( (obs - mean(obs))^2 ) }

The Nash-Sutcliffe efficiency (NSE) is a normalized statistic that determines the relative magnitude of the residual variance ("noise") compared to the measured data variance ("information") (Nash and Sutcliffe, 1970). \cr

NSE indicates how well the plot of observed versus simulated data fits the 1:1 line.\cr

Nash-Sutcliffe efficiencies range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. \cr
-) NSE = 1, corresponds to a perfect match of modelled to the observed data. \cr
-) NSE = 0, indicates that the model predictions are as accurate as the mean of the observed data, \cr
-) -Inf < NSE < 0, indicates that the observed mean is better predictor than the model.
}
\value{
Nash-Sutcliffe efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Nash-Sutcliffe efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Nash, J. E. and J. V. Sutcliffe (1970), River flow forecasting through conceptual models part I -A discussion of principles, Journal of Hydrology, 10 (3), 282-290} \cr

\url{http://en.wikipedia.org/wiki/Nash\%E2\%80\%93Sutcliffe_model_efficiency_coefficient}
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mNSE}}, \code{\link{rNSE}}
}
\examples{
obs <- 1:10
sim <- 1:10
NSE(sim, obs)

obs <- 1:10
sim <- 2:11
NSE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
NSE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'NSE'
NSE(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
