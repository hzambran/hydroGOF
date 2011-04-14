\name{mNSeff}
\Rdversion{1.1}
\alias{mNSeff}
\alias{mNSeff.default}
\alias{mNSeff.matrix}
\alias{mNSeff.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Nash-Sutcliffe efficiency
}
\description{
Modified Nash-Sutcliffe efficiency between \code{sim} and \code{obs}, with treatment of missing values. \cr
}
\usage{
mNSeff(sim, obs, ...)

\method{mNSeff}{default}(sim, obs, j=1, na.rm=TRUE, ...)

\method{mNSeff}{data.frame}(sim, obs, j=1, na.rm=TRUE, ...)

\method{mNSeff}{matrix}(sim, obs, j=1, na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{j}{
numeric, with the exponent to be used in the computation of the modified Nash-Sutcliffe efficiency. The default value is \code{j=1}.
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

\deqn{ mNSeff = 1 -\frac { \sum_{i=1}^N { \left| S_i - O_i \right|^j } } { \sum_{i=1}^N { \left| O_i - \bar{O} \right|^j } }  }{%
mNSeff = 1 - ( sum( abs(obs - sim)^j ) / sum( abs(obs - mean(obs))^j )}

When \code{j=1}, the modified NSeff is not inflated by the squared values of the differences, because the squares are replaced by absolute values.
}
\value{
Modified Nash-Sutcliffe efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the modified Nash-Sutcliffe efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005} \cr

\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241} 
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NSeff}}, \code{\link{rNSeff}}
}
\examples{
sim <- 1:10
obs <- 1:10
mNSeff(sim, obs)

sim <- 2:11
obs <- 1:10
mNSeff(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'mNSeff' for the "best" (unattainable) case
mNSeff(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'mNSeff'
mNSeff(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
