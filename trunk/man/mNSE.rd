% File mNSE.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{mNSE}
\Rdversion{1.1}
\alias{mNSeff}
\alias{mNSE}
\alias{mNSE.default}
\alias{mNSE.matrix}
\alias{mNSE.data.frame}
\alias{mNSE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modified Nash-Sutcliffe efficiency
}
\description{
Modified Nash-Sutcliffe efficiency between \code{sim} and \code{obs}, with treatment of missing values. \cr
}
\usage{
mNSE(sim, obs, ...)

\method{mNSE}{default}(sim, obs, j=1, na.rm=TRUE, ...)

\method{mNSE}{data.frame}(sim, obs, j=1, na.rm=TRUE, ...)

\method{mNSE}{matrix}(sim, obs, j=1, na.rm=TRUE, ...)

\method{mNSE}{zoo}(sim, obs, j=1, na.rm=TRUE, ...)
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

\deqn{ mNSE = 1 -\frac { \sum_{i=1}^N { \left| S_i - O_i \right|^j } } { \sum_{i=1}^N { \left| O_i - \bar{O} \right|^j } }  }{%
mNSE = 1 - ( sum( abs(obs - sim)^j ) / sum( abs(obs - mean(obs))^j )}

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
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation \cr
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NSE}}, \code{\link{rNSE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
sim <- 1:10
obs <- 1:10
mNSE(sim, obs)

sim <- 2:11
obs <- 1:10
mNSE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'mNSE' for the "best" (unattainable) case
mNSE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'mNSE'
mNSE(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
