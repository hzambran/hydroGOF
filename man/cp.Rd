%% File cp.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or lat

\name{cp}
\Rdversion{1.1}
\alias{cp}
\alias{cp.default}
\alias{cp.matrix}
\alias{cp.data.frame}
\alias{cp.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coefficient of persistence
}
\description{
Coefficient of persistence between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
cp(sim, obs, ...)

\method{cp}{default}(sim, obs, na.rm=TRUE, ...)

\method{cp}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{cp}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{cp}{zoo}(sim, obs, na.rm=TRUE, ...)
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
\deqn{ cp = 1 -\frac { \sum_{i=2}^N { \left( S_i - O_i \right)^2 } } { \sum_{i=1}^{N-1} { \left( O_{i+1} - O_i \right)^2 } }  }{%
cp = 1 - [ sum( (obs[2:n] - sim[2:n])^2 ] / sum( ( obs[2:n] - obs[1:(n-1)] )^2 ) }
     
Coefficient of persistence (Kitadinis and Bras, 1980; Corradini et al., 1986) is used to compare the model  performance against a simple model using the observed value of the previous day as the prediction for the current day. \cr

The coefficient of persistence compare the predictions of the model with the predictions obtained by assuming that the process is a Wiener process (variance increasing linearly with time), in which case, the best estimate for the future is given by the latest measurement (Kitadinis and Bras, 1980). \cr

Persistence model efficiency is a normalized model evaluation statistic that quantifies the relative magnitude of the residual variance (noise) to the variance of the errors obtained by the use of a simple persistence model (Moriasi et al., 2007). 

CP ranges from 0 to 1, with CP = 1 being the optimal value and it should be larger than 0.0 to indicate a minimally acceptable model performance.
}
\value{
Coefficient of persistence between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the coefficient of persistence between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Kitanidis, P.K., and Bras, R.L. 1980. Real-time forecasting with a conceptual hydrologic model. 2. Applications and results. Water Resources Research, Vol. 16, No. 6, pp. 1034:1044} \cr

\cite{Moriasi, D. N. et al. (2007). Model Evaluation Guidelines for Systematic Quantification of Accuracy in Watershed Simulations. Transactions of the ASABE, 50:(3), 885-900}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gof}}
}
\examples{
obs <- 1:10
sim <- 1:10
cp(sim, obs)

obs       <- 1:10
sim[2:10] <- obs[1:9]
cp(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing 'cp' for the "best" (unattainable) case
cp(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new  'cp'
cp(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
