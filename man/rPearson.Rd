% File rPearson.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2017 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{rPearson}
\Rdversion{1.1}
\alias{rPearson}
\alias{rPearson.default}
\alias{rPearson.matrix}
\alias{rPearson.data.frame}
\alias{rPearson.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean Squared Error
}
\description{
Correlation of \code{sim} and \code{obs} if these are vectors, with treatment of missing values. If \code{sim} and \code{obs} are matrices then the covariances (or correlations) between the columns of \code{sim} and the columns of \code{obs} are computed. It is a wrapper to the \code{\link[stats]{cor}} function.
}
\usage{
rPearson(sim, obs, ...)

\method{rPearson}{default}(sim, obs, ...)

\method{rPearson}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{rPearson}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{rPearson}{zoo}(sim, obs, na.rm=TRUE, ...)
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
It is a wrapper to the \code{\link[stats]{cor}} function.
}
\value{
Mean squared error between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the mean squared error between each column of \code{sim} and \code{obs}.
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
\code{\link[stats]{cor}}
}
\examples{
obs <- 1:10
sim <- 1:10
rPearson(sim, obs)

obs <- 1:10
sim <- 2:11
rPearson(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the linear correlation for the "best" case
rPearson(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new correlation value
rPearson(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
