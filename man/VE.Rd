%% File VE.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2016 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{ve}
\Rdversion{1.1}
\alias{VE}
\alias{VE.default}
\alias{VE.matrix}
\alias{VE.data.frame}
\alias{VE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Volumetric Efficiency
}
\description{
Volumetric efficiency between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
VE(sim, obs, ...)

\method{VE}{default}(sim, obs, na.rm=TRUE, ...)

\method{VE}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{VE}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{VE}{zoo}(sim, obs, na.rm=TRUE, ...)
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
\deqn{ VE = 1 -\frac { \sum_{i=1}^N { \left| S_i - O_i \right| } } { \sum_{i=1}^N { \left( O_i \right) } }  }{%
VE = 1 - ( sum( abs(obs - sim) ) / sum( obs ) ) }

Volumetric efficiency was proposed in order to circumvent some problems associated to the Nash-Sutcliffe efficiency. It ranges from 0 to 1 and represents the fraction of water delivered at the proper time; its compliment represents the fractional volumetric mistmach (Criss and Winston, 2008).

}
\value{
Volumetric efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Volumetric efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072}

}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} have to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gof}}, \code{\link{ggof}}, \code{\link{NSE}}
}
\examples{
obs <- 1:10
sim <- 1:10
VE(sim, obs)

obs <- 1:10
sim <- 2:11
VE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the volumetric efficiency for the "best" case
VE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new volumetric efficiency
VE(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
