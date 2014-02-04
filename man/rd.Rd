% File rd.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{rd}
\Rdversion{1.1}
\alias{rd}
\alias{rd.default}
\alias{rd.matrix}
\alias{rd.data.frame}
\alias{rd.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relative Index of Agreement
}
\description{
This function computes the Relative Index of Agreement (d) between \code{sim} and \code{obs}, with treatment of missing values. \cr
If \code{x} is a matrix or a data frame, a vector of the relative index of agreement among the columns is returned.
}
\usage{
rd(sim, obs, ...)

\method{rd}{default}(sim, obs, na.rm=TRUE, ...)

\method{rd}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{rd}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{rd}{zoo}(sim, obs, na.rm=TRUE, ...)
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
\deqn{rd = 1 - \frac{ \sum_{i=1}^N { \left( \frac{O_i - S_i}{O_i} \right) ^2} } { \sum_{i=1}^N {  \left( \frac{ \left| S_i -  \bar{O} \right| + \left| O_i -  \bar{O}  \right|}{\bar{O}} \right)^2 } } }{%
rd = 1 - [ sum( ( (obs - sim) / obs )^2  ] /  sum( ( (abs(sim - mean(obs) ) + abs(obs - mean(obs) ) ) / mean(obs)  )^2 ) } 

%% This index was developed to  be sensitive to systematic over- or under-prediction, in particular during low flow conditions. \cr

%% This index quantify the difference between simulated and observed values in a relative way, in order to significatively reduce the influence of the absolute differences of high flows and to give more weight to over- or under-prediction of low flows. At the same time, differences in low flows become more important, because they are looked in a relative way. \cr

It varies between 0 and 1. A value of 1 indicates a perfect match, and 0 indicates no agreement at all.
}
\value{
Relative index of agreement between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the relative index of agreement between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005} \cr

\cite{Willmott, C. J. 1981. On the validation of models. Physical Geography, 2, 184--194} \cr

\cite{Willmott, C. J. (1984). On the evaluation of model performance in physical geography. Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460} \cr

\cite{Willmott, C. J., S. G. Ackleson, R. E. Davis, J. J. Feddema, K. M. Klink, D. R. Legates, J. O'Donnell, and C. M. Rowe (1985), Statistics for the Evaluation and Comparison of Models, J. Geophys. Res., 90(C5), 8995-9005} \cr

\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233--241}
}

\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation. \cr

If some of the observed values are equal to zero (at least one of them), this index can not be computed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{d}}, \code{\link{md}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
obs <- 1:10
sim <- 1:10
rd(sim, obs)

obs <- 1:10
sim <- 2:11
rd(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the relative index of agreement for the "best" (unattainable) case
rd(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new relative index of agreement
rd(sim=sim, obs=obs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
