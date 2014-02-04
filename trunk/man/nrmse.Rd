% File nrmse.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{nrmse}
\Rdversion{1.1}
\alias{nrmse}
\alias{nrmse.default}
\alias{nrmse.matrix}
\alias{nrmse.data.frame}
\alias{nrmse.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized Root Mean Square Error
}
\description{
Normalized root mean square error (NRMSE) between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
nrmse(sim, obs, ...)

\method{nrmse}{default}(sim, obs, na.rm=TRUE, norm="sd", ...)

\method{nrmse}{data.frame}(sim, obs, na.rm=TRUE, norm="sd", ...)

\method{nrmse}{matrix}(sim, obs, na.rm=TRUE, norm="sd", ...)

\method{nrmse}{zoo}(sim, obs, na.rm=TRUE, norm="sd", ...)
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
  \item{norm}{
character, indicating the value to be used for normalising the root mean square error (RMSE). Valid values are: \cr
-) \kbd{sd}    : standard deviation of observations (default). \cr
-) \kbd{maxmin}: difference between the maximum and minimum observed values
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
\deqn{ nrmse = 100 \frac {\sqrt{ \frac{1}{N} \sum_{i=1}^N { \left( S_i - O_i \right)^2 } } } {nval} }
\deqn{nval=  \left\{
  \begin{array}{cl}
    sd(O_i)           & , \: \textrm{norm="sd"} \\
    O_{max} - O_{min} & , \: \textrm{norm="maxmin"}    
  \end{array}
  \right.}{%
nrmse =  100 * [ rmse(sim, obs) / nval ] ; nval= range(obs, na.rm=TRUE) OR nval=sd(obs), depending on the \code{norm} value } 
}
\value{
Normalized root mean square error (nrmse) between \code{sim} and \code{obs}. The result is given in percentage (\%) \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the normalized root mean square error between each column of \code{sim} and \code{obs}.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} have to have the same length/dimension \cr

Missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rmse}}, \code{\link{ssq}}, \code{\link{gof}}, \code{\link{ggof}}
}

\examples{
obs <- 1:10
sim <- 1:10
nrmse(sim, obs)

obs <- 1:10
sim <- 2:11
nrmse(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the normalized root mean squared error for the "best" (unattainable) case
nrmse(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new normalized root mean squared error
nrmse(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
