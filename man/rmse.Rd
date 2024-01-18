% File rmse.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{rmse}
\Rdversion{1.1}
\alias{rmse}
\alias{rmse.default}
\alias{rmse.matrix}
\alias{rmse.data.frame}
\alias{rmse.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Root Mean Square Error
}
\description{
Root Mean Square Error (RMSE) between \code{sim} and \code{obs}, in the same units of \code{sim} and \code{obs}, with treatment of missing values. \cr
RMSE gives the standard deviation of the model prediction error. A smaller value indicates better model performance.
}
\usage{
rmse(sim, obs, ...)

\method{rmse}{default}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{rmse}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{rmse}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{rmse}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)
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
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Root Mean Square Error.

The first two arguments of \code{fun} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and addtional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{FUN}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{FUN}. 

It is was  designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value. This is the default option.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012). 

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}
  \item{epsilon.value}{
 numeric value to be added to both \code{sim} and \code{obs} when \code{epsilon.type="otherValue"}.
}
}
\details{
\deqn{ rmse = \sqrt{ \frac{1}{N} \sum_{i=1}^N { \left( S_i - O_i \right)^2 } } }{%
rmse = sqrt( mean( (sim - obs)^2, na.rm = TRUE) ) } 
}
\value{
Root mean square error (rmse) between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the RMSE between each column of \code{sim} and \code{obs}.
}
\references{
\url{https://en.wikipedia.org/wiki/Root_mean_square_deviation}
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
\code{\link{nrmse}}, \code{\link{ssq}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
obs <- 1:10
sim <- 1:10
rmse(sim, obs)

obs <- 1:10
sim <- 2:11
rmse(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970

data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the root mean squared error for the "best" (unattainable) case
rmse(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new root mean squared error
rmse(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
