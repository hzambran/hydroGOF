\name{wh}
\Rdversion{1.1}
\alias{wh}
\alias{wh.default}
\alias{wh.matrix}
\alias{wh.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
larger Weights for High values
}
\description{
\code{wh} computes weights for all the values given as input, with larger weights for the high flows and smaller weights for the low ones, depending on the values given to \code{k} and \code{pbb}.
}
\usage{
wh(x, ...)

\method{wh}{default}(x, k=0.5, pbb=0.8, ...)

\method{wh}{data.frame}(x, k=0.5, pbb=0.8, ...)

\method{wh}{matrix}(x, k=0.5, pbb=0.8, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric, zoo, matrix or data.frame with the time series used for computing the weights.
}
  \item{k}{
numeric, with the exponent to be used in the computation of the weights. The default value is \code{k=0.5}.
}
  \item{pbb}{
numeric, with the probability (with a value in [0,1]) used to computed the quantile of \code{x} that defines \code{x_max}, which is used as a threshold for which all the \code{x} values larger than \code{x_max} receives a weight equal to 1. \cr
The smallest value in \code{x} corresponds to a probability of 0 and the largest one to a probability of 1. \cr
The default value is \code{pbb=0.8}.
}
%%  \item{na.rm}{
%%a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
%%When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
%%}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{

%%\deqn{ wrmse_{High} = \sqrt{ \frac{1}{n} * \sum_{i=1}^N { \left( S_i - O_i \right)^j * w_{High}} } } 
%%\deqn{ wrmse_{Low} = \sqrt{ \frac{1}{n} * \sum_{i=1}^N { \left( S_i - O_i \right)^j * w_{Low}} } } 
%%\deqn{ w_{High} = \sum_{i=1}^N { \left( \frac{O_i}{O_{max}} \right)^j } }
%%\deqn{ w_{Low}  = \sum_{i=1}^N { \left( \frac{Q_{max} - O_i}{O_{max}} \right)^j } }{%
%%wrmse_high = sqrt( (1/n) * w_high * sum( sim - obs)^j ) ; wrmse_low = sqrt( (1/n) * w_low * sum( sim - obs)^j ) ; w_high = (obs / max(obs))^j ; w_low = ((max(obs)-obs)/max(obs))^j  }
%%}
\deqn{ wh = \sum_{i=1}^N { \left( \frac{x_i}{x_{max}} \right)^j } }{%
wh = [x / max(x) ]^j }
}
\value{
Weights for each values in \code{x}. \cr

If \code{x} is a matrix or data.frame, the returned value is also a matrix or data.frame, with the weights computed for each column of \code{x}.
}
%%\references{
%%}
\author{
Mauricio Zambrano-Bigiarini <mauricio.zambrano@jrc.ec.europa.eu>
}
\note{
\code{x} may contain missing values, and in such a case a missing value is returned for those values
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wl}}, \code{\link{whl}}
}
\examples{
x <- 1:1000
wh(x)


##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
        
# Computing 'wh'
w <- wh(obs)
plot(obs, w, type="p")

w <- wh(obs, pbb=1)
plot(obs, w, type="p")

w <- wh(obs, k=2)
plot(obs, w, type="p")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
