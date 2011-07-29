\name{whl}
\Rdversion{1.1}
\alias{whl}
\alias{whl.default}
\alias{whl.zoo}
\alias{whl.matrix}
\alias{whl.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
larger Weights for High values
}
\description{
\code{whl} computes weights for all the values given as input, with larger weights for the high flows and smaller weights for the low ones, depending on the values given to \code{k} and \code{pbb}.
}
\usage{
whl(x, ...)

\method{whl}{default}(x, lambda=0.5, lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                      hQ=quantile(x, probs=0.8, na.rm=TRUE), ... )
                      
\method{whl}{zoo}(x, lambda=0.5, lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                  hQ=quantile(x, probs=0.8, na.rm=TRUE), ... )

\method{whl}{data.frame}(x, lambda=0.5, lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                         hQ=quantile(x, probs=0.8, na.rm=TRUE), ... )

\method{whl}{matrix}(x, lambda=0.5, lQ=quantile(x, probs=0.3, na.rm=TRUE), 
                     hQ=quantile(x, probs=0.8, na.rm=TRUE), ... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
numeric, zoo, matrix or data.frame with the time series used for computing the weights.
}
  \item{lambda}{
numeric between 0 and 1 representing the weight to be given to the high-flow part of the signal, with \code{lambda} values close to one when focusing on high-flow events, and close to zero when focusing in low-flow conditions
}
  \item{lQ}{
numeric, with a user-defined threshold used for separating low values from the rest of the signal
}
  \item{hQ}{
numeric, with a user-defined threshold used for separating high values from the rest of the signal
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
\deqn{ whl =  w_{HL} =  \left\{
      \begin{array}{c l}
        \lambda                                            & , O_i \geq hQ \\
        (1 - \lambda) + \frac{(2\lambda-1)(O_i-lQ)}{hQ-lQ} & , lQ < O_i < hQ \\
        1 - \lambda                                        & , O_i  \leq lQ\\ 
      \end{array} \right. }{%
whl = [x / max(x) ]^j }
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
\code{\link{wh}}, \code{\link{wl}}
}
\examples{
x <- 1:1000
whl(x)


##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
        
# Computing 'whl'
w <- whl(obs)
plot(obs, w, type="p")

w <- whl(obs, pbb=1)
plot(obs, w, type="p")

w <- whl(obs, k=2)
plot(obs, w, type="p")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
