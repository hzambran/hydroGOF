\name{wrmse}
\Rdversion{1.1}
\alias{wrmse}
\alias{wrmse.default}
\alias{wrmse.matrix}
\alias{wrmse.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Root Mean Square Error
}
\description{
Weighted Root Mean Square Error (wrmse) between \code{sim} and \code{obs}, in the same units of \code{sim} and \code{obs}, with treatment of missing values. \cr

The weighted RMSE allows placing a stronger weight on the low or on the high portions of the hydrograph, depending on the value given to \code{type}.
}
\usage{
wrmse(sim, obs, ...)

\method{wrmse}{default}(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...)

\method{wrmse}{data.frame}(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...)

\method{wrmse}{matrix}(sim, obs, type="high", j=2, pbb=0, na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{type}{
character, with the type of values that will receive the highest weights in the computation of \code{wrmse}. Valid values :
1) \kbd{high}: high values will receive the highest weights, OR \cr
2) \kbd{low} : low values will receive the highest weights.
}
  \item{j}{
numeric, with the exponent to be used in the computation of the modified Nash-Sutcliffe efficiency. The default value is \code{j=1}.
}
  \item{pbb}{
numeric, indicating how to compute the maximum value to be used in the computation of the weights to be given to the high and low values. \cr
When \code{pbb=0} the maximum value of \code{qmax} is used, whereas when pbb is within 0 and 1, the maximum value to be used corresponds to \code{x} value which is larger to a \code{pbb} fraction of the \code{x} values . The default value is \code{pbb=0}.
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

\deqn{ wrmse_{High} = \sqrt{ \frac{1}{n} * \sum_{i=1}^N { \left( S_i - O_i \right)^j * w_{High}} } } 
\deqn{ wrmse_{Low} = \sqrt{ \frac{1}{n} * \sum_{i=1}^N { \left( S_i - O_i \right)^j * w_{Low}} } } 
\deqn{ w_{High} = \sum_{i=1}^N { \left( \frac{O_i}{O_{max}} \right)^j } }
\deqn{ w_{Low}  = \sum_{i=1}^N { \left( \frac{Q_{max} - O_i}{O_{max}} \right)^j } }{%
wrmse_high = sqrt( (1/n) * w_high * sum( sim - obs)^j ) ; wrmse_low = sqrt( (1/n) * w_low * sum( sim - obs)^j ) ; w_high = (obs / max(obs))^j ; w_low = ((max(obs)-obs)/max(obs))^j  }
}
\value{
Weighted Root Mean Square Error (wrmse) between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the weighted Root Mean Square Error (wrmse) between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Fenicia, F., D. P. Solomatine, H. H. G. Savenije, and P. Matgen, Soft combination of local models in a multi-objective framework. Hydrological and Earth Systems Science, Vol. 4, pp. 91-123, 2007. DOI:10.5194/hessd-4-91-2007
}
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
\code{\link{rmse}}
}
\examples{
sim <- 1:10
obs <- 1:10
wrmse(sim, obs)

sim <- 2:11
obs <- 1:10
wrmse(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
            
# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 
        
# Computing the 'wrmse' for the "best" (unattainable) case
wrmse(sim=sim, obs=obs, type="high")
wrmse(sim=sim, obs=obs, type="low")
        
###########################################################################
# Randomly changing all the LOW values of 'sim' (here, those smaller than its median),
# and then computing the 'wrmse', with the changed low flows
sim   <- obs 
index <- which(sim < median(as.numeric(sim), na.rm=TRUE) )
sim[index] <- obs[index] + rnorm(length(index), mean=10)

wrmse(sim=sim, obs=obs, type="high", pbb=0.99)
wrmse(sim=sim, obs=obs, type="high", pbb=0.975)
wrmse(sim=sim, obs=obs, type="low", pbb=0.99)
wrmse(sim=sim, obs=obs, type="low", pbb=0.975)


# Randomly changing all the LOW values of 'sim' (here, those smaller than its median),
# and then computing the 'wrmse', with the changed low flows
sim   <- obs 
index <- which(sim < median(as.numeric(sim), na.rm=TRUE) )
sim[index] <- obs[index] + rnorm(length(index), mean=20)

wrmse(sim=sim, obs=obs, type="high", pbb=0.99)
wrmse(sim=sim, obs=obs, type="high", pbb=0.975)
wrmse(sim=sim, obs=obs, type="low", pbb=0.99)
wrmse(sim=sim, obs=obs, type="low", pbb=0.975)


########################################################################### 
# Randomly changing all the HIGH values of 'sim' (here, those larger than its median,
# and then computing the 'wrmse', with the changed high flows
sim   <- obs 
index <- which(sim > median(as.numeric(sim), na.rm=TRUE) )
sim[index] <- obs[index] + rnorm(length(index), mean=100)

wrmse(sim=sim, obs=obs, type="high", pbb=0.99)
wrmse(sim=sim, obs=obs, type="high", pbb=0.975)
wrmse(sim=sim, obs=obs, type="low", pbb=0.99)
wrmse(sim=sim, obs=obs, type="low", pbb=0.975)


sim   <- obs 
index <- which(sim > median(as.numeric(sim), na.rm=TRUE) )
sim[index] <- obs[index] + rnorm(length(index), mean=150)

wrmse(sim=sim, obs=obs, type="high", pbb=0.99)
wrmse(sim=sim, obs=obs, type="high", pbb=0.975)
wrmse(sim=sim, obs=obs, type="low", pbb=0.99)
wrmse(sim=sim, obs=obs, type="low", pbb=0.975)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
