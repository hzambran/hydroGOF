\name{wabias}
\Rdversion{1.1}
\alias{wabias}
\alias{wabias.default}
\alias{wabias.matrix}
\alias{wabias.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Absolute Bias
}
\description{
Weighted Absolute Bias (wabias) between \code{sim} and \code{obs}, with treatment of missing values. \cr
\code{wabias} provides information about under or over-prediction of high/low values. Values close to zero indicate better model performance.
}
\usage{
wabias(sim, obs, ...)

\method{wabias}{default}(sim, obs, lambda=0.5, lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                hQ=quantile(obs, na.rm=TRUE, probs=0.95), ...)

\method{wabias}{data.frame}(sim, obs, lambda=0.5, lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                hQ=quantile(obs, na.rm=TRUE, probs=0.95), ...)

\method{wabias}{matrix}(sim, obs, lambda=0.5,lQ=quantile(obs, na.rm=TRUE, probs=0.05), 
                hQ=quantile(obs, na.rm=TRUE, probs=0.95), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{lambda}{
numeric, with the weight to be assigned to the high-flows during the computation of \code{wabias}. Low-flows will have a weight equal to \code{1-lambda}, whereas the weights to be assigned to medium flows are linearly interpolated between \code{lQ} and \code{hQ}. \cr
The default value is \code{lamda=0.5}.
}
  \item{lQ}{
numeric, low-flow separation threshold. \code{obs} values lower than \code{lQ} are considered low-flows, and then will receive a weight equal to \code{1-lambda} during the computation of \code{wabias}. \cr
The default value is \code{lQ=quantile(obs, na.rm=TRUE, probs=0.05)}.
}
  \item{hQ}{
numeric, high-flow separation threshold. \code{obs} values larger than \code{hQ} are considered high-flows, and then will receive a weight equal to \code{lambda} during the computation of \code{wabias}. \cr
The default value is \code{lQ=quantile(obs, na.rm=TRUE, probs=0.05)}.
}
  \item{na.rm}{
a logical value indicating whether \code{NA} should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%\deqn{ wabias = \sum_{i=1}^N { w * \frac{|sim_i - obs_i|}{obs_i} } 
%%\deqn{w=\left\{\begin{matrix}
%%\quad \quad \quad\lambda \quad \quad \quad \quad  , obs_i \leq lQ\\
%%1 - \lambda +  \frac{(obs_i-lQ)*(2*\lambda-1)}{hQ-lQ} \quad \quad , lQ < obs_i < hQ \\
%%\quad \quad 1-\lambda \quad \quad \quad \quad , obs_i \geq hQ\\ 
%%\end{matrix}\right}}{%
%%wabias <-  mean( w * abs(sim - obs) / obs )}
%%}
\value{
Weighted Absolute Bias between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the modified Nash-Sutcliffe efficiency between each column of \code{sim} and \code{obs}.
}
\references{
%%\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005} \cr

%%\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233-241} 
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
\code{\link{pbias}}, \code{\link{wrmse}}
}
\examples{
sim <- 1:10
obs <- 1:10
wabias(sim, obs)

sim <- 2:11
obs <- 1:10
wabias(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

lQ=4.45 # low-flows separation threshold
hQ=22.8 # high-flows separation threshold
                
# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 
        
# Computing the 'wabias' for the "best" (unattainable) case
wabias(sim=sim, obs=obs, lambda=0.9)
wabias(sim=sim, obs=obs, lambda=0.1)
        
###########################################################################
# Randomly changing all the LOW values of 'sim' (here, those smaller than 'lQ'),
# and then computing the 'wabias', with the changed low flows
sim   <- obs 
index.low <- which(sim < lQ )
sim[index.low] <- obs[index.low] + rnorm(length(index.low), mean=10)

wabias(sim=sim, obs=obs, lambda=0.9, lQ=lQ, hQ=hQ)
wabias(sim=sim, obs=obs, lambda=0.1, lQ=lQ, hQ=hQ)


# Randomly changing all the LOW values of 'sim' (here, those smaller than 'lQ'),
# and then computing the 'wabias', with the changed low flows
sim   <- obs 
sim[index.low] <- obs[index.low] - rnorm(length(index.low), mean=20)

wabias(sim=sim, obs=obs, lambda=0.9, lQ=lQ, hQ=hQ)
wabias(sim=sim, obs=obs, lambda=0.1, lQ=lQ, hQ=hQ)


########################################################################### 
# Randomly changing all the HIGH values of 'sim' (here, those larger than 'hQ',
# and then computing the 'wabias', with the changed high flows
sim        <- obs 
index.high <- which(obs > hQ )
sim[index.high] <- obs[index.high] + rnorm(length(index.high), mean=100)

wabias(sim=sim, obs=obs, lambda=0.9, lQ=lQ, hQ=hQ)
wabias(sim=sim, obs=obs, lambda=0.1, , lQ=lQ, hQ=hQ)     

# Randomly changing all the HIGH values of 'sim' (here, those larger than 'hQ',
# and then computing the 'wabias', with the changed high flows
sim <- obs 
sim[index.high] <- obs[index.high] - rnorm(length(index.high), mean=200)

wabias(sim=sim, obs=obs, lambda=0.9, lQ=lQ, hQ=hQ)
wabias(sim=sim, obs=obs, lambda=0.1, lQ=lQ, hQ=hQ)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
