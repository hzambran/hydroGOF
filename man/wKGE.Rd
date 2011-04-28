\name{wKGE}
\Rdversion{1.1}
\alias{wKGE}
\alias{wKGE.default}
\alias{wKGE.matrix}
\alias{wKGE.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Kling-Gupta Efficiency
}
\description{
Weighted Kling-Gupta efficiency (wKGE) between \code{sim} and \code{obs}, with treatment of missing values. \cr
%\code{wKGE} provides information about under or over-prediction of high/low values. Values close to zero indicate better model performance.
}
\usage{
wKGE(sim, obs, ...)

\method{wKGE}{default}(sim, obs, s=c(1,1,1), j=0.5, w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...)

\method{wKGE}{data.frame}(sim, obs, s=c(1,1,1), j=0.5, w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...)

\method{wKGE}{matrix}(sim, obs, s=c(1,1,1), j=0.5, w="wl", k=0.5, pbb=0.8, 
                       lambda=0.5, lQ=quantile(obs, na.rm=TRUE, probs=0.3), 
                       hQ=quantile(obs, na.rm=TRUE, probs=0.8), na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, xts, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, xts, matrix or data.frame with observed values
}
  \item{s}{
numeric of length 3, representing the scaling factors to be used for re-scaling the criteria space before computing the Euclidean distance from the ideal point c(1,1,1), i.e., \code{s} elements are used for adjusting the emphasis on different components.
The first elements is used for rescaling the Pearson product-moment correlation coefficient (\code{r}), the second element is used for rescaling \code{Alpha} and the third element is used for re-scaling \code{Beta}
}
  \item{j}{
numeric, representing an arbitrary power (positive number) to be used in the computation of the numerator and denominator of \code{wKGE}.
}
  \item{w}{
character, indicating the weighting function to be used in the computation of \code{wKGE}. Valid values are: \cr
1) \kbd{wl} : weighting function to be used when focusing on low values (see \code{\link{wl}}), OR \cr
2) \kbd{wh} : weighting function to be used when focusing on high values (see \code{\link{wh}}), OR \cr
3) \kbd{whl}: weighting function to be used when focusing on high or low values, depending on the value of \code{whl} (see \code{\link{NSeff}}).
}
  \item{k}{
numeric, representing an arbitrary power (positive number) to be used in the computation of \code{wl} and \code{wh}.
}
  \item{pbb}{
numeric with values in [0,1], representing the quantile used to compute the maximum value (\kbd{hQ}) involved in the computation of \code{\link{wh}} and \code{\link{wl}}. See the argument \kbd{probs} in \code{\link[stats]{quantile}}.
}
  \item{lambda}{
numeric, with the weight to be assigned to the high-flows during the computation of \code{wKGE}. Low-flows will have a weight equal to \code{1-lambda}, whereas the weights to be assigned to medium flows are linearly interpolated between \code{lQ} and \code{hQ}. \cr
The default value is \code{lambda=0.5}.
}
  \item{lQ}{
numeric, low-flow separation threshold. \code{obs} values lower than \code{lQ} are considered low-flows, and then will receive a weight equal to \code{1-lambda} during the computation of \code{wKGE}. \cr
The default value is \code{lQ=quantile(obs, na.rm=TRUE, probs=0.2)}.
}
  \item{hQ}{
numeric, high-flow separation threshold. \code{obs} values larger than \code{hQ} are considered high-flows, and then will receive a weight equal to \code{lambda} during the computation of \code{wKGE}. \cr
The default value is \code{hQ=quantile(obs, na.rm=TRUE, probs=0.8)}.
}
  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%\deqn{ wKGE = \sum_{i=1}^N { w * \frac{|sim_i - obs_i|}{obs_i} } 
%%\deqn{w=\left\{\begin{matrix}
%%\quad \quad \quad\lambda \quad \quad \quad \quad  , obs_i \leq lQ\\
%%1 - \lambda +  \frac{(obs_i-lQ)*(2*\lambda-1)}{hQ-lQ} \quad \quad , lQ < obs_i < hQ \\
%%\quad \quad 1-\lambda \quad \quad \quad \quad , obs_i \geq hQ\\ 
%%\end{matrix}\right}}{%
%%wKGE <-  mean( w * abs(sim - obs) / obs )}
%%}
\value{
Weighted Kling-Gupta efficiency between \code{sim} and \code{obs}. \cr

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
\code{\link{NSeff}}, \code{\link{mNSeff}}, \code{\link{rNSeff}}, \code{\link{rmse}}
}
\examples{
sim <- 1:10
obs <- 1:10
wKGE(sim, obs)

sim <- 2:11
obs <- 1:10
wKGE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

lQ=4.45 # low-flows separation threshold
hQ=22.8 # high-flows separation threshold
                
# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 
        
# Computing the 'wKGE' for the "best" (unattainable) case
wKGE(sim=sim, obs=obs, type="low", j=0.5, pbb=0.8)
wKGE(sim=sim, obs=obs, type="high", j=2, pbb=0.8)
        
###########################################################################
# Randomly changing all the LOW values of 'sim' (here, those smaller than 'lQ'),
# and then computing the 'wKGE', with the changed low flows
sim   <- obs 
index.low <- which(sim < lQ )
sim[index.low] <- obs[index.low] + rnorm(length(index.low), mean=10)

wKGE(sim=sim, obs=obs, type="low", j=0.5, pbb=0.8)
wKGE(sim=sim, obs=obs, type="high", j=2, pbb=0.8)


# Randomly changing all the LOW values of 'sim' (here, those smaller than 'lQ'),
# and then computing the 'wKGE', with the changed low flows
sim   <- obs 
sim[index.low] <- obs[index.low] - rnorm(length(index.low), mean=20)

wKGE(sim=sim, obs=obs, type="low", j=0.5, pbb=0.8)
wKGE(sim=sim, obs=obs, type="high", j=2, pbb=0.8)


########################################################################### 
# Randomly changing all the HIGH values of 'sim' (here, those larger than 'hQ',
# and then computing the 'wKGE', with the changed high flows
sim        <- obs 
index.high <- which(obs > hQ )
sim[index.high] <- obs[index.high] + rnorm(length(index.high), mean=100)

wKGE(sim=sim, obs=obs, type="low", j=0.5, pbb=0.8)
wKGE(sim=sim, obs=obs, type="high", j=2, pbb=0.8)     

# Randomly changing all the HIGH values of 'sim' (here, those larger than 'hQ',
# and then computing the 'wKGE', with the changed high flows
sim <- obs 
sim[index.high] <- obs[index.high] - rnorm(length(index.high), mean=200)

wKGE(sim=sim, obs=obs, type="low", j=0.5, pbb=0.8)
wKGE(sim=sim, obs=obs, type="high", j=2, pbb=0.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
