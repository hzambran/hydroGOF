\name{KGE}
\Rdversion{1.1}
\alias{KGE}
\alias{KGE.default}
\alias{KGE.matrix}
\alias{KGE.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kling-Gupta Efficiency
}
\description{
Kling-Gupta efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This goodness-of-fit measure was developed by Gupta et al. (2009) to provide a diagnostically interesting decomposition of the Nash-Sutcliffe efficiency (and hence MSE), which facilitates the analysis of the relative importance of its different components in the context of hydrological modelling".

In the computation of this index, there are three main components involved: \cr
-) \code{r}    : the Pearson product-moment correlation coefficient. Ideal value is r=1. \cr
-) \code{Alpha}: the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Ideal value is Alpha=1. \cr
-) \code{Beta} : the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1. \cr

In order to fully understand this index, please read the reference.
}
\usage{
KGE(sim, obs, ...)

\method{KGE}{default}(sim, obs, s=c(1,1,1), method=c("2009", "2012"), na.rm=TRUE, ...)

\method{KGE}{data.frame}(sim, obs, s=c(1,1,1),  method=c("2009", "2012"), na.rm=TRUE, ...)

\method{KGE}{matrix}(sim, obs, s=c(1,1,1),  method=c("2009", "2012"), na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}
  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}
  \item{s}{
numeric of length 3, representing the scaling factors to be used for re-scaling the criteria space before computing the Euclidean distance from the ideal point c(1,1,1), i.e., \code{s} elements are used for adjusting the emphasis on different components.
The first elements is used for rescaling the Pearson product-moment correlation coefficient (\code{r}), the second element is used for rescaling \code{Alpha} and the third element is used for re-scaling \code{Beta}
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
\deqn{KGE = 1 - ED}
\deqn{ ED =  \sqrt{ (s[1]*(r-1))^2 +(s[2]*(\alpha-1))^2 + (s[3]*(\beta-1))^2 } }
\deqn{\alpha=\sigma_s/\sigma_o}
\deqn{\beta=\mu_s/\mu_o}{%
KGE = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(Alpha-1))^2 + (s[3]*(Beta-1))^2] }

Kling-Gupta efficiencies range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. \cr
}
\value{
Kling-Gupta efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694}

\cite{Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424-425, 264-277, DOI:10.1016/j.jhydrol.2012.01.011}
}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NSE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
obs <- 1:10
sim <- 1:10
KGE(sim, obs)

obs <- 1:10
sim <- 2:11
KGE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
require(zoo)
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'KGE' for the "best" (unattainable) case
KGE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'KGE'
KGE(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
