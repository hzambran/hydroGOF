%% File ubRMSE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2024-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{ubRMSE}
\Rdversion{1.1}
\alias{ubRMSE}
\alias{ubRMSE.default}
\alias{ubRMSE.matrix}
\alias{ubRMSE.data.frame}
\alias{ubRMSE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Unbiased Root Mean Square Error
}
\description{
unbiased Root Mean Square Error (ubRMSE) between \code{sim} and \code{obs}, in the same units of \code{sim} and \code{obs}, with treatment of missing values. \cr

ubRMSE was introduced by Entekhabi et al. (2010) to improve the evaluation of the temporal dynamic of volumentric soil moisture, by removing from the traditional RMSE the mean bias error caused by the mistmatch  between the spatial representativeness of in situ soil moisture and the corresponding gridded values. \cr 

A smaller value indicates better model performance.
}
\usage{
ubRMSE(sim, obs, ...)

\method{ubRMSE}{default}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{ubRMSE}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{ubRMSE}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{ubRMSE}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ...,
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

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
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
The traditional root mean square error (RMSE) is severely compromised if there are biases in either the mean
or the amplitude of fluctuations of the simulated values. If it can be estimated reliably, the
mean-bias (BIAS) can easily be removed from RMSE, leading to the unbiased RMSE:

\deqn{ ubRMSE = \sqrt{ RMSE^2 - BIAS^2 } }{%
ubRMSE = sqrt( RMSE^2 - BIAS^2 ) } 
}
\value{
Unbiased Root mean square error (ubRMSE) between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes or data.frames, the returned value is a vector, with the ubRMSE between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Entekhabi, D., Reichle, R. H., Koster, R. D., & Crow, W. T. (2010). Performance metrics for soil moisture retrievals and application requirements. Journal of Hydrometeorology, 11(3), 832-840. doi: 10.1175/2010JHM1223.1} \cr

\cite{Ling, X., Huang, Y., Guo, W., Wang, Y., Chen, C., Qiu, B., Ge, J., Qin, K., Xue, Y., Peng, J. (2021). Comprehensive evaluation of satellite-based and reanalysis soil moisture products using in situ observations over China. Hydrology and Earth System Sciences, 25(7), 4209-4229. doi:10.5194/hess-25-4209-2021} \cr
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
\code{\link{rmse}}, \code{\link{ssq}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
ubRMSE(sim, obs)

obs <- 1:10
sim <- 2:11
ubRMSE(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'ubRMSE' for the "best" (unattainable) case
ubRMSE(sim=sim, obs=obs)

##################
# Example 3: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

ubRMSE(sim=sim, obs=obs)

##################
# Example 4: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

ubRMSE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
ubRMSE(sim=lsim, obs=lobs)

##################
# Example 5: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)

##################
# Example 6: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)

##################
# Example 7: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)

##################
# Example 8: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

ubRMSE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
ubRMSE(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
