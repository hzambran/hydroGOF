% File NSE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2011-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{NSE}
\Rdversion{1.1}
\alias{NSeff}
\alias{NSE}
\alias{NSE.default}
\alias{NSE.matrix}
\alias{NSE.data.frame}
\alias{NSE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nash-Sutcliffe Efficiency
}
\description{
Nash-Sutcliffe efficiency between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
NSE(sim, obs, ...)

\method{NSE}{default}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{NSE}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{NSE}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{NSE}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
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
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Nash-Sutcliffe efficiency.

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

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}
  \item{epsilon.value}{
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}
\details{
\deqn{ NSE = 1 -\frac { \sum_{i=1}^N { \left( S_i - O_i \right)^2 } } { \sum_{i=1}^N { \left( O_i - \bar{O} \right)^2 } }  }{%
NSE = 1 - ( sum( (obs - sim)^2 ) / sum( (obs - mean(obs))^2 ) }

The Nash-Sutcliffe efficiency (NSE) is a normalized statistic that determines the relative magnitude of the residual variance ("noise") compared to the measured data variance ("information") (Nash and Sutcliffe, 1970). \cr

NSE indicates how well the plot of observed versus simulated data fits the 1:1 line.\cr

Nash-Sutcliffe efficiencies range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. \cr
-) NSE = 1, corresponds to a perfect match of modelled to the observed data. \cr
-) NSE = 0, indicates that the model predictions are as accurate as the mean of the observed data, \cr
-) -Inf < NSE < 0, indicates that the observed mean is better predictor than the model.
}
\value{
Nash-Sutcliffe efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Nash-Sutcliffe efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\url{https://en.wikipedia.org/wiki/Nash\%E2\%80\%93Sutcliffe_model_efficiency_coefficient} \cr

\cite{Nash, J.E. and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282-290. doi:10.1016/0022-1694(70)90255-6} \cr

\cite{Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi:10.1002/hyp.7072}

\cite{Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019} \cr

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055}  
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
\code{\link{mNSE}}, \code{\link{rNSE}}, \code{\link{wNSE}}, \code{\link{KGE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
NSE(sim, obs)

obs <- 1:10
sim <- 2:11
NSE(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
NSE(sim=sim, obs=obs)

##################
# Example 3: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

NSE(sim=sim, obs=obs)

##################
# Example 4: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

NSE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
NSE(sim=lsim, obs=lobs)

##################
# Example 5: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

NSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)

##################
# Example 6: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
NSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)

##################
# Example 7: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
NSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)

##################
# Example 8: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

NSE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
NSE(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
