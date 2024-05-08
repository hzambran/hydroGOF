%% File wNSE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2008-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{wNSE}
\Rdversion{1.1}
\alias{wNSE}
\alias{wNSE.default}
\alias{wNSE.matrix}
\alias{wNSE.data.frame}
\alias{wNSE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Nash-Sutcliffe efficiency
}
\description{
Weighted Nash-Sutcliffe efficiency between \code{sim} and \code{obs}, with treatment of missing values. \cr

This goodness-of-fit measure was proposed by Hundecha and Bardossy (2004) to put special focus on high values.
}
\usage{
wNSE(sim, obs, ...)

\method{wNSE}{default}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{wNSE}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{wNSE}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{wNSE}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ..., 
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
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the weighted Nash-Sutcliffe efficiency.

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
\deqn{ wNSE = 1 -\frac { \sum_{i=1}^N  O_i * ( S_i - O_i )^2 } { \sum_{i=1}^N  O_i * (  O_i - \bar{O}  )^2 } }{%
wNSE = 1 - ( sum( obs * ( obs - sim )^2 ) / sum( obs * ( obs - mean(obs) ) ^2 ) ) } 
}
\value{
Weighted Nash-Sutcliffe efficiency between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the relative Nash-Sutcliffe efficiency between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Nash, J.E. and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: A discussion of principles, J. Hydrol. 10 (1970), pp. 282-290. doi:10.1016/0022-1694(70)90255-6.} \cr

\cite{Hundecha, Y., Bardossy, A. (2004). Modeling of the effect of land use changes on the runoff generation of a river basin through parameter regionalization of a watershed model. Journal of hydrology, 292(1-4), 281-295. doi:10.1016/j.jhydrol.2004.01.002.}

\cite{Hundecha, Y., Ouarda, T. B., Bardossy, A. (2008). Regional estimation of parameters of a rainfall-runoff model at ungauged watersheds using the 'spatial' structures of the parameters within a canonical physiographic-climatic space. Water Resources Research, 44(1). doi:10.1029/2006WR005439.}

\cite{Hundecha, Y. and Merz, B. (2012), Exploring the Relationship between Changes in Climate and Floods Using a Model-Based Analysis, Water Resour. Res., 48(4), 1-21, doi:10.1029/2011WR010527.}.
}
\author{
sluedtke (github user)
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation \cr

If some of the observed values are equal to zero (at least one of them), this index can not be computed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NSE}}, \code{\link{rNSE}}, \code{\link{mNSE}}, \code{\link{KGE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
wNSE(sim, obs)

obs <- 1:10
sim <- 2:11
wNSE(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'wNSE' for the "best" (unattainable) case
wNSE(sim=sim, obs=obs)

##################
# Example 3: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

wNSE(sim=sim, obs=obs)

##################
# Example 4: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

wNSE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
wNSE(sim=lsim, obs=lobs)

##################
# Example 5: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

wNSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)

##################
# Example 6: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
wNSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)

##################
# Example 7: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
wNSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)

##################
# Example 8: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

wNSE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
wNSE(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
