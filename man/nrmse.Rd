% File nrmse.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{nrmse}
\Rdversion{1.1}
\alias{nrmse}
\alias{nrmse.default}
\alias{nrmse.matrix}
\alias{nrmse.data.frame}
\alias{nrmse.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalized Root Mean Square Error
}
\description{
Normalized root mean square error (NRMSE) between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
nrmse(sim, obs, ...)

\method{nrmse}{default}(sim, obs, na.rm=TRUE, norm="sd", fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

\method{nrmse}{data.frame}(sim, obs, na.rm=TRUE, norm="sd", fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

\method{nrmse}{matrix}(sim, obs, na.rm=TRUE, norm="sd", fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

\method{nrmse}{zoo}(sim, obs, na.rm=TRUE, norm="sd", fun=NULL, ..., 
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
  \item{norm}{
character, indicating the value to be used for normalising the root mean square error (RMSE). Valid values are: \cr
-) \kbd{sd}    : standard deviation of observations (default). \cr
-) \kbd{maxmin}: difference between the maximum and minimum observed values
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing this goodness-of-fit index.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. 

It is was  designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value. This is the default option.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012). 

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}
  \item{epsilon.value}{
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}
\details{
\deqn{ nrmse = 100 \frac {\sqrt{ \frac{1}{N} \sum_{i=1}^N { \left( S_i - O_i \right)^2 } } } {nval} }
\deqn{nval=  \left\{
  \begin{array}{cl}
    sd(O_i)           & , \: \textrm{norm="sd"} \\
    O_{max} - O_{min} & , \: \textrm{norm="maxmin"}    
  \end{array}
  \right.}{%
nrmse =  100 * [ rmse(sim, obs) / nval ] ; nval= range(obs, na.rm=TRUE) OR nval=sd(obs), depending on the \code{norm} value } 
}
\value{
Normalized root mean square error (nrmse) between \code{sim} and \code{obs}. The result is given in percentage (\%) \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the normalized root mean square error between each column of \code{sim} and \code{obs}.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} have to have the same length/dimension \cr

Missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pbias}}, \code{\link{pbiasfdc}}, \code{\link{mae}}, \code{\link{mse}}, \code{\link{rmse}}, \code{\link{ubRMSE}}, \code{\link{ssq}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
nrmse(sim, obs)

obs <- 1:10
sim <- 2:11
nrmse(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'nrmse' for the "best" (unattainable) case
nrmse(sim=sim, obs=obs)

##################
# Example 3: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

nrmse(sim=sim, obs=obs)

##################
# Example 4: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

nrmse(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
nrmse(sim=lsim, obs=lobs)

##################
# Example 5: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

nrmse(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
nrmse(sim=lsim, obs=lobs)

##################
# Example 6: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
nrmse(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
nrmse(sim=lsim, obs=lobs)

##################
# Example 7: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
nrmse(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
nrmse(sim=lsim, obs=lobs)

##################
# Example 8: nrmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

nrmse(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
nrmse(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
