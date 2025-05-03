%% File APFB.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2024-2025 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{APFB}
\Rdversion{1.1}
\alias{APFB}
\alias{APFB.default}
\alias{APFB.matrix}
\alias{APFB.data.frame}
\alias{APFB.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Annual Peak Flow Bias
}
\description{
Annual peak flow bias between \code{sim} and \code{obs}, with treatment of missing values. 

This function was prposed by Mizukami et al. (2019) to identify differences in high (streamflow) values. See Details.
}
\usage{
APFB(sim, obs, ...)

\method{APFB}{default}(sim, obs, na.rm=TRUE, start.month=1, 
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{APFB}{data.frame}(sim, obs, na.rm=TRUE, start.month=1, 
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{APFB}{matrix}(sim, obs, na.rm=TRUE, start.month=1,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
\method{APFB}{zoo}(sim, obs, na.rm=TRUE, start.month=1, 
             fun=NULL, ...,
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
  \item{start.month}{
[OPTIONAL]. Only used when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represent months in [January, December]. By default \code{start.month=1}.
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
The annual peak flow bias (APFB; Mizukami et al., 2019) is designed to drive the calibration of hydrological models focused in the reproduction of high-flow events.

The high flow bias (APFB) ranges from 0 to Inf, with an optimal value of 0. Higher values of APFB indicate stronger differences between the high values of \code{sim} and \code{obs}. Essentially, the closer to 0, the more similar the high values of \code{sim} and \code{obs} are. \cr
}
\value{
If \code{out.PerYear=FALSE}: numeric with the mean annual peak flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the  mean annual peak flow bias between each column of \code{sim} and \code{obs}.

If \code{out.PerYear=TRUE}: a list of two elements:
\item{APFB.value}{
numeric with the  mean annual peak flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the mean annual peak flow bias  between each column of \code{sim} and \code{obs}.
}
\item{APFB.PerYear}{
-) If \code{sim} and \code{obs} are not data.frame/matrix, the output is numeric, with the mean annual peak flow bias obtained for the individual years between \code{sim} and \code{obs}. 

-) If \code{sim} and \code{obs} are data.frame/matrix, this output is a data.frame, with the  mean annual peak flow bias obtained for the individual years between \code{sim} and \code{obs}. 
}
}
\references{
\cite{Mizukami, N.; Rakovec, O.; Newman, A.J.; Clark, M.P.; Wood, A.W.; Gupta, H.V.; Kumar, R.: (2019). On the choice of calibration metrics for "high-flow" estimation using hydrologic models, Hydrology Earth System Sciences 23, 2601-2614, doi:10.5194/hess-23-2601-2019.}
}
\author{
Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{NSE}}, \code{\link{wNSE}}, \code{\link{wsNSE}}, \code{\link{HFB}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: Looking at the difference between 'NSE', 'wNSE', and 'APFB'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to high flows, i.e., those equal or higher than 
# the quantile 0.9 of the observed values.
sim      <- obs
hQ.thr   <- quantile(obs, probs=0.9, na.rm=TRUE)
hQ.index <- which(obs >= hQ.thr)
hQ.n     <- length(hQ.index)
sim[hQ.index] <- sim[hQ.index] + rnorm(hQ.n, mean=mean(sim[hQ.index], na.rm=TRUE))

# Traditional Nash-Sutcliffe eficiency
NSE(sim=sim, obs=obs)

# Weighted Nash-Sutcliffe efficiency (Hundecha and Bardossy, 2004)
wNSE(sim=sim, obs=obs)

# APFB (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'APFB' for the "best" (unattainable) case
APFB(sim=sim, obs=obs)

##################
# Example 3: APFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher 
#            than the quantile 0.9 of the observed values.

sim           <- obs
hQ.thr        <- quantile(obs, probs=0.9, na.rm=TRUE)
hQ.index      <- which(obs >= hQ.thr)
hQ.n          <- length(hQ.index)
sim[hQ.index] <- sim[hQ.index] + rnorm(hQ.n, mean=mean(sim[hQ.index], na.rm=TRUE))
ggof(sim, obs)

APFB(sim=sim, obs=obs)

##################
# Example 4: APFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher  
#            than the quantile 0.9 of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

APFB(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
APFB(sim=lsim, obs=lobs)


##################
# Example 5: APFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher  
#            than the quantile 0.9 of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

APFB(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
APFB(sim=sim1, obs=obs1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
