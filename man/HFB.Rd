%% File HFB.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2024-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{HFB}
\Rdversion{1.1}
\alias{HFB}
\alias{HFB.default}
\alias{HFB.matrix}
\alias{HFB.data.frame}
\alias{HFB.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Median annual high-flow bias
}
\description{

Median annual high-flow bias between \code{sim} and \code{obs}, with treatment of missing values and explicit focus on the reproduction of high-flow events.

This function is designed to identify differences in high values. See Details.
}
\usage{
HFB(sim, obs, ...)

\method{HFB}{default}(sim, obs, na.rm=TRUE, 
             hQ.thr=0.1, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{HFB}{data.frame}(sim, obs, na.rm=TRUE, 
             hQ.thr=0.1, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{HFB}{matrix}(sim, obs, na.rm=TRUE, 
             hQ.thr=0.1, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
\method{HFB}{zoo}(sim, obs, na.rm=TRUE, 
             hQ.thr=0.1, start.month=1, out.PerYear=FALSE,
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
  \item{hQ.thr}{
numeric, representing the exceedence probabiliy used to identify high flows in \code{obs}. All values in \code{obs} that are equal or higher than \code{quantile(obs, probs=(1-hQ.thr))} are considered as high flows. By default \code{hQ.thr=0.1}. \cr
On the other hand, the high values in \code{sim} are those located at the same i-th position than the i-th value of the \code{obs} deemed as high flows. 
}
  \item{start.month}{
[OPTIONAL]. Only used when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represent months in [January, December]. By default \code{start.month=1}.
}
  \item{out.PerYear}{
logical, indicating whether the output of this function has to include the median annual high-flows bias obtained for the individual years in \code{sim} and \code{obs} or not.
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

The median annual high-flow bias (HFB) is a goodness-of-fit metric designed to support the calibration and evaluation of hydrological models with specific 
emphasis on the reproduction of high-flow conditions.

The HFB ranges from 0 to 1, with an optimal value of 1. Values close to 1 indicate that the simulated high flows closely match the observed high flows,  whereas values approaching 0 indicate increasing discrepancies between the simulated and observed high-flow magnitudes.

The HFB function is inspired by the annual peak-flow bias (APFB) objective function proposed by Mizukami et al. (2019). However, it differs from that metric in four important aspects:

1) Instead of considering only the single observed annual peak flow in each year, it considers all high flows in each year, where "high flows" are defined as all values equal to or greater than a user-defined exceedance probability threshold of the observed values. By default, high flows correspond to the upper 10\% of observed flows (i.e., \code{hQ.thr=0.1}).

2) Instead of selecting simulated high flows independently of the observed events, it evaluates simulated flows occurring at the same time steps as the observed high flows, thereby preserving temporal correspondence between observed and simulated events.

3) For each year, the metric uses the median of the individual high-flow ratios rather than a single annual peak-flow value, providing a more robust summary of high-flow bias within that year.

4) When computing the final performance value, the metric uses the median of the annual values instead of the mean, reducing the influence of extreme years and improving robustness when the distribution of annual biases is asymmetric.

Mathematically, the annual high-flow performance for year \eqn{y} is defined as:

\deqn{
HFB_y = 1 - \left| \operatorname{median} \left( \frac{Q^{sim}_{y,i}}{Q^{obs}_{y,i}} \right) - 1 \right|
}

where \eqn{Q^{sim}_{y,i}} and \eqn{Q^{obs}_{y,i}} are the simulated and observed flows corresponding to the set of high-flow events \eqn{i} occurring in year \eqn{y}.

The overall HFB value is then computed as:

\deqn{
HFB = 1 - \left| \operatorname{median}(HFB_y) - 1 \right|
}

This formulation ensures that the metric is bounded between 0 and 1, with the maximum value of 1 representing perfect agreement between simulated and observed high flows.
}

\value{
If \code{out.PerYear=FALSE}: numeric with the median high flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the high flow bias between each column of \code{sim} and \code{obs}.

If \code{out.PerYear=TRUE}: a list of two elements:
\item{HFB.value}{
numeric with the median annual high flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the median annual high flow bias between each column of \code{sim} and \code{obs}.
}
\item{HFB.PerYear}{
-) If \code{sim} and \code{obs} are not data.frame/matrix, the output is numeric, with the median high flow bias obtained for the individual years between \code{sim} and \code{obs}. 

-) If \code{sim} and \code{obs} are data.frame/matrix, this output is a data.frame, with the median high flow bias obtained for the individual years between \code{sim} and \code{obs}. 
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
\code{\link{APFB}}, \code{\link{NSE}}, \code{\link{wNSE}}, \code{\link{wsNSE}}, \code{\link{JDKGE}}, \code{\link{PMR}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: Looking at the difference between 'NSE', 'wNSE', and 'HFB'
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

# HFB (Garcia et al., 2017):
HFB(sim=sim, obs=obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'HFB' for the "best" (unattainable) case
HFB(sim=sim, obs=obs)

##################
# Example 3: HFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values.

sim           <- obs
hQ.thr        <- quantile(obs, hQ.thr=0.9, na.rm=TRUE)
hQ.index      <- which(obs >= hQ.thr)
hQ.n          <- length(hQ.index)
sim[hQ.index] <- sim[hQ.index] + rnorm(hQ.n, mean=mean(sim[hQ.index], na.rm=TRUE))
ggof(sim, obs)

HFB(sim=sim, obs=obs)

##################
# Example 4: HFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

HFB(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
HFB(sim=lsim, obs=lobs)


##################
# Example 5: HFB for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

HFB(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
HFB(sim=sim1, obs=obs1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
