%% File HFB.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2024-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{HFB}
\Rdversion{1.1}
\alias{HFB}
\alias{HFB.default}
\alias{HFB.matrix}
\alias{HFB.data.frame}
\alias{HFB.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
High-flows bias
}
\description{
High flow bias between \code{sim} and \code{obs}.

This function is designed to identify differences in high values. See Details.
}
\usage{
HFB(sim, obs, ...)

\method{HFB}{default}(sim, obs, na.rm=TRUE, 
             prob=0.9, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{HFB}{data.frame}(sim, obs, na.rm=TRUE, 
             prob=0.9, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{HFB}{matrix}(sim, obs, na.rm=TRUE, 
             prob=0.9, start.month=1, out.PerYear=FALSE,
             fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
\method{HFB}{zoo}(sim, obs, na.rm=TRUE, 
             prob=0.9, start.month=1, out.PerYear=FALSE,
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
  \item{prob}{
numeric, representing the non-exceedence probabiliy used to identify high flows in \code{obs}. All values in \code{obs} that are equal or higher than \code{quantile(obs, probs=prob)} are considered as high flows.\cr
On the other hand, the high values in \code{sim} are those located at the same i-th position than the i-th value of \code{obs} deemed as high flows. 
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
The median annual high flow bias (HFB) is designed to drive the calibration of hydrological models focused in the reproduction of high-flow events.

The HFB function is inspired in the annual peak-flow bias (APFB) objective function proposed by Mizukami et al. (2019). However, it has four important diferences:

1) instead of considering only the observed annual peak flow in each year, it considers all the high flows in each year, where "high flows" are all the values above a user-defined quantile of the observed values, by default 0.9 (\code{prob=0.9}).

2) insted of considering only the simulated high flows for each year, which might occur in a date/time different from the date in which occurs the observed annual peak flow, it considers as many high simulated flows as the number of high observed flows for each year, each one in the exact same date/time in which the corresponding observed high flow occurred.

3) for each year, instead of using a single bias value (i.e., the bias in the single annual peak flow), it uses the median of all the bias in the user-defined high flows 

4) when computing the final value of this metric, instead o using the mean of the annual values, it uses the median, in order to take a stronger representation of the bias when its distribution is not symetric.
}
\value{
The high flow bias (HFB) ranges from 0 to Inf, with an optimal value of 0. Higher values of HFB indicate stronger differences between the high values of \code{sim} and \code{obs}. Essentially, the closer to 0, the more similar the high values of \code{sim} and \code{obs} are. \cr

If \code{out.PerYear=FALSE}: numeric with the median high flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the high flow bias between each column of \code{sim} and \code{obs}.

If \code{out.PerYear=TRUE}: a list of two elements:
\item{HFB.value}{
numeric with the median annual high flow bias between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the median annual high flow bias between each column of \code{sim} and \code{obs}.
}
\item{hbf.PerYear}{
numeric with the median annual high flow bias obtained for the individual years between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, this output is a data.frame numeric with the median annual high flow bias obtained for the individual years between each column of \code{sim} and \code{obs}.
}
}
\references{
\cite{Mizukami, N., Rakovec, O., Newman, A. J., Clark, M. P., Wood, A. W., Gupta, H. V., and Kumar, R.: (2019). On the choice of calibration metrics for "high-flow" estimation using hydrologic models, Hydrology Earth System Sciences 23, 2601–2614, doi:10.5194/hess-23-2601-2019.}
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
\code{\link{apfb}}, \code{\link{NSE}}, \code{\link{wNSE}}, , \code{\link{wsNSE}}, \code{\link{gof}}, \code{\link{ggof}}
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
hQ.thr        <- quantile(obs, probs=0.9, na.rm=TRUE)
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

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
HFB(sim=sim1, obs=obs1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
