%% File wsNSE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2024-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{wsNSE}
\Rdversion{1.1}
\alias{wsNSE}
\alias{wsNSE.default}
\alias{wsNSE.matrix}
\alias{wsNSE.data.frame}
\alias{wsNSE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted seasonal Nash-Sutcliffe Efficiency
}
\description{
Weighted seasonal Nash-Sutcliffe Efficiency between \code{sim} and \code{obs}, with treatment of missing values.  

This function is designed to identify differences in high or low values, depending on the user-defined value given to the \code{lambda} argument. See Usage and Details.
}
\usage{
wsNSE(sim, obs, ...)

\method{wsNSE}{default}(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{wsNSE}{data.frame}(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{wsNSE}{matrix}(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)
             
\method{wsNSE}{zoo}(sim, obs, na.rm=TRUE, 
              j=2, lambda=0.95, lQ.thr=0.6, hQ.thr=0.1, fun=NULL, ..., 
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
  \item{j}{
numeric, representing an arbitrary value used to power the differences between observations and simulations. By default \code{j=2}, which mimics the traditional Nash-Sutcliffe function, mainly focused on thr representation of high values. For low flows, suggested values for \code{j} are 1, 1/2 or 1/3. See Legates and McCabe, (1999) and Krausse et al. (2005) for a discussion of suggested values of \code{j}. 
}
  \item{lambda}{
numeric in [0, 1] representing the weight given to the high observed values. The closer the \code{lambda=1} value is to 1, the higher the weight given to high values. On the contrary, the closer the \code{lambda=1} value is to 0, the higher the weight given to low values.

Low values get a weight equal to \code{1-lambda}. Between high and low values there is a linear transition from \code{lambda} to \code{1-lambda}, respectively.
}
  \item{lQ.thr}{
numeric, representing the non-exceedence probabiliy used to identify low flows in \code{obs}. All values in \code{obs} that are equal or lower than \code{quantile(obs, probs=(1-lQ.thr))} are considered as low values. By default \code{lQ.thr=0.6}. 

On the other hand, the low values in \code{sim} are those located at the same i-th position than the i-th value of the \code{obs} deemed as low flows.  
}
  \item{hQ.thr}{
numeric, representing the non-exceedence probabiliy used to identify high flows in \code{obs}. All values in \code{obs} that are equal or higher than \code{quantile(obs, probs=(1-hQ.thr))} are considered as high flows. By default \code{hQ.thr=0.1}. 

On the other hand, the high values in \code{sim} are those located at the same i-th position than the i-th value of the \code{obs} deemed as high flows.  
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
The weighted seasonal Nash-Sutcliffe Efficiency was proposed by Zambrano-Bigiarini and Bellin (2012), inspired by the well-known Nash-Sutcliffe efficiency (NSE, Nash and Sutcliffe, 1970), and the  commentaries made by Schaefli and Gupta (2007) and Criss and Winston (2008).
 
This function gives different weights to the high/low values in the (obs_i - sim_i) terms used in the Nash-Sutcliffe formula, using high weights for high or low flows, depending on how close the user-defined 'lambda' value is to 1 or zero, respectively. Between  high and low values there is a linear transition from \code{lambda} to \code{1-lambda}, respectively.

Following the traditional Nash-Sutcliffe efficiency, the weighted seasonal Nash-Sutcliffe Efficiency (wsNSE) ranges from -Inf to 1, with an optimal value of 1. Higher values of wsNSE indicate lower differences between \code{sim} and \code{obs}. Essentially, the closer to 1, the more similar\code{sim} and \code{obs} are.
}
\value{
numeric with the the weighted seasonal Nash-Sutcliffe Efficiency (wsNSE) between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the the weighted seasonal Nash-Sutcliffe Efficiency (wsNSE) between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Zambrano-Bigiarini, M.; Bellin, A. (2012). Comparing goodness-of-fit measures for calibration of models focused on extreme events. EGU General Assembly 2012, Vienna, Austria, 22-27 Apr 2012, EGU2012-11549-1.}

\cite{Nash, J.E.; J.V. Sutcliffe. (1970). River flow forecasting through conceptual models. Part 1: a discussion of principles, Journal of Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6.}

\cite{Schaefli, B.; Gupta, H. (2007). Do Nash values have value?. Hydrological Processes 21, 2075-2080. doi:10.1002/hyp.6825.}

\cite{Criss, R. E.; Winston, W. E. (2008), Do Nash values have value?. Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi:10.1002/hyp.7072.}

\cite{Yilmaz, K. K.; Gupta, H. V.; Wagener, T. (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resources Research, 44, W09417, doi:10.1029/2007WR006716.}

\cite{Krause, P.; Boyle, D. P.; Base, F. (2005). Comparison of different efficiency criteria for hydrological model assessment. Adv. Geosci., 5, 89-97. doi:10.5194/adgeo-5-89-2005.}

\cite{Legates, D. R.; G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation. Water Resources Research, 35(1), 233-241. doi:10.1029/1998WR900018.}
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
\code{\link{NSE}}, \code{\link{wNSE}}, \code{\link{wsNSE}}, \code{\link{APFB}}, \code{\link{KGElf}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: Looking at the difference between 'KGE', 'NSE', 'wNSE', 'wsNSE',
# 'APFB' and 'KGElf' for detecting differences in high flows

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

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)

# Weighted Nash-Sutcliffe efficiency (Hundecha and Bardossy, 2004)
wNSE(sim=sim, obs=obs)

# wsNSE (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs)

# APFB (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs)


##################
# Example 2: Looking at the difference between 'KGE', 'NSE', 'wsNSE',
# 'dr', 'rd', 'md', 'APFB' and 'KGElf' for detecting differences in low flows

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to low flows, i.e., those equal or lower than 
# the quantile 0.4 of the observed values.
sim      <- obs
lQ.thr   <- quantile(obs, probs=0.4, na.rm=TRUE)
lQ.index <- which(obs <= lQ.thr)
lQ.n     <- length(lQ.index)
sim[lQ.index] <- sim[lQ.index] + rnorm(lQ.n, mean=mean(sim[lQ.index], na.rm=TRUE))

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)

# Weighted seasonal Nash-Sutcliffe efficiency (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs)

# Refined Index of Agreement (Willmott et al., 2012):
dr(sim=sim, obs=obs)

# Relative Index of Agreement (Krause et al., 2005):
rd(sim=sim, obs=obs)

# Modified Index of Agreement (Krause et al., 2005):
md(sim=sim, obs=obs)

# Annual Peak Flow Bias (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs)


##################
# Example 3: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'wsNSE' for the "best" (unattainable) case
wsNSE(sim=sim, obs=obs)


##################
# Example 4: wsNSE for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

wsNSE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
wsNSE(sim=lsim, obs=lobs)


##################
# Example 5: wsNSE for simulated values created equal to the observed values and then 
#            random noise is added only to high flows, i.e., those equal or higher than 
#            the quantile 0.9 of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

wsNSE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
wsNSE(sim=sim1, obs=obs1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
