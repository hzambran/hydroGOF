%% File PMR.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2026-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{PMR}
\Rdversion{1.1}
\alias{PMR}
\alias{PMRrob}
\alias{PMR.default}
\alias{PMR.matrix}
\alias{PMR.data.frame}
\alias{PMR.zoo}

\title{
Proxy for Model Robustness
}

\description{
Proxy for model robustness (PMR) between \code{sim} and \code{obs}, with treatment of missing values.
}

\usage{
PMR(sim, obs, ...)

\method{PMR}{default}(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{PMR}{data.frame}(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{PMR}{matrix}(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{PMR}{zoo}(sim, obs, k=NULL, min.years=5, days.per.year=365,
             na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)
}

\arguments{

  \item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}

  \item{obs}{
numeric, zoo, matrix or data.frame with observed values
}

  \item{k}{
integer value representing the length of the moving window (number of time steps) used to compute the bias over sub-periods. \cr

The k argument should reflect the temporal scale at which robustness is intended to be evaluated, and therefore depends primarily on the time resolution of the data. Royer-Gaspard et al. (2021) recommended to use multi-year windows, typically in the range of 3 to 5 years, to ensure that each sub-period captures meaningful hydroclimatic variability while still allowing enough windows for comparison.
}
  \item{min.years}{
only used when the user does not explicitly define the value of \code{k}, i.e., when \code{k=NULL}. \cr

Minimum numbers of years used to ensure that each sub-period used int eh computation of PMR captures meaningful hydroclimatic variability while still allowing enough windows for comparison. By default, \code{min.years=5}.
}
  \item{days.per.year}{
only used when the user does not explicitly define the value of \code{k}, i.e., when \code{k=NULL}.  \cr

Number of days in a year. A value of Use 365.25 is recoomended instead of the default value of 365 when \code{sim} and \code{obs} are long climatological series.
}

  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}

  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the proxy for model robustness.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}

  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
}

  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{FUN}.

It was designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value. This is the default option.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012).

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the mean of the observed values. The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs} before applying \code{fun}.
}

  \item{epsilon.value}{
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values to obtain the constant to be added to both \code{sim} and \code{obs} before applying \code{fun}.
}

}

\details{

\deqn{
PMR = 2 \times \frac{1}{N}
\sum_{i=1}^{N}
\left|
(\bar{S}_i - \bar{O}_i)
-
(\bar{S} - \bar{O})
\right|
\frac{1}{\bar{O}}
}{
PMR = 2 * mean( abs( moving.bias - mean.bias ) ) / mean(obs)
}

where:

\itemize{

\item \eqn{\bar{S}_i} and \eqn{\bar{O}_i} are the mean simulated and observed values computed over the i-th moving window of length \code{k},

\item \eqn{\bar{S}} and \eqn{\bar{O}} are the overall mean simulated and observed values,

\item \eqn{N} is the number of moving windows.
}

The proxy for model robustness (PMR) is a dimensionless statistic that quantifies the temporal stability of model bias by measuring the average deviation of sub-period bias from the overall bias.

PMR indicates how consistent the model performance is across different time periods or hydrological conditions.

The proxy for model robustness ranges from 0 to positive infinity. Essentially, the closer to 0, the more temporally robust the model is. \cr

-) PMR = 0 corresponds to a perfectly robust model, in which the model bias is identical across all sub-periods. \cr

-) 0 < PMR < 1 indicates relatively stable model performance with moderate temporal variability in bias. \cr

-) PMR > 1 indicates increasing variability in model bias across time periods, suggesting reduced robustness of model performance.
}

\value{

Proxy for model robustness between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the proxy for model robustness between each column of \code{sim} and \code{obs}.
}

\references{

\cite{
Royer-Gaspard, P., Thirel, G., Andreassian, V., Perrin, C., and Coron, L. (2021). A robust and efficient framework for evaluating hydrological model robustness. Hydrology and Earth System Sciences, 25, 5703--5726. doi:10.5194/hess-25-5703-2021. }

\cite{
Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171--182. doi:10.1016/j.jhydrol.2011.11.055.}

}

\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}

\note{

\code{obs} and \code{sim} have to have the same length/dimension. \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation.

The choice of window length \code{k} influences the temporal scale at which robustness is evaluated.
}

\seealso{

\code{\link{PBIAS}}, \code{\link{NSE}}, \code{\link{VE}}, \code{\link{JDKGE}}, \code{\link{gof}}, \code{\link{ggof}}

}

\examples{

##################
# Example 1: ideal case
obs <- 1:100
sim <- 1:100

PMR(sim, obs, k=10)

##################
# Example 2:
# Simulated values with systematic bias in the second half

set.seed(123)

obs <- 1:100
sim <- obs

sim[51:100] <- sim[51:100] + 5

PMR(sim, obs, k=10)

##################
# Example 3:
# Applying logarithmic transformation

PMR(sim, obs, k=10, fun=log)

##################
# Example 4:
# Using Pushpalatha2012 epsilon

PMR(sim, obs, k=10, fun=log, epsilon.type="Pushpalatha2012")

##################
# Example 5:
# Matrix input (multi-site case)

obs <- cbind(site1=1:100, site2=1:100)

sim <- obs
sim[51:100,2] <- sim[51:100,2] + 10

PMR(sim, obs, k=10)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }