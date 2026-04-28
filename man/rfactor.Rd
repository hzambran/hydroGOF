%% File rfactor.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2008-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{rfactor}
\Rdversion{1.1}
\alias{rfactor}
\alias{rfactor.default}
\alias{rfactor.matrix}
\alias{rfactor.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
R-factor
}
\description{
\var{R-factor} represents the average width of the given uncertainty bounds divided by the standard deviation of the observations. \cr

Ideally, i.e., with a combination of model structure and parameter values that perfectly represents the catchment under study, and in absence of measurement errors and other additional sources of uncertainty, all the simulated values should be in a perfect match with the observations, leading to a \var{P-factor} equal to 1, and an \var{R-factor} equal to zero. However, in real-world applications we aim at encompassing as much observations as possible within the given uncertainty bounds (\var{P-factor} close to 1) while keeping the width of the uncertainty bounds as small as possible (\var{R-factor} close to 0), in order to avoid obtaining a good bracketing of observations at expense of uncertainty bounds too wide to be informative for the decision-making process.
}
\usage{
rfactor(x, ...)

\method{rfactor}{default}(x, lband, uband, na.rm=TRUE, ...)

\method{rfactor}{data.frame}(x, lband, uband, na.rm=TRUE, ...)

\method{rfactor}{matrix}(x, lband, uband, na.rm=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
ts or zoo object with the observed values.
}
  \item{lband}{
numeric, ts or zoo object with the values of the lower uncertainty bound
}
  \item{uband}{
numeric, ts or zoo object with the values of the upper uncertainty bound
}
  \item{na.rm}{
logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{

The R-factor quantifies the average width of the prediction uncertainty band relative to the variability of the observed data. It is a measure of the magnitude of predictive uncertainty associated with a model simulation.

Mathematically, the R-factor is defined as:

\deqn{
R\text{-factor} = \frac{\overline{d_x}}{\sigma_x}
}

where \eqn{\sigma_x} is the standard deviation of the observed variable \eqn{x}, and \eqn{\overline{d_x}} is the average thickness of the uncertainty band, computed as:

\deqn{
\overline{d_x} = \frac{1}{N} \sum_{i=1}^{N} \left( uband_i - lband_i \right)
}

where \eqn{N} is the total number of observations, and \eqn{lband_i} and \eqn{uband_i} are the lower and upper uncertainty bounds, respectively, at time step \eqn{i}.

The \var{R-factor} ranges from 0 to infinity, with an optimal value of 0 indicating perfect agreement between simulated and observed values (i.e., zero prediction uncertainty). In practical applications, the \var{R-factor} represents the width of the uncertainty interval and should be as small as possible. Values close to or smaller than 1 are commonly considered indicative of an acceptable level of predictive uncertainty, although acceptable thresholds depend on the quality of observations and the modeling context.

Because a larger fraction of observations can often be bracketed by widening the uncertainty bounds, the \var{R-factor} is typically interpreted jointly with the \var{P-factor}. A balance between a high \var{P-factor} (good coverage) and a low \var{R-factor} (narrow uncertainty bounds) is therefore sought during model calibration and uncertainty analysis.
}
\value{
Average width of the given uncertainty bounds, given by \code{lband} and \code{uband}, divided by the standard deviation of the observations \code{x} \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the R-factor between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Abbaspour, K.C.; Faramarzi, M.; Ghasemi, S.S.; Yang, H. (2009), Assessing the impact of climate change on water resources in Iran, Water Resources Research, 45(10), W10,434, doi:10.1029/2008WR007615.}

\cite{Abbaspour, K.C., Yang, J. ; Maximov, I.; Siber, R.; Bogner, K.; Mieleitner, J. ; Zobrist, J.; Srinivasan, R. (2007), Modelling hydrology and water quality in the pre-alpine/alpine Thur watershed using SWAT, Journal of Hydrology, 333(2-4), 413-430, doi:10.1016/j.jhydrol.2006.09.014.}

\cite{Schuol, J.; Abbaspour, K.C.; Srinivasan, R.; Yang, H. (2008b), Estimation of freshwater availability in the West African sub-continent using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30, doi:10.1016/j.jhydrol.2007.12.025}

\cite{Abbaspour, K.C. (2007), User manual for SWAT-CUP, SWAT calibration and uncertainty analysis programs, 93pp, Eawag: Swiss Fed. Inst. of Aquat. Sci. and Technol. Dubendorf, Switzerland.}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
So far, the argument \code{na.rm} is not being taken into account.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pfactor}}, \code{\link{plotbands}}
}
\examples{
x <- 1:10
lband <- x - 0.1
uband <- x + 0.1
rfactor(x, lband, uband)

lband <- x - rnorm(10)
uband <- x + rnorm(10)
rfactor(x, lband, uband)

#############
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds, centred at the observations
lband <- obs - 5
uband <- obs + 5

rfactor(obs, lband, uband)

# Randomly generating the lower and upper uncertainty bounds
uband <- obs + rnorm(length(obs))
lband <- obs - rnorm(length(obs))

rfactor(obs, lband, uband)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
