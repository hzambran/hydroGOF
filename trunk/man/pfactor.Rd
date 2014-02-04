% File pfactor.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{pfactor}
\Rdversion{1.1}
\alias{pfactor}
\alias{pfactor.default}
\alias{pfactor.matrix}
\alias{pfactor.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
P-factor
}
\description{
\var{P-factor} is the percent of observations that are within the given uncertainty bounds. \cr

Ideally, i.e., with a combination of model structure and parameter values that perfectly represents the catchment under study, and in absence of measurement errors and other additional sources of uncertainty, all the simulated values should be in a perfect match with the observations, leading to a \var{P-factor} equal to 1, and an \var{R-factor} equal to zero. However, in real-world applications we aim at encompassing as much observations as possible within the given uncertainty bounds (\var{P-factor} close to 1) while keeping the width of the uncertainty bounds as small as possible (\var{R-factor} close to 0), in order to avoid obtaining a good bracketing of observations at expense of uncertainty bounds too wide to be informative for the decision-making process.
}
\usage{
pfactor(x, ...)

\method{pfactor}{default}(x, lband, uband, na.rm=TRUE, ...)

\method{pfactor}{data.frame}(x, lband, uband, na.rm=TRUE, ...)

\method{pfactor}{matrix}(x, lband, uband, na.rm=TRUE, ...)
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
a logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%\deqn{ pfactor = 1 -\frac { \sum_{i=2}^N { \left( S_i - O_i \right)^2 } } { \sum_{i=1}^{N-1} { \left( O_{i+1} - O_i \right)^2 } }  }{%
%%cp = 1 - [ sum( (obs[2:n] - sim[2:n])^2 ] / sum( ( obs[2:n] - obs[1:(n-1)] )^2 ) }
%%}
\value{
Percent of the \code{x} observations that are within the given uncertainty bounds given by \code{lband} and \code{uband}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the \var{P-factor} between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Abbaspour, K. C., M. Faramarzi, S. S. Ghasemi, and H. Yang (2009), Assessing the impact of climate change on water resources in Iran, Water Resour. Res., 45(10), W10,434, doi:10.1029/2008WR007615} \cr

\cite{Abbaspour, K. C., J. Yang, I. Maximov, R. Siber, K. Bogner, J. Mieleitner, J. Zobrist, and R. Srinivasan (2007), Modelling hydrology and water quality in the pre-alpine/alpine Thur watershed using SWAT, Journal of Hydrology, 333(2-4), 413-430, doi:10.1016/j.jhydrol.2006.09.014} \cr

\cite{Schuol, J., K. Abbaspour, R. Srinivasan, and H. Yang (2008b), Estimation of freshwater availability in the West African sub-continent using the SWAT hydrologic model, Journal of Hydrology, 352(1-2), 30, doi:10.1016/j.jhydrol.2007.12.025} \cr

\cite{Abbaspour, C., Karim (2007), User manual for SWAT-CUP, SWAT calibration and uncertainty analysis programs, 93pp, Eawag: Swiss Fed. Inst. of Aquat. Sci. and Technol. Dubendorf, Switzerland, Available at \url{http://www.eawag.ch/organisation/abteilungen/siam/software/swat/index_EN}}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
So far, the argument \code{na.rm} is not being taken into account.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{rfactor}}, \code{\link{plotbands}}
}
\examples{
x <- 1:10
lband <- x - 0.1
uband <- x + 0.1
pfactor(x, lband, uband)

lband <- x - rnorm(10)
uband <- x + rnorm(10)
pfactor(x, lband, uband)

#############
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds, centred at the observations
lband <- obs - 5
uband <- obs + 5

pfactor(obs, lband, uband)

# Randomly generating the lower and upper uncertainty bounds
uband <- obs + rnorm(length(obs))
lband <- obs - rnorm(length(obs))

pfactor(obs, lband, uband)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
