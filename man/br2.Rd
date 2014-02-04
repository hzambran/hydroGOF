%% File br2.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or lat

\name{br2}
\Rdversion{1.1}
\alias{br2}
\alias{br2.default}
\alias{br2.matrix}
\alias{br2.data.frame}
\alias{br2.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
br2
}
\description{
Coefficient of determination (r2) multiplied by the slope of the regression line between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
br2(sim, obs, ...)

\method{br2}{default}(sim, obs, na.rm=TRUE, ...)

\method{br2}{data.frame}(sim, obs, na.rm=TRUE, ...)

\method{br2}{matrix}(sim, obs, na.rm=TRUE, ...)

\method{br2}{zoo}(sim, obs, na.rm=TRUE, ...)
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
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
\deqn{ br2 = |b| R2 , |b| <= 1  ; br2 = \frac{R2}{|b|}, b > 1  }{%
br2 = abs(b)*R2, abs(b) <= 1; br2= R2/abs(b), abs(b) > 1 } 

A model that systematically over or under-predicts all the time will still result in "good" \code{r2} (close to 1), even if all predictions were wrong (Krause et al., 2005).
The \code{br2} coefficient allows accounting for the discrepancy in the magnitude of two signals (depicted by 'b') as well as their dynamics (depicted by \code{r2})
}
\value{
br2 between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the \code{br2} between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Krause, P., Boyle, D. P., and Base, F.: Comparison of different efficiency criteria for hydrological model assessment, Adv. Geosci., 5, 89-97, 2005}

}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation \cr

The slope \code{b} is computed as the coefficient of the linear regression between \code{sim} and \code{obs}, forcing the intercept be equal to zero.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[stats]{cor}}, \code{\link[stats]{lm}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
# Looking at the difference between r2 and br2 for a case with systematic 
# over-prediction of observed values
obs <- 1:10
sim1 <- 2*obs + 5
sim2 <- 2*obs + 25

# The coefficient of determination is equal to 1 even if there is no one single 
# simulated value equal to its corresponding observed counterpart
r2 <- (cor(sim1, obs, method="pearson"))^2 # r2=1

# 'br2' effectively penalises the systematic over-estimation
br2(sim1, obs) # br2 = 0.3684211
br2(sim2, obs) # br2 = 0.1794872

ggof(sim1, obs)
ggof(sim2, obs)

# Computing 'br2' without forcing the intercept be equal to zero
br2.2 <- r2/2 # br2 = 0.5

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing 'br2' for the "best" (unattainable) case
br2(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new  'br2'
br2(sim=sim, obs=obs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
