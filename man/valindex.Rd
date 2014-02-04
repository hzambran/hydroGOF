% File valindex.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{valindex}
\Rdversion{1.1}
\alias{valindex}
\alias{valindex.default}
\alias{valindex.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Valid Indexes
}
\description{
Identify the indexes that are simultaneously valid (not missing) in \code{sim} and \code{obs}.
}
\usage{
valindex(sim, obs, ...)

\method{valindex}{default}(sim, obs, ...)

\method{valindex}{matrix}(sim, obs, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sim}{
zoo, xts, numeric, matrix or data.frame with simulated values
}
  \item{obs}{
zoo, xts, numeric, matrix or data.frame with observed values
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
A vector with the indexes that are simultaneously valid (not missing) in \code{obs} and \code{sim}.
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano Bigiarini <mauricio.zambrano@ing.unitn.it>
}
\note{
This function is used in the functions of this package for removing missing values from the observed and simulated time series.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{is.na}}, \code{\link{which}}
}
\examples{
sim <- 1:5
obs <- c(1, NA, 3, NA, 5)
valindex(sim, obs)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
