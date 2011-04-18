\name{valindex}
\Rdversion{1.1}
\alias{valindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Valid Indexes
}
\description{
Identify the indexes that are simultaneously valid (not missing) in \code{sim} and \code{obs}.
}
\usage{
valindex(obs, sim)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{
zoo, xts, numeric, matrix or data.frame with observed values
}
  \item{sim}{
zoo, xts, numeric, matrix or data.frame with simulated values
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
