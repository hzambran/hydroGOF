% File plotbandsonly.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{plotbandsonly}
\Rdversion{1.1}
\alias{plotbandsonly}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Adds uncertainty bounds to an existing plot.
}
\description{
Adds a polygon representing uncertainty bounds to an existing plot.
}
\usage{
plotbandsonly(lband, uband, dates, date.fmt="\%Y-\%m-\%d",
          bands.col="lightblue", border= NA, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lband}{
zoo or xts object with the values of the lower band.
}
  \item{uband}{
zoo or xts object with the values of the upper band.
}
  \item{dates}{
OPTIONAL. Date, factor, or character object indicating the dates that will be assigned to \code{lband} and \code{uband}. \cr
If \code{dates} is a factor or character vector, its values are converted to dates using the date format specified by \code{date.fmt}. \cr
When \code{lband} and \code{uband} are already of zoo class, \bold{the values provided by \code{dates} over-write the original dates of the objects}.
}
  \item{date.fmt}{
OPTIONAL. Character indicating the format of \code{dates}. See \code{format} in \code{\link[base]{as.Date}}.
}
  \item{bands.col}{
See \code{\link[graphics]{polygon}}. Color to be used for filling the area between the lower and upper uncertainty bound.
}
  \item{border}{
See \code{\link[graphics]{polygon}}. The color to draw the border.  The default, 'NULL', means to  use 'par("fg")'.  Use 'border = NA' to omit borders.
}
  \item{\dots}{
further arguments passed to the code{\link[graphics]{polygon}} function for plotting the bands, or from other methods
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
It requires the \pkg{hydroTSM} package
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{pfactor}}, \code{\link{rfactor}}
}
\examples{
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Selecting only the daily values belonging to the year 1961
obs <- window(obs, end=as.Date("1961-12-31"))

# Generating the lower and upper uncertainty bounds
lband <- obs - 5
uband <- obs + 5

\dontrun{
plot(obs, type="n")
plotbandsonly(lband, uband)
points(obs, col="blue", cex=0.6, type="o")
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
