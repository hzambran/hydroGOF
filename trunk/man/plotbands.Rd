% File plotbands.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{plotbands}
\Rdversion{1.1}
\alias{plotbands}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a ts with observed values and two confidence bounds 
}
\description{
It plots a ts with observed values and two confidence bounds. Optionally can also add a simulated time series, in order to be compared with 'x'.
}
\usage{
plotbands(x, lband, uband, sim, 
          dates, date.fmt="\%Y-\%m-\%d",
          gof.leg= TRUE, gof.digits=2, 
          legend=c("Obs", "Sim", "95PPU"), leg.cex=1,
          bands.col="lightblue", border= NA,
          tick.tstep= "auto", lab.tstep= "auto", lab.fmt=NULL,
          cal.ini=NA, val.ini=NA, 
          main="Confidence Bounds for 'x'", 
          xlab="Time", ylab="Q, [m3/s]", ylim,
          col=c("black", "blue"), type= c("lines", "lines"),
          cex= c(0.5, 0.5), cex.axis=1.2, cex.lab=1.2,          
          lwd=c(0.6, 1), lty=c(3, 4), pch=c(1,9), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
zoo or xts object with the observed values.
}
  \item{lband}{
zoo or xts object with the values of the lower band.
}
  \item{uband}{
zoo or xts object with the values of the upper band.
}
  \item{sim}{
OPTIONAL. zoo or xts object with the simulated values.
}
  \item{dates}{
OPTIONAL. Date, factor, or character object indicating the dates that will be assigned to \code{x}, \code{lband}, \code{uband}, and \code{sim} (when provided). \cr
If \code{dates} is a factor or character vector, its values are converted to dates using the date format specified by \code{date.fmt}. \cr
When \code{x}, \code{lband}, \code{uband}, and \code{sim} are already of zoo class, \bold{the values provided by \code{dates} over-write the original dates of the objects}.
}
  \item{date.fmt}{
OPTIONAL. Character indicating the format in which the dates entered are stored in \code{cal.ini} and \code{val.ini}. See \code{format} in \code{\link[base]{as.Date}}.\cr
Default value is \kbd{\%Y-\%m-\%d} \cr
ONLY required when \code{cal.ini}, \code{val.ini} or \code{dates} is provided.
}
  \item{gof.leg}{
logical indicating if the p-factor and r-factor have to be computed and plotted as legends on the graph.
}
  \item{gof.digits}{
OPTIONAL, numeric. Only used when \code{gof.leg=TRUE}. Decimal places used for rounding the goodness-of-fit indexes
}
  \item{legend}{
OPTIONAL. logical or character vector of length 3 with the strings that will be used for the legend of the plot. \cr
-) When \code{legend} is a character vector, the first element is used for labelling the observed series, the second for labelling the simulated series and the third one for the predictive uncertainty bounds. Default value is \code{c("obs", "sim", "95PPU")} \cr
-) When \code{legend=FALSE}, the legend is not drawn.
}
  \item{leg.cex}{
OPTIONAL. numeric. Used for the GoF legend. Character expansion factor *relative* to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex'. Default value is 1.
}
  \item{bands.col}{
See \code{\link[graphics]{polygon}}. Color to be used for filling the area between the lower and upper uncertainty bound.
}
  \item{border}{
See \code{\link[graphics]{polygon}}. The color to draw the border.  The default, 'NULL', means to  use 'par("fg")'.  Use 'border = NA' to omit borders.
}
  \item{tick.tstep}{
character, indicating the time step that have to be used for putting the ticks on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
  \item{lab.tstep}{
character, indicating the time step that have to be used for putting the labels on the time axis. Valid values are: \kbd{auto}, \kbd{years}, \kbd{months},\kbd{weeks}, \kbd{days}, \kbd{hours}, \kbd{minutes}, \kbd{seconds}.
}
  \item{lab.fmt}{
Character indicating the format to be used for the label of the axis. See \code{lab.fmt} in \code{\link[hydroTSM]{drawTimeAxis}}.
}
 \item{cal.ini}{
OPTIONAL. Character with the date in which the calibration period started. \cr
ONLY used for drawing a vertical red line at this date. 
}
  \item{val.ini}{
OPTIONAL. Character with the date in which the validation period started. \cr
ONLY used for drawing a vertical red line at this date. 
}
  \item{main}{
an overall title for the plot: see 'title' 
}
  \item{xlab}{
a title for the x axis: see 'title'
}
  \item{ylab}{
a title for the y axis: see 'title'
}
  \item{ylim}{
the y limits of the plot. See \code{\link[graphics]{plot.default}}.
}
  \item{col}{
colors to be used for plotting the \code{x} and \code{sim} ts.
}
  \item{type}{
character.  Indicates if the observed and simulated series have to be plotted as lines or points. Possible values are: \cr
-) \kbd{lines} : the observed/simulated series are plotted as lines \cr
-) \kbd{points}: the observed/simulated series are plotted as points
}
  \item{cex}{
See code {\link[graphics]{plot.default}}. A numerical vector giving the amount by which plotting characters and symbols should be scaled relative to the default.  \cr
This works as a multiple of 'par("cex")'. 'NULL' and 'NA' are equivalent to '1.0'.  Note that this does not affect annotation.
}
  \item{cex.axis}{
magnification of axis annotation relative to 'cex'.
}
  \item{cex.lab}{
Magnification to be used for x and y labels relative to the current setting of 'cex'. See '?par'.
}
  \item{lwd}{
See \code{\link[graphics]{plot.default}}. The line width, see 'par'.
}
  \item{lty}{
See \code{\link[graphics]{plot.default}}. The line type, see 'par'. 
}
  \item{pch}{
numeric, with the type of symbol for \code{x} and \code{y}. (e.g.: 1: white circle; 9: white rhombus with a cross inside)
}
  \item{\dots}{
further arguments passed to the code{\link[graphics]{points}} function for plotting \code{x}, or from other methods
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
plotbands(obs, lband, uband)
}

# Randomly generating a simulated time series
sim <- obs + rnorm(length(obs), mean=3)

\dontrun{
plotbands(obs, lband, uband, sim)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
