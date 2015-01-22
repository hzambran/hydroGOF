%% File plot2.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or lat

\name{plot2}
\Rdversion{1.1}
\alias{plot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting 2 Time Series
}
\description{
Plotting of 2 time series, in two different vertical windows or overlapped in the same window. \cr
It requires the \pkg{hydroTSM} package.
}
\usage{
plot2(x, y, plot.type = "multiple", 
      tick.tstep = "auto", lab.tstep = "auto", lab.fmt=NULL,
      main, xlab = "Time", ylab,
      cal.ini=NA, val.ini=NA, date.fmt="\%Y-\%m-\%d",
      gof.leg = FALSE, gof.digits=2, 
      gofs=c("ME", "MAE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", 
             "rNSE", "d", "md", "rd", "r", "R2", "bR2", "KGE", "VE"),
      legend, leg.cex = 1,
      col = c("black", "blue"),
      cex = c(0.5, 0.5), cex.axis=1.2, cex.lab=1.2, 
      lwd= c(1,1), lty=c(1,3), pch = c(1, 9), 
      pt.style = "ts", add = FALSE, 
      ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
time series that will be plotted. class(x) must be ts or zoo. If \code{leg.gof=TRUE}, then \code{x} is considered as \bold{simulated} (for some goodness-of-fit functions this is important)
}
  \item{y}{
time series that will be plotted. class(x) must be ts or zoo. If \code{leg.gof=TRUE}, then \code{y} is considered as \bold{observed} values (for some goodness-of-fit functions this is important)
}
  \item{plot.type}{
character, indicating if the 2 ts have to be plotted in the same window or in two different vertical ones. Valid values are: \cr
-) \kbd{single}  : (default) superimposes the 2 ts on a single plot \cr
-) \kbd{multiple}: plots the 2 series on 2 multiple vertical plots 
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
  \item{main}{
an overall title for the plot: see \code{\link[graphics]{title}}
}
  \item{xlab}{
label for the 'x' axis
}
  \item{ylab}{
label for the 'y' axis 
}
 \item{cal.ini}{
OPTIONAL. Character, indicating the date in which the calibration period started. \cr
When \code{cal.ini} is provided, all the values in \code{obs} and \code{sim} with dates previous to \code{cal.ini} are SKIPPED from the computation of the goodness-of-fit measures (when \code{gof.leg=TRUE}), but their values are still plotted, in order to examine if the warming up period was too short, acceptable or too long for the chosen calibration period.
In addition, a vertical red line in drawn at this date. 
}
  \item{val.ini}{
OPTIONAL. Character with the date in which the validation period started. \cr
ONLY used for drawing a vertical red line at this date. 
}
  \item{date.fmt}{
OPTIONAL. Character indicating the format in which the dates entered are stored in \code{cal.ini} and \code{val.ini}. Default value is \kbd{\%Y-\%m-\%d}. ONLY required when \code{cal.ini} or \code{val.ini} is provided.
}
  \item{gof.leg}{
logical, indicating if several numerical goodness-of-fit values have to be computed between \code{sim} and \code{obs}, and plotted as a legend on the graph. If \code{gof.leg=TRUE} (default value), then \code{x} is considered as observed and \code{y} as simulated values (for some gof functions this is important). This legend is ONLY plotted when \code{plot.type="single"}
}
  \item{gof.digits}{
OPTIONAL, only used when \code{gof.leg=TRUE}. Decimal places used for rounding the goodness-of-fit indexes.
}
  \item{gofs}{
character, with one or more strings indicating the goodness-of-fit measures to be shown in the legend of the plot when  \code{gof.leg=TRUE}. \cr
Possible values are in \code{c("ME", "MAE", "MSE", "RMSE", "NRMSE", "PBIAS", "RSR", "rSD", "NSE", "mNSE", "rNSE", "d", "md", "rd", "cp", "r", "R2", "bR2", "KGE", "VE")}.
}
  \item{legend}{
vector of length 2 to appear in the legend.  
}
  \item{leg.cex}{
numeric, indicating the character expansion factor *relative* to current 'par("cex")'. Used for text, and provides the default for 'pt.cex' and 'title.cex'. Default value = 1 \cr
So far, it controls the expansion factor of the 'GoF' legend and the legend referring to \code{x} and \code{y}
}
  \item{col}{
character, with the colors of \code{x} and \code{y}
}
  \item{cex}{
numeric, with the values controlling the size of text and symbols of \code{x} and \code{y} with respect to the default
}
  \item{cex.axis}{
numeric, with the magnification of axis annotation relative to 'cex'. See \code{\link[graphics]{par}}.
}
  \item{cex.lab}{
numeric, with the magnification to be used for x and y labels relative to the current setting of 'cex'. See \code{\link[graphics]{par}}.
}
  \item{lwd}{
vector with the line width of \code{x} and \code{y}
}
  \item{lty}{
vector with the line type of \code{x} and \code{y}
}
  \item{pch}{
vector with the type of symbol for \code{x} and \code{y}. (e.g.: 1: white circle; 9: white rhombus with a cross inside)
}
  \item{pt.style}{
Character, indicating if the 2 ts have to be plotted as lines or bars. Valid values are: \cr
-) \kbd{ts} : (default) each ts is plotted as a lines along the \code{x} axis \cr
-) \kbd{bar}: the 2 series are plotted as a barplot. 
}
  \item{add}{
logical indicating if other plots will be added in further calls to this function. \cr
-) \kbd{FALSE} => the plot and the legend are plotted on the same graph \cr
-) \kbd{TRUE}  => the legend is plotted in a new graph, usually when called from another function (e.g.: \code{\link{ggof}})
}
  \item{\dots}{
further arguments passed to \code{\link[zoo]{plot.zoo}} function for plotting \code{x}, or from other methods
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
It requires the package \pkg{hydroTSM}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ggof}}
}
\examples{
sim <- 2:11
obs <- 1:10
\dontrun{
plot2(sim, obs)
}

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Plotting 'sim' and 'obs' in 2 separate panels
plot2(x=obs, y=sim)

# Plotting 'sim' and 'obs' in the same window
plot2(x=obs, y=sim, plot.type="single")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
