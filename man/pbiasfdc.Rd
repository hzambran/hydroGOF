% File pbiasfdc.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{pbiasfdc}
\Rdversion{1.1}
\alias{pbiasfdc}
\alias{pbiasfdc.default}
\alias{pbiasfdc.matrix}
\alias{pbiasfdc.data.frame}
\alias{pbiasfdc.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percent Bias in the Slope of the Midsegment of the Flow Duration Curve
}
\description{
Percent Bias in the slope of the midsegment of the flow duration curve (FDC) [\%]. It is related to the vertical soil moisture redistribution. \cr
}
\usage{
pbiasfdc(sim, obs, ...)

\method{pbiasfdc}{default}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, fun=NULL, ..., 
       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
       epsilon.value=NA)

\method{pbiasfdc}{data.frame}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
        epsilon.value=NA)

\method{pbiasfdc}{matrix}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
        epsilon.value=NA)
       
\method{pbiasfdc}{zoo}(sim, obs, lQ.thr=0.7, hQ.thr=0.2, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
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
  \item{lQ.thr}{
numeric, used to classify low flows. All the streamflows with a probability of exceedence larger or equal to \code{lQ.thr} are classified as low flows
}
  \item{hQ.thr}{
numeric, used to classify high flows. All the streamflows with a probability of exceedence larger or equal to \code{hQ.thr} are classified as high flows
}
  \item{na.rm}{
a logical value indicating whether 'NA' values should be stripped before the computation proceeds.
}
  \item{plot}{
a logical value indicating if the flow duration curves corresponding to \code{obs} and \code{sim} have to be  plotted or not.
}
  \item{verbose}{
logical; if TRUE, progress messages are printed 
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing this goodness-of-fit index.

The first two arguments of \code{fun} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
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
%%\details{
%%
%%}
\value{
Percent Bias in the slope of the midsegment of the flow duration curve, between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes, the returned value is a vector, with the Percent Bias in the slope of the midsegment of the flow duration curve, between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
The result is given in percentage (\%). \cr

It requires the \pkg{hydroTSM} package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[hydroTSM]{fdc}, \code{\link{gof}}
}
\examples{
\dontrun{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
pbiasfdc(sim, obs)

obs <- 1:10
sim <- 2:11
pbiasfdc(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'pbiasfdc' for the "best" (unattainable) case
pbiasfdc(sim=sim, obs=obs)

##################
# Example 3: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

pbiasfdc(sim=sim, obs=obs)

##################
# Example 4: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

pbiasfdc(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 5: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 6: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 7: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 8: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

pbiasfdc(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
pbiasfdc(sim=sim1, obs=obs1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
