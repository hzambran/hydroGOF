%% File gof.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{gof}
\Rdversion{1.1}
\alias{gof}
\alias{gof.default}
\alias{gof.matrix}
\alias{gof.data.frame}
\alias{gof.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical Goodness-of-fit measures
}
\description{
Numerical goodness-of-fit measures between \code{sim} and \code{obs}, with treatment of missing values. Several performance indices for comparing two vectors, matrices or data.frames
}
\usage{
gof(sim, obs, ...)

\method{gof}{default}(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE,
             j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), lQ.thr=0.7, 
             hQ.thr=0.2, start.month=1, digits=2, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
\method{gof}{matrix}(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE,
             j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), lQ.thr=0.7, 
             hQ.thr=0.2, start.month=1, digits=2, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
\method{gof}{data.frame}(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE,
             j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), lQ.thr=0.7, 
             hQ.thr=0.2, start.month=1, digits=2, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
\method{gof}{zoo}(sim, obs, na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE,
             j=1, norm="sd", s=c(1,1,1), method=c("2009", "2012"), lQ.thr=0.7, 
             hQ.thr=0.2, start.month=1, digits=2, fun=NULL, ...,
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
  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{do.spearman}{
logical. Indicates if the Spearman correlation has to be computed. The default is FALSE.
}
  \item{do.pbfdc}{
logical. Indicates if the Percent Bias in the Slope of the midsegment of the Flow Duration Curve (\code{\link{pbiasfdc}}) has to be computed. The default is FALSE.
}
  \item{j}{
argument passed to the \code{\link{mNSE}} function
}
  \item{norm}{
argument passed to the \code{\link{nrmse}} function
}
  \item{s}{
argument passed to the \code{\link{KGE}} function
}
  \item{method}{
argument passed to the \code{\link{KGE}} function
}
  \item{lQ.thr}{
argument passed to the (optional) \code{\link{pbiasfdc}} function
}
  \item{hQ.thr}{
argument passed to the (optional) \code{\link{pbiasfdc}} function
}
  \item{start.month}{
[OPTIONAL]. Only used for the computation of the split KGE (\code{sKGE}) when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represent months in [January, December]. By default \code{start.month=1}.
}
  \item{digits}{
decimal places used for rounding the goodness-of-fit indexes.
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the all the goodness-of-fit functions.

The first two arguments of \code{fun} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and addtional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. 

It is was  designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{FUN} without the addition of any nummeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{FUN}, as described in Pushpalatha et al. (2012). 

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{FUN}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{FUN}.
}
  \item{epsilon.value}{
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}
%%\details{
%%}
\value{
The output of the \code{gof} function is a matrix with one column only, and the following rows: \cr
\item{ME}{Mean Error}
\item{MAE}{Mean Absolute Error}
\item{MSE}{Mean Squared Error}
\item{RMSE}{Root Mean Square Error}
\item{ubRMSE}{Unbiased Root Mean Square Error}
\item{NRMSE}{Normalized Root Mean Square Error  ( -100\% <= nrms <= 100\% )}
\item{PBIAS}{Percent Bias}
\item{RSR}{Ratio of RMSE to the Standard Deviation of the Observations, RSR = rms / sd(obs). ( 0 <= RSR <= +Inf )}
\item{rSD}{Ratio of Standard Deviations, rSD = sd(sim) / sd(obs)}
\item{NSE}{Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )}
\item{mNSE}{Modified Nash-Sutcliffe Efficiency}
\item{rNSE}{Relative Nash-Sutcliffe Efficiency}
\item{d}{Index of Agreement ( 0 <= d <= 1 )}
\item{dr}{Refined Index of Agreement ( -1 <= dr <= 1 )}
\item{md}{Modified Index of Agreement ( 0 <= md <= 1 )}
\item{rd}{Relative Index of Agreement ( 0 <= rd <= 1 )}
\item{cp}{Persistence Index ( 0 <= PI <= 1 ) }
\item{r}{Pearson Correlation coefficient ( -1 <= r <= 1 )}
\item{R2}{Coefficient of Determination ( 0 <= R2 <= 1 )}
\item{bR2}{R2 multiplied by the coefficient of the regression line between \code{sim} and \code{obs} \cr ( 0 <= bR2 <= 1 )}
\item{KGE}{Kling-Gupta efficiency between \code{sim} and \code{obs} \cr ( -Inf <= KGE <= 1 )}
\item{KGElf}{Kling-Gupta Efficiency for low values between \code{sim} and \code{obs} \cr ( -Inf <= KGElf <= 1 )}
\item{KGEnp}{Non-parametric version of the Kling-Gupta Efficiency between \code{sim} and \code{obs} \cr ( -Inf <= KGEnp <= 1 )}
\item{sKGE}{Split Kling-Gupta Efficiency between \code{sim} and \code{obs} \cr ( -Inf <= sKGE <= 1 ). Only computed when both \code{sim} and \code{obs} are zoo objects}
\item{VE}{Volumetric efficiency between \code{sim} and \code{obs} \cr ( -Inf <= VE <= 1)}
\item{r.Spearman}{Spearman Correlation coefficient ( -1 <= r.Spearman <= 1 ). Only computed when \code{do.spearman=TRUE}}
\item{pbiasfdc}{PBIAS in the slope of the midsegment of the Flow Duration Curve}

%% ...
}
\references{
\cite{Legates, D. R., and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resour. Res., 35(1), 233--241} \cr

\cite{Krause P., Boyle D.P., and B\"{a}se F., Comparison of different efficiency criteria for hydrological model assessment, Advances in Geosciences 5 (2005), pp. 89--97} \cr

\cite{Moriasi, D.N., Arnold, J.G., Van Liew, M.W., Bingner, R.L., Harmel, R.D., Veith, T.L. 2007. Model evaluation guidelines for systematic quantification of accuracy in watershed simulations \cr Transactions of the ASABE. 50(3):885-900} \cr

\cite{Boyle, D. P., H. V. Gupta, and S. Sorooshian (2000), Toward Improved Calibration of Hydrologic Models: Combining the Strengths of Manual and Automatic Methods, Water Resour. Res., 36(12), 3663--3674} \cr

\cite{Kitanidis, P. K., and R. L. Bras (1980), Real-Time Forecasting With a Conceptual Hydrologic Model 2. Applications and Results, Water Resour. Res., 16(6), 1034--1044} \cr

\cite{J.E. Nash and J.V. Sutcliffe, River flow forecasting through conceptual models. Part 1: a discussion of principles, J. Hydrol. 10 (1970), pp. 282--290} \cr

\cite{Yapo P. O., Gupta H. V., Sorooshian S., 1996. Automatic calibration of conceptual rainfall-runoff models: sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23--48} \cr

\cite{Yilmaz, K. K., H. V. Gupta, and T. Wagener  (2008), A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model, Water Resour. Res., 44, W09417, doi:10.1029/2007WR006716} \cr

\cite{Hoshin V. Gupta, Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694} \cr

\cite{Criss, R. E. and Winston, W. E. (2008), Do Nash values have value? Discussion and alternate proposals. Hydrological Processes, 22: 2723-2725. doi: 10.1002/hyp.7072}
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension. \cr

Missing values in \code{obs} and/or \code{sim} can be removed before the computations, depending on the value of \code{na.rm}. \cr

Although \code{r} and \code{r2} have been widely used for model evaluation, these statistics are over-sensitive to outliers and insensitive to additive and proportional differences between model predictions and measured data (Legates and McCabe, 1999)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{me}}, \code{\link{mae}}, \code{\link{mse}}, \code{\link{rmse}}, \code{\link{ubRMSE}}, \code{\link{nrmse}}, 
\code{\link{pbias}}, \code{\link{rsr}}, \code{\link{rSD}}, \code{\link{NSE}}, \code{\link{mNSE}}, \code{\link{rNSE}}, 
\code{\link{d}}, \code{\link{dr}}, \code{\link{md}}, \code{\link{rd}}, \code{\link{cp}}, \code{\link{rPearson}}, 
\code{\link{br2}}, \code{\link{KGE}}, \code{\link{KGElf}}, \code{\link{sKGE}}, \code{\link{KGEnp}}, \code{\link{VE}},
\code{\link{pbiasfdc}},
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
gof(sim, obs)

obs <- 1:10
sim <- 2:11
gof(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'gof' for the "best" (unattainable) case
gof(sim=sim, obs=obs)

##################
# Example 3: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

gof(sim=sim, obs=obs)

##################
# Example 4: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

gof(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
gof(sim=lsim, obs=lobs)

##################
# Example 5: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

gof(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
gof(sim=lsim, obs=lobs)

##################
# Example 6: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
gof(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
gof(sim=lsim, obs=lobs)

##################
# Example 7: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
gof(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
gof(sim=lsim, obs=lobs)

##################
# Example 8: gof for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

gof(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
gof(sim=sim1, obs=obs1)

# Storing a matrix object with all the GoFs:
g <-  gof(sim, obs)

# Getting only the RMSE
g[4,1]
g["RMSE",]

\dontrun{
# Writing all the GoFs into a TXT file
write.table(g, "GoFs.txt", col.names=FALSE, quote=FALSE)

# Getting the graphical representation of 'obs' and 'sim' along with the 
# numeric goodness of fit 
ggof(sim=sim, obs=obs)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
