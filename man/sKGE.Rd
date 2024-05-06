%% File sKGE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2017-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{sKGE}
\Rdversion{1.1}
\alias{sKGE}
\alias{sKGE.default}
\alias{sKGE.matrix}
\alias{sKGE.data.frame}
\alias{sKGE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split Kling-Gupta Efficiency
}
\description{
Split Kling-Gupta efficiency between \code{sim} and \code{obs}.

This goodness-of-fit measure was developed by Fowler et al. (2018), as a modification to the original Kling-Gupta efficiency (KGE) proposed by Gupta et al. (2009). See Details.
}
\usage{
sKGE(sim, obs, ...)

\method{sKGE}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"),
              start.month=1, out.PerYear=FALSE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{sKGE}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"),
              start.month=1, out.PerYear=FALSE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

\method{sKGE}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"),
              start.month=1, out.PerYear=FALSE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)
             
\method{sKGE}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"),
              start.month=1, out.PerYear=FALSE, fun=NULL, ...,
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
  \item{s}{
numeric of length 3, representing the scaling factors to be used for re-scaling the criteria space before computing the Euclidean distance from the ideal point c(1,1,1), i.e., \code{s} elements are used for adjusting the emphasis on different components.
The first elements is used for rescaling the Pearson product-moment correlation coefficient (\code{r}), the second element is used for rescaling \code{Alpha} and the third element is used for re-scaling \code{Beta}
}
  \item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}
  \item{method}{
character, indicating the formula used to compute the variability ratio in the Kling-Gupta efficiency. Valid values are:

-) \kbd{2009}: the variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. This is the default option. See Gupta et al. (2009).

-) \kbd{2012}: the variability is defined as \sQuote{Gamma}, the ratio of the coefficient of variation of \code{sim} values to the coefficient of variation of \code{obs}. See Kling et al. (2012). 
}
  \item{start.month}{
[OPTIONAL]. Only used when the (hydrological) year of interest is different from the calendar year.

numeric in [1:12] indicating the starting month of the (hydrological) year. Numeric values in [1, 12] represent months in [January, December]. By default \code{start.month=1}.
}
  \item{out.PerYear}{
logical, indicating whether the output of this function has to include the Kling-Gupta efficiencies obtained for the individual years in \code{sim} and \code{obs} or not.
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing this goodness-of-fit index.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
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
\details{
Garcia et al. (2017) tested different objective functions and found that the mean value of the KGE applied to the streamflows (i.e., KGE(Q)) and the KGE applied to the inverse of the streamflows (i.e., KGE(1/Q) is able to provide a an aceptable representation of low-flow indices important for water management. They also found that KGE applied to a transformation of streamflow values (e.g., log)  is inadequate to capture low-flow indices important for water management. \cr

The robustness of their findings depends more on the climate variability rather than the objective function, and they are insensitive to the hydrological model used in the evaluation.\cr

Traditional Kling-Gupta efficiencies (Gupta et al., 2009; Kling et al., 2012) range from -Inf to 1 and, therefore, KGElf should also range from -Inf to 1. Essentially, the closer to 1, the more similar \code{sim} and \code{obs} are. \cr

Knoben et al. (2019) showed that traditional Kling-Gupta (Gupta et al., 2009; Kling et al., 2012) values greater than -0.41 indicate that a model improves upon the mean flow benchmark, even if the model's KGE value is negative.
}
\value{
If \code{out.PerYear=FALSE}: numeric with the Split Kling-Gupta efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Split Kling-Gupta efficiency between each column of \code{sim} and \code{obs}

If \code{out.PerYear=FALSE}: a list of two elements:
\item{sKGE.value}{
numeric with the Split Kling-Gupta efficiency. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Split Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\item{KGE.PerYear}{
numeric with the Kling-Gupta efficincies obtained for the individual years in \code{sim} and \code{obs}.
}
}
\references{
\cite{Fowler, K.; Coxon, G.; Freer, J.; Peel, M.; Wagener, T.; Western, A.; Woods, R.; Zhang, L. (2018). Simulating runoff under changing climatic conditions: A framework for model improvement. Water Resources Research, 54(12), 812-9832. doi:10.1029/2018WR023989.}

\cite{Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055.}

\cite{Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature metrics for an improved overall performance evaluation of hydrological models. Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044.} 

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the sKGE criterion. doi:10.5194/hess-22-4583-2018}

\cite{Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019.}
}
\author{
Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{KGE}}, \code{\link{KGElf}}, \code{\link{KGEnp}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: Looking at the difference between 'method=2009' and 'method=2012'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")

# sKGE (Garcia et al., 2017):
sKGE(sim=sim, obs=obs, method="2012")

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'sKGE' for the "best" (unattainable) case
sKGE(sim=sim, obs=obs)

##################
# Example 3: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

sKGE(sim=sim, obs=obs)

##################
# Example 4: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

sKGE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
sKGE(sim=lsim, obs=lobs)

##################
# Example 5: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

sKGE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
sKGE(sim=lsim, obs=lobs)

##################
# Example 6: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
sKGE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
sKGE(sim=lsim, obs=lobs)

##################
# Example 7: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
sKGE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
sKGE(sim=lsim, obs=lobs)

##################
# Example 8: sKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

sKGE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
sKGE(sim=sim1, obs=obs1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
