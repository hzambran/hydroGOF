%% File KGElf.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2017-2022 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{KGElf}
\Rdversion{1.1}
\alias{KGElf}
\alias{KGElf.default}
\alias{KGElf.matrix}
\alias{KGElf.data.frame}
\alias{KGElf.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kling-Gupta Efficiency for low values
}
\description{
Kling-Gupta efficiency between \code{sim} and \code{obs}, with focus on low (streamflow) values and treatment of missing values. \cr

This goodness-of-fit measure was developed by Garcia et al. (2017), as a modification to the original Kling-Gupta efficiency (KGE) proposed by Gupta et al. (2009). See Details.
}
\usage{
KGElf(sim, obs, ...)

\method{KGElf}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
               fun=function(x) 1/x, ...,
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

\method{KGElf}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
               fun=function(x) 1/x, ...,
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

\method{KGElf}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
               fun=function(x) 1/x, ...,
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)
             
\method{KGElf}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
               fun=function(x) 1/x, ...,
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue"), 
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
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing this goodness-of-fit index.

The first two arguments of \code{FUN} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and addtional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{FUN}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{FUN}. 

It is was  designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{FUN} without the addition of any nummeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{FUN}, as described in Pushpalatha et al. (2012). 

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{FUN}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{FUN}.
}
  \item{epsilon.value}{
 numeric value to be added to both \code{sim} and \code{obs} when \code{epsilon.type="otherValue"}.
}
}
\details{

Garcia et al. (2017) tested different objective functions and found that the mean value of the KGE applied to the streamflows (i.e., KGE(Q)) and the KGE applied to the inverse of the streamflows (i.e., KGE(1/Q) is able to provide a an aceptable representation of low-flow indices important for water management. They also found that KGE applied to a transformation of streamflow values (e.g., log)  is inadequate to capture low-flow indices important for water management. \cr

The robustness of their findings depends more on the climate variability rather than the objective function, and they are insensitive to the hydrological model used in the evaluation.

\deqn{KGElf = 1 - ED}
\deqn{ ED =  \sqrt{ (s[1]*(r-1))^2 +(s[2]*(vr-1))^2 + (s[3]*(\beta-1))^2 } }
\deqn{r=\textrm{Pearson product-moment correlation coefficient}}
\deqn{\beta=\mu_s/\mu_o}
\deqn{vr=  \left\{
  \begin{array}{cc}
    \alpha & , \: \textrm{method="2009"} \\
    \gamma & , \: \textrm{method="2012"}
  \end{array}
  \right.}  
\deqn{\alpha=\sigma_s/\sigma_o}
\deqn{\gamma=\frac{CV_s}{CV_o}= \frac{\sigma_s/\mu_s}{\sigma_o/\mu_o}}{%
KGElf = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2] ; r=Pearson product-moment correlation coefficient ; alpha=sigma_s/sigma_o ; beta=mu_s/mu_o ; gamma=CV_s/CV_o}

Traditional Kling-Gupta efficiencies (Gupta et al., 2009; Kling et al., 2012) range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. \cr
Knoben et al. (2019) showed that traditional Kling-Gupta (Gupta et al., 2009; Kling et al., 2012) values greater than -0.41 indicate that a model improves upon the mean flow benchmark, even if the model's KGE value is negative.
}
\value{
numeric with the Kling-Gupta efficiency between \code{sim} and \code{obs}. \cr
If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\references{

\cite{Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. Hydrological sciences journal, 62(7), 1149-1166. doi:10.1080/02626667.2017.1308511} \cr

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andr\'{e}assian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055} \cr

\cite{Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature metrics for an improved overall performance evaluation of hydrological models. Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044} \cr

\cite{Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694} \cr

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011} \cr

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGElf criterion. doi:10.5194/hess-22-4583-2018} \cr

\cite{Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019} \cr
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
\code{\link{KGE}}, \code{\link{KGEnp}}, \code{\link{sKGE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
KGElf(sim, obs)

obs <- 1:10
sim <- 2:11
KGElf(sim, obs)

##################
# Example2: Looking at the difference between 'method=2009' and 'method=2012'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs, method="2012")

##################
# Example3: KGElf for simulated values equal to observations plus random noise 
#           on the first half of the observed values
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim <- obs 
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)

# Computing the new 'KGElf'
KGElf(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'KGElf'
KGElf(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
