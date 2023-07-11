%% File KGE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or late

\name{KGE}
\Rdversion{1.1}
\alias{KGE}
\alias{KGE.default}
\alias{KGE.matrix}
\alias{KGE.data.frame}
\alias{KGE.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kling-Gupta Efficiency
}
\description{
Kling-Gupta efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This goodness-of-fit measure was developed by Gupta et al. (2009) to provide a diagnostically interesting decomposition of the Nash-Sutcliffe efficiency (and hence MSE), which facilitates the analysis of the relative importance of its different components (correlation, bias and variability) in the context of hydrological modelling. 
Kling et al. (2012), proposed a revised version of this index, to ensure that the bias and variability ratios are not cross-correlated.

Kling-Gupta efficiencies range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. 
Knoben et al. (2019) showed that KGE values greater than -0.41 indicate that a model improves upon the mean flow benchmark, even if the model's KGE value is negative.

In the computation of this index, there are three main components involved: 

1) \kbd{r}    : the Pearson product-moment correlation coefficient. Ideal value is r=1.

2) \kbd{Beta} : the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1.

3) \kbd{vr}   : variability ratio, which could be computed using the standard deviation (\kbd{Alpha}) or the coefficient of variation (\kbd{Gamma}) of \code{sim} and \code{obs}, depending on the value of \code{method}:


  3.1) \kbd{Alpha}: the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Its ideal value is \kbd{Alpha=1}.

  3.2) \kbd{Gamma}: the ratio between the coefficient of variation (\kbd{CV}) of the simulated values to the coefficient of variation of the observed ones. Its ideal value is \kbd{Gamma=1}.

For a full discussion pf the Kling-Gupta index, and its advantages over the Nash-Sutcliffe efficiency (\code{\link{NSE}}) see Gupta et al. (2009).
}
\usage{
KGE(sim, obs, ...)

\method{KGE}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{KGE}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{KGE}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
\method{KGE}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
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

-) \kbd{2021}: the bias is defined as \sQuote{Beta}, the ratio of \code{mean(sim)} minus \code{mean(obs)} to the standard deviation of \code{obs}.  The variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. See Tang et al. (2021). 
}
  \item{out.type}{
character, indicating the whether the output of the function has to include each one of the three terms used in the computation of the Kling-Gupta efficiency or not. Valid values are:

-) \kbd{single}: the output is a numeric with the Kling-Gupta efficiency only.

-) \kbd{full}: the output is a list of two elements: the first one with the Kling-Gupta efficiency, and the second is a numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method}).
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Kling-Gupta efficiency.

The first two arguments of \code{fun} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and addtional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. 

It is was  designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any nummeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012). 

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}
  \item{epsilon.value}{
 numeric value to be added to both \code{sim} and \code{obs} when \code{epsilon.type="otherValue"}.
}
}
\details{
\deqn{KGE = 1 - ED}
\deqn{ ED =  \sqrt{ (s[1]*(r-1))^2 +(s[2]*(vr-1))^2 + (s[3]*(\beta-1))^2 } }{%
KGE = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2] 

r     = Pearson product-moment correlation coefficient 
beta  = mu_s/mu_o ; 
alpha = sigma_s/sigma_o
gamma = CV_s/CV_o; 

method="2009": vr=alfa
method="2012": vr=gamma }
\deqn{r=Pearson product-moment correlation coefficient}
\deqn{vr=  \left\{
  \begin{array}{cc}
    \alpha & , \: method=2009 \\
    \gamma & , \: method=2012
  \end{array}
\right.}
\deqn{\beta=\mu_s/\mu_o}
\deqn{\alpha=\sigma_s/\sigma_o}
\deqn{\gamma=\frac{CV_s}{CV_o} = \frac{\sigma_s/\mu_s}{\sigma_o/\mu_o}}
}
\value{
If \code{out.type=single}: numeric with the Kling-Gupta efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}

If \code{out.type=full}: a list of two elements:
\item{KGE.value}{
numeric with the Kling-Gupta efficiency. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\item{KGE.elements}{
numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method}). If \code{sim} and \code{obs} are matrices, the output value is a matrix, with the previous three elements computed for each column of \code{sim} and \code{obs}\cr  
}
}
\references{
\cite{Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGE criterion. doi:10.5194/hess-22-4583-2018.}

\cite{Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019.}

\cite{Mizukami, N.; Rakovec, O.; Newman, A. J.; Clark, M. P.; Wood, A. W.; Gupta, H. V.; Kumar, R. (2019). On the choice of calibration metrics for "high-flow" estimation using hydrologic models. doi:10.5194/hess-23-2601-2019.}

\cite{Tang, G., Clark, M. P., & Papalexiou, S. M. (2021). SC-earth: a station-based serially complete earth dataset from 1950 to 2019. Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1}

\cite{Cinkus, G., Mazzilli, N., Jourde, H., Wunsch, A., Liesch, T., Ravbar, N., Chen, Z., and Goldscheider, N. (2023). When best is the enemy of good - critical evaluation of performance criteria in hydrological models. Hydrology and Earth System Sciences 27, 2397-2411, doi:10.5194/hess-27-2397-2023}

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
\code{\link{NSE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
KGE(sim, obs)

obs <- 1:10
sim <- 2:11
KGE(sim, obs)

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

##################
# Example3: KGE for simulated values equal to observations plus random noise 
#           on the first half of the observed values
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim <- obs 
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)

# Computing the new 'KGE'
KGE(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'KGE'
KGE(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
