%% File KGE.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
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

This goodness-of-fit measure was developed by Gupta et al. (2009) to provide a diagnostically interesting decomposition of the Nash-Sutcliffe efficiency (and hence MSE), which facilitates the analysis of the relative importance of its different components (correlation, bias and variability) in the context of hydrological modelling\cr
Kling et al. (2012), proposed a revised version of this index, to ensure that the bias and variability ratios are not cross-correlated \cr

In the computation of this index, there are three main components involved:\cr
1) \kbd{r}    : the Pearson product-moment correlation coefficient. Ideal value is r=1 \cr
2) \kbd{Beta} : the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1 \cr
3) \kbd{vr}   : variability ratio, which could be computed using the standard deviation (\kbd{Alpha}) or the coefficient of variation (\kbd{Gamma}) of \code{sim} and \code{obs}, depending on the value of \code{method} \cr

3.1) \kbd{Alpha}: the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Ideal value is Alpha=1. \cr
3.2) \kbd{Gamma}: the ratio between the coefficient of variation (\kbd{CV}) of the simulated values to the coefficient of variation of the observed ones. Ideal value is Gamma=1. \cr

For a full discussion pf the Kling-Gupta index, and its advantages over the Nash-Sutcliffe efficiency (\code{\link{NSE}}) see Gupta et al. (2009).
}
\usage{
KGE(sim, obs, ...)

\method{KGE}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
             out.type=c("single", "full"), ...)

\method{KGE}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
             out.type=c("single", "full"), ...)

\method{KGE}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
             out.type=c("single", "full"), ...)
             
\method{KGE}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012"), 
             out.type=c("single", "full"), ...)
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
character, indicating the formula used to compute the variability ratio in the Kling-Gupta efficiency. Valid values are:\cr
-) \kbd{2009}: the variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. This is the default option. See Gupta et al. 2009\cr
-) \kbd{2012}: the variability is defined as \sQuote{Gamma}, the ratio of the coefficient of variation of \code{sim} values to the coefficient of variation of \code{obs}. See Kling et al. 2012. 
}
  \item{out.type}{
character, indicating the if the output of the function has to include or not each one of the three terms used in the computation of the Kling-Gupta efficiency. Valid values are:\cr
-) \kbd{single}: the output is a numeric with the Kling-Gupta efficiency only\cr
-) \kbd{full}: the output is a list of two elements: the first one with the Kling-Gupta efficiency, and the second is a numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method})
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
\deqn{KGE = 1 - ED}
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
KGE = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2] ; r=Pearson product-moment correlation coefficient ; alpha=sigma_s/sigma_o ; beta=mu_s/mu_o ; gamma=CV_s/CV_o}

Kling-Gupta efficiencies range from -Inf to 1. Essentially, the closer to 1, the more accurate the model is. \cr
}
\value{
If \code{out.type=single}: numeric with the Kling-Gupta efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}\cr
If \code{out.type=full}: a list of two elements:
\item{KGE.value}{
numeric with the Kling-Gupta efficiency. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\item{KGE.elements}{
numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method}). If \code{sim} and \code{obs} are matrices, the output value is a matrix, with the previous three elements computed for each column of \code{sim} and \code{obs}\cr  
}
}
\references{
\cite{Gupta, Hoshin V., Harald Kling, Koray K. Yilmaz, Guillermo F. Martinez. Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, Volume 377, Issues 1-2, 20 October 2009, Pages 80-91. DOI: 10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694}

\cite{Kling, H., M. Fuchs, and M. Paulin (2012), Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, Volumes 424-425, 6 March 2012, Pages 264-277, DOI:10.1016/j.jhydrol.2012.01.011}
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
