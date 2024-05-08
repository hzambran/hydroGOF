%% File KGEkm.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2011-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{KGEkm}
\Rdversion{1.1}
\alias{KGEkm}
\alias{KGEkm.default}
\alias{KGEkm.matrix}
\alias{KGEkm.data.frame}
\alias{KGEkm.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Kling-Gupta Efficiency with knowable-moments
}
\description{
Kling-Gupta efficiency between \code{sim} and \code{obs}, with use of knowable moments and treatment of missing values.

This goodness-of-fit measure was developed by Pizarro and Jorquera (2024), as a modification to the original Kling-Gupta efficiency (KGE) proposed by Gupta et al. (2009). See Details.
}
\usage{
KGEkm(sim, obs, ...)

\method{KGEkm}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{KGEkm}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

\method{KGEkm}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
\method{KGEkm}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
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

-) \kbd{2012}: the variability is defined as \sQuote{Gamma}, the ratio of the coefficient of variation of \code{sim} values to the coefficient of variation of \code{obs}. See Pizarro and Jorquera (2024) and Kling et al. (2012). 

-) \kbd{2009}: the variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. This is the default option. See Gupta et al. (2009).

-) \kbd{2021}: the bias is defined as \sQuote{Beta}, the ratio of \code{mean(sim)} minus \code{mean(obs)} to the standard deviation of \code{obs}. The variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. See Tang et al. (2021). 
}
  \item{out.type}{
character, indicating the whether the output of the function has to include each one of the three terms used in the computation of the Kling-Gupta efficiency or not. Valid values are:

-) \kbd{single}: the output is a numeric with the Kling-Gupta efficiency only.

-) \kbd{full}: the output is a list of two elements: the first one with the Kling-Gupta efficiency, and the second is a numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method}).
}
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Kling-Gupta efficiency.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
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
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}
\details{
Traditional objective functions, such as Nash-Sutcliffe Efficiency (NSE) and Kling-Gupta Efficiency (KGE), often make assumptions about data distribution and are sensitive to outliers. The Kling-Gupta Efficiency with knowable-moments (KGEkm) goodness-of-fit measure was developed by Pizarro and Jorquera (2024) to provide a reliable estimation and effective description of high-order statistics from typical hydrological samples and, therefore, reducing uncertainty in their estimation and computation of the KGE.


In the same line that the traditional Kling-Gupta efficiency,  the KGEkm ranges from -Inf to 1. Essentially, the closer to 1, the more similar \code{sim} and \code{obs} are. 


In the computation of this index, there are three main components involved: 

1) \kbd{r}    : the Pearson product-moment correlation coefficient. Ideal value is r=1.

2) \kbd{Beta} : the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1.

3) \kbd{vr}   : variability ratio, which could be computed using the standard deviation (\kbd{Alpha}) or the coefficient of variation (\kbd{Gamma}) of \code{sim} and \code{obs}, depending on the value of \code{method}:

  3.1) \kbd{Alpha}: the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Its ideal value is \kbd{Alpha=1}.

  3.2) \kbd{Gamma}: the ratio between the coefficient of variation (\kbd{CV}) of the simulated values to the coefficient of variation of the observed ones. Its ideal value is \kbd{Gamma=1}.


\deqn{KGEkm = 1 - ED}
\deqn{ ED =  \sqrt{ (s[1]*(r-1))^2 +(s[2]*(vr-1))^2 + (s[3]*(\beta-1))^2 } }{%
KGEkm = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2] 

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
\item{KGEkm.value}{
numeric with the Kling-Gupta efficiency. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\item{KGEkm.elements}{
numeric with 3 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the ratio between the mean of the simulated values to the mean of observations (\sQuote{Beta}), and the variability measure (\sQuote{Gamma} or \sQuote{Alpha}, depending on the value of \code{method}). If \code{sim} and \code{obs} are matrices, the output value is a matrix, with the previous three elements computed for each column of \code{sim} and \code{obs}\cr  
}
}
\references{
\cite{Pizarro, A.; Jorquera, J. (2024). Advancing objective functions in hydrological modelling: Integrating knowable moments for improved simulation accuracy. Journal of Hydrology, 634, 131071. doi:10.1016/j.jhydrol.2024.131071.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.}

\cite{Tang, G.; Clark, M. P.; Papalexiou, S. M. (2021). SC-earth: a station-based serially complete earth dataset from 1950 to 2019. Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.}

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGEkm criterion. doi:10.5194/hess-22-4583-2018.}

\cite{Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores. Hydrology and Earth System Sciences, 23(10), 4323-4331. doi:10.5194/hess-23-4323-2019.}

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
\code{\link{KGE}}, \code{\link{KGElf}}, \code{\link{sKGE}}, \code{\link{KGEnp}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
KGEkm(sim, obs)

obs <- 1:10
sim <- 2:11
KGEkm(sim, obs)

##################
# Example2: Looking at the difference between 'method=2009' and 'method=2012'

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGEkm 2012 (method="2012" is the default option for KGEkm)
KGEkm(sim=sim, obs=obs, method="2012", out.type="full")

# KGEkm 2009
KGEkm(sim=sim, obs=obs, method="2009", out.type="full")


##################
# Example 2: Looking at the difference between 'KGEkm', KGE', 'NSE', 'wNSE', 
#            'wsNSE' and 'APFB' for detecting differences in high flows

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to high flows, i.e., those equal or higher than 
# the quantile 0.9 of the observed values.
sim      <- obs
hQ.thr   <- quantile(obs, probs=0.9, na.rm=TRUE)
hQ.index <- which(obs >= hQ.thr)
hQ.n     <- length(hQ.index)
sim[hQ.index] <- sim[hQ.index] + rnorm(hQ.n, mean=mean(sim[hQ.index], na.rm=TRUE))

# KGEkm (Pizarro and Jorquera, 2024; method='2012')
KGEkm(sim=sim, obs=obs)

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012")

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021")

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)

# Weighted Nash-Sutcliffe efficiency (Hundecha and Bardossy, 2004)
wNSE(sim=sim, obs=obs)

# wsNSE (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs)

# APFB (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)


##################
# Example 4: Looking at the difference between 'KGE', 'NSE', 'wsNSE',
#            'dr', 'rd', 'md', and 'KGElf' for detecting 
#            differences in low flows

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, created equal to the observed values and then 
# random noise is added only to low flows, i.e., those equal or lower than 
# the quantile 0.4 of the observed values.
sim      <- obs
lQ.thr   <- quantile(obs, probs=0.4, na.rm=TRUE)
lQ.index <- which(obs <= lQ.thr)
lQ.n     <- length(lQ.index)
sim[lQ.index] <- sim[lQ.index] + rnorm(lQ.n, mean=mean(sim[lQ.index], na.rm=TRUE))

# KGEkm (Pizarro and Jorquera, 2024; method='2012')
KGEkm(sim=sim, obs=obs)

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012")

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021")

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)

# Weighted seasonal Nash-Sutcliffe efficiency (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs, lambda=0.05, j=1/2)

# Refined Index of Agreement (Willmott et al., 2012):
dr(sim=sim, obs=obs)

# Relative Index of Agreement (Krause et al., 2005):
rd(sim=sim, obs=obs)

# Modified Index of Agreement (Krause et al., 2005):
md(sim=sim, obs=obs)

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs)


##################
# Example 5: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

KGEkm(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
KGEkm(sim=lsim, obs=lobs)

##################
# Example 6: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)

##################
# Example 7: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)

##################
# Example 8: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)

##################
# Example 9: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

KGEkm(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
KGEkm(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
