%% File KGElf.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2017-2024 Mauricio Zambrano-Bigiarini
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

\method{KGElf}{default}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue", "none"), 
               epsilon.value=NA, ...)

\method{KGElf}{data.frame}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue", "none"), 
               epsilon.value=NA, ...)

\method{KGElf}{matrix}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue", "none"), 
               epsilon.value=NA, ...)
             
\method{KGElf}{zoo}(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
               epsilon.type=c("Pushpalatha2012", "otherFactor", "otherValue", "none"), 
               epsilon.value=NA, ...)
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

-) \kbd{2021}: the bias is defined as \sQuote{Beta}, the ratio of \code{mean(sim)} minus \code{mean(obs)} to the standard deviation of \code{obs}. The variability is defined as \sQuote{Alpha}, the ratio of the standard deviation of \code{sim} values to the standard deviation of \code{obs}. See Tang et al. (2021). 
}
  \item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. 

It is designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012). This is the default option.

2) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{FUN}.

3) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value.

}
  \item{epsilon.value}{
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{

Garcia et al. (2017) tested different objective functions and found that the mean value of the KGE applied to the streamflows (i.e., KGE(Q)) and the KGE applied to the inverse of the streamflows (i.e., KGE(1/Q) is able to provide a an aceptable representation of low-flow indices important for water management. They also found that KGE applied to a transformation of streamflow values (e.g., log)  is inadequate to capture low-flow indices important for water management. \cr

The robustness of their findings depends more on the climate variability rather than the objective function, and they are insensitive to the hydrological model used in the evaluation.

\deqn{KGE_{lf} =  \frac{KGE(Q) + KGE(1/Q)}{2} }{%
KGElf = 1 - sqrt[ (s[1]*(r-1))^2 + (s[2]*(vr-1))^2 + (s[3]*(Beta-1))^2] ; r=Pearson product-moment correlation coefficient ; alpha=sigma_s/sigma_o ; beta=mu_s/mu_o ; gamma=CV_s/CV_o}

Traditional Kling-Gupta efficiencies (Gupta et al., 2009; Kling et al., 2012) range from -Inf to 1 and, therefore, KGElf should also range from -Inf to 1. Essentially, the closer to 1, the more similar \code{sim} and \code{obs} are. \cr

Knoben et al. (2019) showed that traditional Kling-Gupta (Gupta et al., 2009; Kling et al., 2012) values greater than -0.41 indicate that a model improves upon the mean flow benchmark, even if the model's KGE value is negative.
}
\value{
numeric with the Kling-Gupta efficiency for low flows between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Kling-Gupta efficiency between each column of \code{sim} and \code{obs}
}
\references{

\cite{Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to calibrate rainfall-runoff models for low-flow index simulations?. Hydrological sciences journal, 62(7), 1149-1166. doi:10.1080/02626667.2017.1308511.}

\cite{Pushpalatha, R., Perrin, C., Le Moine, N. and Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420, 171-182. doi:10.1016/j.jhydrol.2011.11.055.}

\cite{Pfannerstill, M.; Guse, B.; Fohrer, N. (2014). Smart low flow signature metrics for an improved overall performance evaluation of hydrological models. Journal of Hydrology, 510, 447-458. doi:10.1016/j.jhydrol.2013.12.044.}

\cite{Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using log-transformed flows within the KGE criterion. doi:10.5194/hess-22-4583-2018}.

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
\code{\link{KGE}}, \code{\link{KGEnp}}, \code{\link{sKGE}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
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
#           on the first half of the observed values. 
#           This random noise has more relative importance for low flows than 
#           for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim <- obs 
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

# Computing 'KGElf'
KGElf(sim=sim, obs=obs)

##################
# Example 4: KGElf for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

KGElf(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
KGElf(sim=lsim, obs=lobs)

##################
# Example 5: KGElf for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

KGElf(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGElf(sim=lsim, obs=lobs)

##################
# Example 6: KGElf for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
KGElf(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGElf(sim=lsim, obs=lobs)

##################
# Example 7: KGElf for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
KGElf(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGElf(sim=lsim, obs=lobs)

##################
# Example 8: KGElf for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

KGElf(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
KGElf(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
