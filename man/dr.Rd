%% File dr.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2023-2024 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{dr}
\Rdversion{1.1}
\alias{dr}
\alias{dr.default}
\alias{dr.matrix}
\alias{dr.data.frame}
\alias{dr.zoo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Refined Index of Agreement
}
\description{
Refined Index of Agreement (dr) between \code{sim} and \code{obs}, with treatment of missing values.
}
\usage{
dr(sim, obs, ...)

\method{dr}{default}(sim, obs, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

\method{dr}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

\method{dr}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ...,
            epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
            epsilon.value=NA)

\method{dr}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ...,
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
  \item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Nash-Sutcliffe efficiency.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
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
 -) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
 -) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}
\details{
\deqn{c = 2}
\deqn{A = \sum_{i=1}^N {\left| S_i - O_i \right|} }
\deqn{B = c \sum_{i=1}^N {\left| O_i - \bar{O} \right|} }
\deqn{dr = 1 - \frac{A} { B } ; A \leq B}{%
dr = 1 - A / B ; A <= B } 
\deqn{dr = 1 - \frac{B} { A } ; A > B}{%
dr = 1 - B / A ; A > B } 

The Refined Index of Agreement (dr, Willmott et al., 2012) is a reformulation of the orginal Willmott's index of agreement developed in the 1980s (Willmott, 1981; Willmott, 1984; Willmott et al., 1985)

The Refined Index of Agreement (dr) is dimensionless, and it varies between -1 to 1 (in contrast to the original d, which varies in [0, 1]).

The Refined Index of Agreement (dr) is monotonically related with the modified Nash-Sutcliffe (E1) desribed in Legates and McCabe (1999).

In general, dr is more rationally related to model accuracy than are other existing indices (Willmott et al., 2012; Willmott et al., 2015). It also is quite flexible, making it applicable to a wide range of model-performance problems (Willmott et al., 2012)

}
\value{
Refined Index of Agreement (dr) between \code{sim} and \code{obs}. \cr

If \code{sim} and \code{obs} are matrixes or data.frames, the returned value is a vector, with the Refined Index of Agreement (dr) between each column of \code{sim} and \code{obs}.
}
\references{
\cite{Willmott, C.J., Robeson, S.M. and Matsuura, K. (2012). A refined index of model performance. International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419.}

\cite{Willmott, C.J., Robeson, S.M., Matsuura, K. and Ficklin, D.L. (2015). Assessment of three dimensionless measures of model performance. Environmental Modelling & Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012}

\cite{Willmott, C. J. (1981). On the validation of models. Physical Geography, 2, 184--194}

\cite{Willmott, C. J. (1984). On the evaluation of model performance in physical geography. Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460}

\cite{Willmott, C. J., S. G. Ackleson, R. E. Davis, J. J. Feddema, K. M. Klink, D. R. Legates, J. O'Donnell, and C. M. Rowe (1985), Statistics for the Evaluation and Comparison of Models, J. Geophys. Res., 90(C5), 8995-9005} 
}
\author{
Mauricio Zambrano Bigiarini <mzb.devel@gmail.com>
}
\note{
\code{obs} and \code{sim} has to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{d}}, \code{\link{md}}, \code{\link{rd}}, \code{\link{gof}}, \code{\link{ggof}}
}
\examples{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
dr(sim, obs)

obs <- 1:10
sim <- 2:11
dr(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'dr' for the "best" (unattainable) case
dr(sim=sim, obs=obs)

##################
# Example 3: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

dr(sim=sim, obs=obs)

##################
# Example 4: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

dr(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
dr(sim=lsim, obs=lobs)

##################
# Example 5: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

dr(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
dr(sim=lsim, obs=lobs)

##################
# Example 6: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
dr(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
dr(sim=lsim, obs=lobs)

##################
# Example 7: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
dr(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
dr(sim=lsim, obs=lobs)

##################
# Example 8: dr for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

dr(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
dr(sim=sim1, obs=obs1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
