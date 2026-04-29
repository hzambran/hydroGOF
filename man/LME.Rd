%% File KGEkm.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2026-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

%% File LME.Rd
%% Part of the hydroGOF R package
%% Copyright 2024
%% Distributed under GPL 2 or later

\name{LME}
\Rdversion{1.1}
\alias{LME}
\alias{LME.default}
\alias{LME.matrix}
\alias{LME.data.frame}
\alias{LME.zoo}

\title{
Liu-Mean Efficiency
}

\description{
Liu-Mean Efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This goodness-of-fit measure was proposed by Liu et al. (2020) as an alternative to the Nash-Sutcliffe efficiency (NSE), designed to provide a more balanced assessment of model performance by normalising the mean squared error using the mean of the observed values instead of their variance.

The Liu-Mean Efficiency emphasizes proportional error relative to the mean magnitude of the observed variable, making it particularly useful in hydrological applications where the mean value is a meaningful scale for evaluating prediction accuracy.

For a short description of the metric and the numeric range of values, please see Details.
}

\usage{
LME(sim, obs, ...)

\method{LME}{default}(sim, obs, na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LME}{data.frame}(sim, obs, na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LME}{matrix}(sim, obs, na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LME}{zoo}(sim, obs, na.rm=TRUE, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)
}

\arguments{

\item{sim}{
numeric, zoo, matrix or data.frame with simulated values.
}

\item{obs}{
numeric, zoo, matrix or data.frame with observed values.
}

\item{na.rm}{
a logical value indicating whether 'NA' should be stripped before the computation proceeds. \cr
When an 'NA' value is found at the i-th position in \code{obs} \bold{OR} \code{sim}, the i-th value of \code{obs} \bold{AND} \code{sim} are removed before the computation.
}

\item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Liu-Mean Efficiency.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}

\item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
}

\item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}.

It is designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012).

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the mean observed values. The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}

\item{epsilon.value}{
-) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
-) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values. The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}

}

\details{

The Liu-Mean Efficiency (LME) is based on the mean squared error (MSE) normalized by the squared mean of the observed values.

Its formulation is conceptually similar to the Nash-Sutcliffe efficiency, but uses the mean of observations as the reference scaling factor instead of the variance. This modification reduces the sensitivity of the metric to high variability and makes the performance evaluation more directly interpretable in terms of proportional error relative to the mean magnitude of the observed variable.

The Liu-Mean Efficiency ranges from -Inf to 1.

A value of:

- \kbd{LME = 1} indicates perfect agreement between \code{sim} and \code{obs}.
- \kbd{LME = 0} indicates that the mean squared error equals the squared mean of the observed values.
- \kbd{LME < 0} indicates that the model predictions are worse than the squared mean of the observed values.

Essentially, the closer the LME value is to 1, the more similar \code{sim} and \code{obs} are.

\deqn{LME = 1 - \frac{MSE}{\mu_o^2}}

\deqn{MSE = \frac{1}{n} \sum_{i=1}^{n} (sim_i - obs_i)^2}

\deqn{\mu_o = \frac{1}{n} \sum_{i=1}^{n} obs_i}

where:

\itemize{

\item \kbd{n} is the number of paired observations

\item \kbd{sim_i} is the simulated value at time step i

\item \kbd{obs_i} is the observed value at time step i

\item \kbd{\mu_o} is the mean of the observed values

}

}

\value{

numeric with the Liu-Mean Efficiency between \code{sim} and \code{obs}.

If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Liu-Mean Efficiency between each column of \code{sim} and \code{obs}.

}

\references{

\cite{Liu, D.; Chen, X.; Lian, Y.; Lou, Z. (2020). A new performance measure for hydrologic models. Journal of Hydrology, 590, 125488. doi:10.1016/j.jhydrol.2020.125488.}

\cite{Nash, J.E.; Sutcliffe, J.V. (1970). River flow forecasting through conceptual models part I — A discussion of principles. Journal of Hydrology, 10(3), 282-290.}

\cite{Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420-421, 171-182.}

}

\author{
Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}

\note{

\code{obs} and \code{sim} have to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation

}

\seealso{
\code{\link{me}}, \code{\link{pbias}},\code{\link{NSE}}, \code{\link{KGE}}, \code{\link{gof}}, \code{\link{ggof}}
}

\examples{

# Example 0: basic ideal case
obs <- 1:10
sim <- 1:10
LME(sim, obs)

obs <- 1:10
sim <- 2:11
LME(sim, obs)

##################
# Example 1: Looking at the difference between LME and KGE, both with 'method=2009' 
#            and 'method=2012'

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")

# LME (Liu et al., 2020):
LME(sim=sim, obs=obs, method="2012")

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'LME' for the "best" (unattainable) case
LME(sim=sim, obs=obs)

##################
# Example 3: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

LME(sim=sim, obs=obs)

##################
# Example 4: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

LME(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
LME(sim=lsim, obs=lobs)

##################
# Example 5: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

LME(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
LME(sim=lsim, obs=lobs)

##################
# Example 6: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
LME(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
LME(sim=lsim, obs=lobs)

##################
# Example 7: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
LME(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
LME(sim=lsim, obs=lobs)

##################
# Example 8: LME for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

LME(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
LME(sim=sim1, obs=obs1)

##################
# Example 9: LME for a two-column data frame where simulated values are equal to 
#            observations plus random noise on the first half of the observed values 

SIM <- cbind(sim, sim)
OBS <- cbind(obs, obs)

LME(sim=SIM, obs=OBS)

##################
# Example 10: LME for each year, where simulated values are given in a two-column data 
#             frame equal to the observations plus random noise on the first half of the 
#             observed values 
SIM <- cbind(sim, sim)
OBS <- cbind(obs, obs)
LME(sim=SIM, obs=OBS, out.PerYear=TRUE)


}

\keyword{ math }
