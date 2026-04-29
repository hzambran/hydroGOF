%% File JDKGE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ; 
%%                                 https://cran.r-project.org/package=hydroGOF
%%                                 http://www.rforge.net/hydroGOF/
%% Copyright 2026-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later


\name{JDKGE}
\Rdversion{1.1}
\alias{JDKGE}
\alias{JDKGE.default}
\alias{JDKGE.matrix}
\alias{JDKGE.data.frame}
\alias{JDKGE.zoo}

\title{
Joint Divergence Kling-Gupta Efficiency
}

\description{

Joint Divergence Kling-Gupta efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This goodness-of-fit measure extends the traditional Kling-Gupta efficiency by incorporating a fourth diagnostic component that evaluates the similarity between the probability distributions of simulated and observed values. This additional component is based on a divergence measure computed from the logarithm of flows, allowing the metric to explicitly account for differences in distributional shape, including the representation of low and high flows.

The resulting index provides a more comprehensive evaluation of hydrological model performance by jointly considering correlation, variability, bias, and distributional similarity.

For a short description of its four components and the numeric range of values, please see Details.

}

\usage{

JDKGE(sim, obs, ...)

\method{JDKGE}{default}(sim, obs, s=c(1,1,1,1), na.rm=TRUE, method=c("2009","2012","2021"),
              out.type=c("single", "full"), fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
              epsilon.value=NA, density.method=c("hist","kde"), nbins="Sturges" )

\method{JDKGE}{data.frame}(sim, obs, s=c(1,1,1,1), na.rm=TRUE, method=c("2009","2012","2021"),
              out.type=c("single", "full"), fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
              epsilon.value=NA, density.method=c("hist","kde"), nbins="Sturges" )

\method{JDKGE}{matrix}(sim, obs, s=c(1,1,1,1), na.rm=TRUE, method=c("2009","2012","2021"),
              out.type=c("single", "full"), fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
              epsilon.value=NA, density.method=c("hist","kde"), nbins="Sturges" )

\method{JDKGE}{zoo}(sim, obs, s=c(1,1,1,1), na.rm=TRUE, method=c("2009","2012","2021"),
              out.type=c("single", "full"), fun=NULL, ..., 
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
              epsilon.value=NA, density.method=c("hist","kde"), nbins="Sturges" )

}

\arguments{

\item{sim}{
numeric, zoo, matrix or data.frame with simulated values
}

\item{obs}{
numeric, zoo, matrix or data.frame with observed values
}

\item{s}{
numeric of length 4, representing the scaling factors to be used for re-scaling the criteria space before computing the Euclidean distance from the ideal point c(1,1,1,1), i.e., \code{s} elements are used for adjusting the emphasis on different components.

The first element is used for rescaling the Pearson product-moment correlation coefficient (\code{r}), the second element is used for rescaling the variability ratio (\code{Alpha}), the third element is used for rescaling the bias ratio (\code{Beta}), and the fourth element is used for rescaling the distribution similarity component (\code{Delta}).
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
\item{out.type}{
character, indicating whether the output of the function has to include each one of the four terms used in the computation of the Joint Divergence Kling-Gupta efficiency or not. Valid values are:

-) \kbd{single}: the output is a numeric with the Joint Divergence Kling-Gupta efficiency only.

-) \kbd{full}: the output is a list of two elements: the first one with the Joint Divergence Kling-Gupta efficiency, and the second is a numeric with 4 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the variability ratio (\sQuote{Alpha}), the bias ratio (\sQuote{Beta}), and the distribution similarity component (\sQuote{Delta}).
}

\item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Joint Divergence Kling-Gupta efficiency.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}

\item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.

When \code{density.method="kde"}, additional arguments are also passed to the \code{density} function used for kernel density estimation (e.g., bandwidth selection).
}

\item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}.

It was designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}.

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the mean observed values. The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}

\item{epsilon.value}{
numeric value used when \code{epsilon.type="otherValue"} or \code{epsilon.type="otherFactor"}.
}

\item{density.method}{
character, indicating the method used to estimate the empirical probability density functions required to compute the divergence component. Valid values are:

-) \kbd{"hist"}: probability densities are estimated using histograms. This is the default option.

-) \kbd{"kde"}: probability densities are estimated using kernel density estimation.

See 'Details' section for additional information.
}

\item{nbins}{
numeric or character, indicating the number of bins or the method used to determine the number of bins when \code{density.method="hist"}. Default is \kbd{"Sturges"}.
}

}

\details{

In the computation of this index, four diagnostic components are considered:

1) \kbd{r} : the Pearson product-moment correlation coefficient computed from the raw (unsorted) simulated and observed values. The ideal value is r=1.

2) \kbd{vr}: the variability component, whose definition depends on the selected \code{method}:

\itemize{

\item For \kbd{method="2009"} (Gupta et al., 2009), variability is defined as \sQuote{Alpha}, the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones:

\deqn{\alpha = \sigma_s / \sigma_o}

\item For \kbd{method="2012"} (Kling et al., 2012), variability is defined as \sQuote{Gamma}, the ratio between the coefficients of variation of simulated and observed values:

\deqn{\gamma = (\sigma_s / \mu_s) / (\sigma_o / \mu_o)}

\item For \kbd{method="2021"} (Tang et al., 2021), variability is again defined as \sQuote{Alpha}, the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones:

\deqn{\alpha = \sigma_s / \sigma_o}

}

The ideal value of the variability component is 1.

3) \kbd{br} : the bias component, whose definition also depends on the selected \code{method}:

\itemize{

\item For \kbd{method="2009"} and \kbd{method="2012"}, bias is defined as \sQuote{Beta}, the ratio between the mean of the simulated values and the mean of the observed ones:

\deqn{\beta = \mu_s / \mu_o}

\item For \kbd{method="2021"}, bias is defined as a standardized difference between means, denoted here as \sQuote{Beta.2021}:

\deqn{\beta_{2021} = (\mu_s - \mu_o) / \sigma_o}

}

The ideal value of the bias component is 1 for ratio-based definitions and 0 for the standardized bias formulation; however, within the unified JDKGE formulation the optimal point in the criteria space remains (1, 1, 1, 1).

4) \kbd{Delta} : the distribution similarity component, derived from the Jensen-Shannon divergence (JSD) computed from the logarithm of simulated and observed values:

\deqn{\Delta = 1 - JSD}

The Jensen-Shannon divergence quantifies the difference between the empirical probability distributions of log-transformed simulated and observed values, allowing the metric to explicitly evaluate discrepancies in distributional shape, including differences in low-flow and high-flow regimes that may not be fully captured by moment-based statistics alone.

The Joint Divergence Kling-Gupta Efficiency is defined as:

\deqn{
JDKGE = 1 - ED
}

where the Euclidean distance in the criteria space is:

\deqn{
ED =
\sqrt{ (s[1] (r-1))^2 + (s[2] (vr-1))^2 + (s[3] (br-1))^2 + (s[4] (\Delta-1))^2
}
}

Joint Divergence Kling-Gupta efficiencies range from -Inf to 1. Values closer to 1 indicate stronger agreement between simulated and observed values across correlation, variability, bias, and distributional similarity. Values close to zero indicate substantial deviations from ideal agreement, while increasingly negative values reflect progressively poorer model performance relative to the observed hydrological dynamics.

Regarding computational aspects, the choice of \code{density.method} influences both numerical cost and smoothness of the estimated distributions. 
The option \kbd{"hist"} relies on histogram-based density estimation and is computationally efficient, deterministic, and generally suitable for large-scale calibration or Monte Carlo simulations. The option \kbd{"kde"} relies on kernel density estimation, which produces smoother probability density functions and may provide a more stable estimate of distributional divergence, but typically requires greater computational time and memory resources. Consequently, \kbd{"hist"} is recommended for routine model calibration workflows, whereas \kbd{"kde"} may be preferable for diagnostic analyses or research applications where detailed characterization of distributional differences is required.

}

\value{

If \code{out.type=single}: numeric with the Joint Divergence Kling-Gupta efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the efficiency computed between each column of \code{sim} and \code{obs}.

If \code{out.type=full}: a list of two elements:

\item{JDKGE.value}{
numeric with the Joint Divergence Kling-Gupta efficiency.
}

\item{JDKGE.elements}{
numeric with 4 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the variability ratio (\sQuote{Alpha}), the bias ratio (\sQuote{Beta}), and the distribution similarity component (\sQuote{Delta}).
}

}

\references{

\cite{
Ficchi, A.; Bavera, D.; Grimaldi, S.; Moschini, F.; Pistocchi, A.; Russo, C.; Salamon, P.; Toreti, A. (2026). Improving low and high flow simulations at once: An enhanced metric for hydrological model calibrations. EGUsphere [preprint], https://doi.org/10.5194/egusphere-2026-43. 
}

\cite{Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). SC-earth: a station-based serially complete earth dataset from 1950 to 2019. Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.}

}

\author{
Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}

\note{

\code{obs} and \code{sim} have to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation

}

\seealso{

\code{\link{APFB}}, \code{\link{NSE}}, \code{\link{wNSE}}, \code{\link{wsNSE}}, \code{\link{HFB}}, \code{\link{PMR}}, \code{\link{gof}}, \code{\link{ggof}}

}

\examples{

# Example 0.1: ideal case
obs <- 1:10
sim <- 1:10

JDKGE(sim, obs)

##################
# Example 1: simulated values equal to twice the observations
sim <- 2*obs
JDKGE(sim=sim, obs=obs, out.type="full")

##################
# Example 2: using kernel density estimation, instead of histograms (the default)
JDKGE(sim=sim, obs=obs, density.method="kde")

##################
# Example 3: Looking at the difference between JDKGE and KGE, both with 'method=2009' 
#            and 'method=2012'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")

# JDKGE (Ficchi et al., 2026):
JDKGE(sim=sim, obs=obs, method="2009", out.type="full")

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")

# JDKGE (Ficchi et al., 2026):
JDKGE(sim=sim, obs=obs, method="2012", out.type="full")

# KGE 2021
KGE(sim=sim, obs=obs, method="2021", out.type="full")

# JDKGE (Ficchi et al., 2026):
JDKGE(sim=sim, obs=obs, method="2021", out.type="full")

##################
# Example 4: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'JDKGE' for the "best" (unattainable) case
JDKGE(sim=sim, obs=obs)

##################
# Example 5: JDKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

JDKGE(sim=sim, obs=obs)

##################
# Example 6: JDKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

JDKGE(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
JDKGE(sim=lsim, obs=lobs)

##################
# Example 7: JDKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

JDKGE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
JDKGE(sim=lsim, obs=lobs)

##################
# Example 8: JDKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
JDKGE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
JDKGE(sim=lsim, obs=lobs)

##################
# Example 9: JDKGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
JDKGE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
JDKGE(sim=lsim, obs=lobs)

##################
# Example 10: JDKGE for simulated values equal to observations plus random noise 
#             on the first half of the observed values and applying a 
#             user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

JDKGE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
JDKGE(sim=sim1, obs=obs1)

##################
# Example 11: JDKGE for a two-column data frame where simulated values are equal to 
#             observations plus random noise on the first half of the observed values 

SIM <- cbind(sim, sim)
OBS <- cbind(obs, obs)

JDKGE(sim=SIM, obs=OBS)

}

\keyword{ math }
