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
Joint Divergence Kling-Gupta Efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This implementation follows the technical formulation described by Ficchi et al. (2026): the KGE' variability term is used, and the distributional component is computed from a histogram-based Jensen-Shannon divergence applied to log-transformed flows after paper-specific zero handling.

The function also allows JDKGE-style variants based on the 2009 and 2021 KGE formulations, while keeping the paper's 2012 variant as the default.
}

\usage{
JDKGE(sim, obs, ...)

\method{JDKGE}{default}(sim, obs, na.rm=TRUE, s=c(1,1,1,1),
        method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
        density.method=c("hist", "kde", "wasserstein"), nbins="paper",
        timestep=86400, kde.n.grid=512, wasserstein.n.quantiles=512, fun=NULL, ...,
        epsilon.type=c("otherValue", "none", "Pushpalatha2012", "otherFactor"),
        epsilon.value=NA)

\method{JDKGE}{data.frame}(sim, obs, na.rm=TRUE, s=c(1,1,1,1),
        method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
        density.method=c("hist", "kde", "wasserstein"), nbins="paper",
        timestep=86400, kde.n.grid=512, wasserstein.n.quantiles=512, fun=NULL, ...,
        epsilon.type=c("otherValue", "none", "Pushpalatha2012", "otherFactor"),
        epsilon.value=NA)

\method{JDKGE}{matrix}(sim, obs, na.rm=TRUE, s=c(1,1,1,1),
        method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
        density.method=c("hist", "kde", "wasserstein"), nbins="paper",
        timestep=86400, kde.n.grid=512, wasserstein.n.quantiles=512, fun=NULL, ...,
        epsilon.type=c("otherValue", "none", "Pushpalatha2012", "otherFactor"),
        epsilon.value=NA)

\method{JDKGE}{zoo}(sim, obs, na.rm=TRUE, s=c(1,1,1,1),
        method=c("2012", "2009", "2021"), out.type=c("single", "full"), 
        density.method=c("hist", "kde", "wasserstein"), nbins="paper",
        timestep=86400, kde.n.grid=512, wasserstein.n.quantiles=512, fun=NULL, ...,
        epsilon.type=c("otherValue", "none", "Pushpalatha2012", "otherFactor"),
        epsilon.value=NA)
}

\arguments{
\item{sim}{numeric, zoo, matrix or data.frame with simulated values.}

\item{obs}{numeric, zoo, matrix or data.frame with observed values.}

\item{na.rm}{logical value indicating whether missing paired values should be removed before computing the metric.}

\item{s}{numeric of length 4 with scaling factors for the Euclidean distance in criteria space. If \code{s} differs from \code{c(1,1,1,1)}, then \code{sum(s)} must be equal to 1.}

\item{method}{character string indicating the Kling-Gupta formulation used by JDKGE. Valid values are \kbd{"2012"} (default, the paper's KGE' formulation), \kbd{"2009"}, and \kbd{"2021"}.}

\item{out.type}{character string indicating the output format. Use \kbd{"single"} to return the JDKGE value only, or \kbd{"full"} to also return the four diagnostic components.}

\item{density.method}{character, representing the method used to compute the divergence component. \kbd{"hist"} uses the paper-faithful histogram-based Jensen-Shannon divergence, \kbd{"kde"} uses a common-grid kernel density estimate followed by Jensen-Shannon divergence, and \kbd{"wasserstein"} uses a Wasserstein-distance similarity on log-flows.}

\item{nbins}{character, representing the binning rule used by the histogram divergence component. The default \kbd{"paper"} uses the procedure described by Ficchi et al. (2026). This argument is ignored for \code{density.method="kde"} and \code{density.method="wasserstein"}.}

\item{timestep}{numeric, representing the sampling time step in seconds used by the paper's bin-count adjustment. For \code{zoo} inputs this is inferred from the time index when omitted. The default for plain numeric vectors is one day (86400 seconds).}

\item{kde.n.grid}{integer, number of grid points used when \code{density.method="kde"}. Larger values provide a finer common support grid at higher computational cost.}

\item{wasserstein.n.quantiles}{integer, number of quantile levels used to approximate the first Wasserstein distance when \code{density.method="wasserstein"}. Larger values provide a finer approximation at higher computational cost.}

\item{fun}{optional function applied to \code{sim} and \code{obs} before computing JDKGE. The first argument of \code{fun} must be a numeric vector.}

\item{\dots}{additional arguments passed to \code{fun}.}

\item{epsilon.type}{rule used for zero-flow handling in the internal log-based divergence component, and also passed to \code{preproc} when \code{fun} needs an offset before transformation. The default is \kbd{"otherValue"}.}

\item{epsilon.value}{numeric value used when \code{epsilon.type="otherValue"} or \code{epsilon.type="otherFactor"}. When \code{epsilon.type="otherValue"} and \code{epsilon.value=NA} (the default), the value described by Ficchi et al. (2026), \eqn{\epsilon = \min(10^{-6}, 10^{-1} \min(c))}, is computed internally.}
}

\details{
JDKGE combines four components:

\enumerate{
\item the Pearson correlation coefficient \eqn{r},
\item the variability term, defined as \eqn{\gamma = (\sigma_s / \mu_s) / (\sigma_o / \mu_o)} for \kbd{method="2012"} and as \eqn{\alpha = \sigma_s / \sigma_o} for \kbd{method="2009"} and \kbd{method="2021"},
\item the bias term, defined as \eqn{\beta = \mu_s / \mu_o} for \kbd{method="2009"} and \kbd{method="2012"}, and as \eqn{\beta_{2021} = (\mu_s - \mu_o)/\sigma_o} for \kbd{method="2021"}, and
\item the distributional similarity component \eqn{\Delta}.
}

For the divergence component, this implementation follows the paper's workflow:

\enumerate{
\item exact zeros are replaced according to \code{epsilon.type}. With the default \kbd{"otherValue"} and \code{epsilon.value=NA}, \eqn{\epsilon = \min(10^{-6}, 10^{-1} \min(c))}, where \eqn{c} is the set of strictly positive simulated and observed values,
\item the transformed values \eqn{\log(x)} are binned using a histogram,
\item the Freedman-Diaconis width is lower-bounded by \eqn{h_{min} = \min(10^2 \epsilon, 10^{-1})},
\item the number of bins is adjusted by the time-scale factor and clipped to the interval \eqn{[25, 100]},
\item additive smoothing with \eqn{\alpha = \epsilon} is applied to the empirical densities, and
\item Jensen-Shannon divergence is computed with base-2 logarithms.
}

For \code{density.method="kde"}, simulated and observed log-flows are smoothed with Gaussian kernel density estimates evaluated on a common grid over the pooled support using a shared bandwidth estimated from the pooled sample. Jensen-Shannon divergence is then computed from the two resulting probability vectors.

For \code{density.method="wasserstein"}, the log-flow distributions are compared with the first Wasserstein distance computed from empirical quantiles. The resulting distance is converted into a similarity component through
\deqn{\Delta = \exp(-W_1 / s_w)}
where \eqn{s_w} is a robust scale estimated from the pooled log-flows using the interquartile range, with fallback to the standard deviation when needed.

The metric is then computed as:

\deqn{JDKGE = 1 - \sqrt{(s[1](r-1))^2 + (s[2](vr-1))^2 + (s[3](br-1))^2 + (s[4](\Delta-1))^2}}

Joint Divergence Kling-Gupta efficiencies range from \eqn{-\infty} to 1. Values closer to 1 indicate stronger agreement between simulated and observed values across correlation, variability, bias, and distributional similarity.
}

\value{
If \code{out.type="single"}: numeric with the Joint Divergence Kling-Gupta Efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output is a vector with one efficiency value per column pair.

If \code{out.type="full"}: a list with two elements:

\item{JDKGE.value}{numeric with the Joint Divergence Kling-Gupta Efficiency.}
\item{JDKGE.elements}{numeric with 4 elements: \sQuote{r}, the selected bias term, the selected variability term, and \sQuote{Delta}.}
}

\references{
\cite{Ficchi, A.; Bavera, D.; Grimaldi, S.; Moschini, F.; Pistocchi, A.; Russo, C.; Salamon, P.; Toreti, A. (2026). Improving low and high flow simulations at once: An enhanced metric for hydrological model calibrations. EGUsphere [preprint], https://doi.org/10.5194/egusphere-2026-43.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper Danube basin under an ensemble of climate change scenarios. Journal of Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.}

\cite{Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). SC-earth: a station-based serially complete earth dataset from 1950 to 2019. Journal of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.}
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
JDKGE(sim=sim, obs=obs, density.method="kde", kde.n.grid=1024)
JDKGE(sim=sim, obs=obs, density.method="wasserstein")
JDKGE(sim=sim, obs=obs, density.method="wasserstein", wasserstein.n.quantiles=1024)

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
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

JDKGE(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
JDKGE(sim=sim1, obs=obs1)

##################
# Example 7: JDKGE for a two-column data frame where simulated values are equal to 
#            observations plus random noise on the first half of the observed values 

SIM <- cbind(sim, sim)
OBS <- cbind(obs, obs)

JDKGE(sim=SIM, obs=OBS)
}
