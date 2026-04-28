%% File KGE.Rd
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

\method{JDKGE}{default}(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

\method{JDKGE}{data.frame}(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

\method{JDKGE}{matrix}(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

\method{JDKGE}{zoo}(sim, obs,
s=c(1,1,1,1),
na.rm=TRUE,
out.type=c("single","full"),
fun=NULL, ...,
epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
epsilon.value=NA,
density.method=c("hist","kde"),
nbins="Sturges")

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
}

\item{nbins}{
numeric or character, indicating the number of bins or the method used to determine the number of bins when \code{density.method="hist"}. Default is \kbd{"Sturges"}.
}

}

\details{

In the computation of this index, there are four main components involved:

1) \kbd{r} : the Pearson product-moment correlation coefficient. Ideal value is r=1.

2) \kbd{Alpha} : the variability ratio, defined as the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Ideal value is Alpha=1.

3) \kbd{Beta} : the bias ratio, defined as the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1.

4) \kbd{Delta} : the distribution divergence component, derived from the Jensen-Shannon divergence (JSD) computed from the logarithm of simulated and observed values. Ideal value is Delta=1.

Joint Divergence Kling-Gupta efficiencies range from -Inf to 1. Essentially, the closer to 1, the more similar \code{sim} and \code{obs} are across correlation, variability, bias, and distributional characteristics. Values close to zero indicate substantial deviations from the ideal agreement, while increasingly negative values reflect progressively poorer model performance relative to the observed dynamics.

\deqn{JDKGE = 1 - ED}

\deqn{
ED = \sqrt{
(s[1]*(r-1))^2 +
(s[2]*(\alpha-1))^2 +
(s[3]*(\beta-1))^2 +
(s[4]*(\Delta-1))^2
}
}

\deqn{r=Pearson product-moment correlation coefficient}

\deqn{\alpha=\sigma_s/\sigma_o}

\deqn{\beta=\mu_s/\mu_o}

\deqn{\Delta=1 - JSD}

where:

JSD = Jensen-Shannon divergence between the empirical probability distributions of log-transformed simulated and observed values.

The distribution divergence component allows the metric to explicitly evaluate differences in the overall distributional behavior of simulated and observed series, including discrepancies in low-flow and high-flow regimes that may not be fully captured by moment-based statistics alone.

Regarding computational aspects, the choice of \code{density.method} influences both numerical cost and smoothness of the estimated distributions. The option \kbd{"hist"} relies on histogram-based density estimation and is computationally efficient, deterministic, and generally suitable for large-scale calibration or Monte Carlo simulations. The option \kbd{"kde"} relies on kernel density estimation, which produces smoother probability density functions and may provide a more stable estimate of distributional divergence, but typically requires greater computational time and memory resources. Consequently, \kbd{"hist"} is recommended for routine model evaluation and calibration workflows, whereas \kbd{"kde"} may be preferable for diagnostic analyses or research applications where detailed characterization of distributional differences is required.

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
Joint Divergence Kling-Gupta Efficiency performance metric for hydrological model evaluation.
EGUsphere preprint.
doi:10.5194/egusphere-2026-43
}

}

\author{

Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>

}

\note{

\code{obs} and \code{sim} have to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation

}

\seealso{

\code{\link{KGE}}, \code{\link{NSE}}, \code{\link{gof}}, \code{\link{ggof}}

}

\examples{

# Example 1: ideal case
obs <- 1:10
sim <- 1:10

JDKGE(sim, obs)

##################
# Example 2: simulated values equal to twice the observations

sim <- 2*obs

JDKGE(sim=sim, obs=obs, out.type="full")

##################
# Example 3: using logarithmic transformation

JDKGE(sim=sim, obs=obs, fun=log)

##################
# Example 4: using kernel density estimation

JDKGE(sim=sim, obs=obs,
      density.method="kde")

}

\keyword{ math }
