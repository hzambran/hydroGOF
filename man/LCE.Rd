%% File LCE.Rd
%% Part of the hydroGOF R package, https://github.com/hzambran/hydroGOF ;
%% https://cran.r-project.org/package=hydroGOF
%% http://www.rforge.net/hydroGOF/
%% Copyright 2026-2026 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or later

\name{LCE}
\Rdversion{1.1}
\alias{LCE}
\alias{LCE.default}
\alias{LCE.matrix}
\alias{LCE.data.frame}
\alias{LCE.zoo}

\title{
Lee and Choi Efficiency
}

\description{
Lee and Choi Efficiency between \code{sim} and \code{obs}, with treatment of missing values.

This goodness-of-fit measure was proposed by Lee and Choi (2022) as an alternative to the Liu-Mean Efficiency (LME), designed to provide a multi-dimensional and diagnostically balanced evaluation of model performance; by jointly considering correlation, variability and bias; whereas LME is fundamentally a single-error-based metric.   

Unlike some single-error-based criteria, LCE explicitly combines the correlation coefficient and the variability ratio through two complementary terms, \code{r*Alpha} and \code{r/Alpha}, in order to penalize inconsistent representations of timing and variability.

For a short description of its components and the numeric range of values, please see Details.
}

\usage{
LCE(sim, obs, ...)

\method{LCE}{default}(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LCE}{data.frame}(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LCE}{matrix}(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

\method{LCE}{zoo}(sim, obs, na.rm=TRUE, out.type=c("single", "full"),
             fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)
}

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
\item{out.type}{
character, indicating whether the output of the function has to include each one of the terms used in the computation of the Lee and Choi Efficiency or not. Valid values are:

-) \kbd{single}: the output is a numeric with the Lee and Choi Efficiency only.

-) \kbd{full}: the output is a list of two elements: the first one with the Lee and Choi Efficiency, and the second is a numeric with 5 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the variability ratio (\sQuote{Alpha}), the bias ratio (\sQuote{Beta}), the product between correlation and variability (\sQuote{rAlpha}), and the ratio between correlation and variability (\sQuote{rOverAlpha}).
}
\item{fun}{
function to be applied to \code{sim} and \code{obs} in order to obtain transformed values thereof before computing the Lee and Choi Efficiency.

The first argument MUST BE a numeric vector with any name (e.g., \code{x}), and additional arguments are passed using \code{\dots}.
}
\item{\dots}{
arguments passed to \code{fun}, in addition to the mandatory first numeric vector.
}
\item{epsilon.type}{
argument used to define a numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}.

It was designed to allow the use of logarithm and other similar functions that do not work with zero values.

Valid values of \code{epsilon.type} are:

1) \kbd{"none"}: \code{sim} and \code{obs} are used by \code{fun} without the addition of any numeric value.

2) \kbd{"Pushpalatha2012"}: one hundredth (1/100) of the mean observed values is added to both \code{sim} and \code{obs} before applying \code{fun}, as described in Pushpalatha et al. (2012).

3) \kbd{"otherFactor"}: the numeric value defined in the \code{epsilon.value} argument is used to multiply the mean observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs}, before applying \code{fun}.

4) \kbd{"otherValue"}: the numeric value defined in the \code{epsilon.value} argument is directly added to both \code{sim} and \code{obs}, before applying \code{fun}.
}
\item{epsilon.value}{
-) when \code{epsilon.type="otherValue"} it represents the numeric value to be added to both \code{sim} and \code{obs} before applying \code{fun}. \cr
-) when \code{epsilon.type="otherFactor"} it represents the numeric factor used to multiply the mean of the observed values, instead of the one hundredth (1/100) described in Pushpalatha et al. (2012). The resulting value is then added to both \code{sim} and \code{obs} before applying \code{fun}.
}
}

\details{
In the computation of this index, there are three main diagnostic components involved:

1) \kbd{r} : the Pearson product-moment correlation coefficient. Ideal value is r=1.

2) \kbd{Alpha} : the variability ratio, defined as the ratio between the standard deviation of the simulated values and the standard deviation of the observed ones. Ideal value is Alpha=1.

3) \kbd{Beta} : the bias ratio, defined as the ratio between the mean of the simulated values and the mean of the observed ones. Ideal value is Beta=1.

The Lee and Choi Efficiency combines these terms through two additional correlation-variability components:

\deqn{r\alpha}

and

\deqn{r/\alpha}

Both terms have an ideal value of 1. The first term, \eqn{r\alpha}, penalizes cases where the combined effect of correlation and variability is inconsistent with the observed dynamics. The second term, \eqn{r/\alpha}, penalizes the opposite imbalance between correlation and variability. Together, these two terms make LCE sensitive to compensating errors between timing and dispersion.

Lee and Choi efficiencies range from -Inf to 1. Essentially, the closer to 1, the more similar \code{sim} and \code{obs} are in terms of timing, variability and bias.

\deqn{LCE = 1 - ED}

\deqn{
ED = \sqrt{ (r\alpha - 1)^2 + (r/\alpha - 1)^2 + (\beta - 1)^2 }
}

where:

\deqn{r=Pearson product-moment correlation coefficient}

\deqn{\alpha=\sigma_s/\sigma_o}

\deqn{\beta=\mu_s/\mu_o}

\deqn{r\alpha=r\times\alpha}

\deqn{r/\alpha=r/\alpha}

where \eqn{\mu_s} and \eqn{\mu_o} are the mean simulated and observed values, respectively, and \eqn{\sigma_s} and \eqn{\sigma_o} are their corresponding standard deviations.

A value of \kbd{LCE=1} indicates perfect agreement between simulated and observed values. Values close to 1 indicate strong agreement across correlation, variability and bias, whereas negative values indicate progressively poorer model performance.
}

\value{
If \code{out.type=single}: numeric with the Lee and Choi Efficiency between \code{sim} and \code{obs}. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Lee and Choi Efficiency between each column of \code{sim} and \code{obs}.

If \code{out.type=full}: a list of two elements:
\item{LCE.value}{
numeric with the Lee and Choi Efficiency. If \code{sim} and \code{obs} are matrices, the output value is a vector, with the Lee and Choi Efficiency between each column of \code{sim} and \code{obs}
}
\item{LCE.elements}{
numeric with 5 elements: the Pearson product-moment correlation coefficient (\sQuote{r}), the variability ratio (\sQuote{Alpha}), the bias ratio (\sQuote{Beta}), the product between correlation and variability (\sQuote{rAlpha}), and the ratio between correlation and variability (\sQuote{rOverAlpha}). If \code{sim} and \code{obs} are matrices, the output value is a matrix, with the previous five elements computed for each column of \code{sim} and \code{obs}\cr
}
}

\references{
\cite{Lee, J. S., & Choi, H. I. (2022). A rebalanced performance criterion for hydrological model calibration. Journal of Hydrology, 606, 127372. https://doi.org/10.1016/j.jhydrol.2021.127372}

\cite{Liu, D. (2020). A rational performance criterion for hydrological model. Journal of Hydrology, 590, 125488. https://doi.org/10.1016/j.jhydrol.2020.125488}

\cite{Choi, H. I. (2021). Comment on Liu (2020): A rational performance criterion for hydrological model. Journal of Hydrology, 606, 126927. https://doi.org/10.1016/j.jhydrol.2021.126927}

\cite{Liu, D. (2021). Reply to "Comment on Liu (2020): A rational performance criterion for a hydrological model" by HyunIl Choi. Journal of Hydrology, 603, 126935. https://doi.org/10.1016/j.jhydrol.2021.126927}

\cite{Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009). Decomposition of the mean squared error and NSE performance criteria: Implications for improving hydrological modelling. Journal of Hydrology, 377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003.}

\cite{Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A review of efficiency criteria suitable for evaluating low-flow simulations. Journal of Hydrology, 420-421, 171-182. doi:10.1016/j.jhydrol.2011.11.055.}
}

\author{
Mauricio Zambrano-Bigiarini <mzb.devel@gmail.com>
}

\note{
\code{obs} and \code{sim} have to have the same length/dimension \cr

The missing values in \code{obs} and \code{sim} are removed before the computation proceeds, and only those positions with non-missing values in \code{obs} and \code{sim} are considered in the computation.
}

\seealso{
\code{\link{LME}}, \code{\link{KGE}}, \code{\link{NSE}}, \code{\link{gof}}, \code{\link{ggof}}
}

\examples{
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
LCE(sim, obs)

obs <- 1:10
sim <- 2:11
LCE(sim, obs)

##################
# Example2: simulated values equal to observations plus random noise

set.seed(123)
obs <- 1:100
sim <- obs + rnorm(100, mean=0, sd=5)

LCE(sim=sim, obs=obs)

LCE(sim=sim, obs=obs, out.type="full")

##################
# Example3: applying logarithmic transformation

LCE(sim=sim, obs=obs, fun=log)

##################
# Example4: applying logarithmic transformation with Pushpalatha constant

LCE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

}
\keyword{ math }
