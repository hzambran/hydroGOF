%% File dr.Rd
%% Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
%%                                 http://cran.r-project.org/web/packages/hydroGOF/
%% Copyright 2011-2014 Mauricio Zambrano-Bigiarini
%% Distributed under GPL 2 or lat

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
This function computes the Refined Index of Agreement (dr) between \code{sim} and \code{obs}, with treatment of missing values.

If \code{x} is a matrix or a data frame, a vector with the Refined Index of Agreement of each column is returned.
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

The first two arguments of \code{FUN} MUST BE NAMED \kbd{sim} and \kbd{obs} (in that specific order), and addtional arguments are passed using \code{\dots}.
}
  \item{\dots}{
arguments passed to \code{FUN}, in addition to the mandatory \kbd{sim} and \kbd{obs}.
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
\cite{Willmott, C.J., Robeson, S.M. and Matsuura, K. (2012). A refined index of model performance. International Journal of climatology, 32(13), pp.2088-2094. doi:10.1002/joc.2419.} \cr

\cite{Willmott, C.J., Robeson, S.M., Matsuura, K. and Ficklin, D.L. (2015). Assessment of three dimensionless measures of model performance. Environmental Modelling & Software, 73, pp.167-174. doi:10.1016/j.envsoft.2015.08.012} \cr

\cite{Willmott, C. J. (1981). On the validation of models. Physical Geography, 2, 184--194}  \cr

\cite{Willmott, C. J. (1984). On the evaluation of model performance in physical geography. Spatial Statistics and Models, G. L. Gaile and C. J. Willmott, eds., 443-460} \cr

\cite{Willmott, C. J., S. G. Ackleson, R. E. Davis, J. J. Feddema, K. M. Klink, D. R. Legates, J. O'Donnell, and C. M. Rowe (1985), Statistics for the Evaluation and Comparison of Models, J. Geophys. Res., 90(C5), 8995-9005} \cr

\cite{Legates, D. R. and G. J. McCabe Jr. (1999), Evaluating the Use of "Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model Validation, Water Resources Research, 35(1), 233-241. doi:10.1029/1998WR900018}

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
obs <- 1:10
sim <- 1:10
dr(sim, obs)

obs <- 1:10
sim <- 2:11
dr(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the index of agreement for the "best" (unattainable) case
dr(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new index of agreement
dr(sim=sim, obs=obs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
