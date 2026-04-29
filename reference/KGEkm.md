# Kling-Gupta Efficiency with knowable-moments

Kling-Gupta efficiency between `sim` and `obs`, with use of knowable
moments and treatment of missing values.

This goodness-of-fit measure was developed by Pizarro and Jorquera
(2024), as a modification to the original Kling-Gupta efficiency (KGE)
proposed by Gupta et al. (2009). See Details.

## Usage

``` r
KGEkm(sim, obs, ...)

# Default S3 method
KGEkm(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'data.frame'
KGEkm(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'matrix'
KGEkm(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
# S3 method for class 'zoo'
KGEkm(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2012", "2009", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- s:

  numeric of length 3, representing the scaling factors to be used for
  re-scaling the criteria space before computing the Euclidean distance
  from the ideal point c(1,1,1), i.e., `s` elements are used for
  adjusting the emphasis on different components. The first elements is
  used for rescaling the Pearson product-moment correlation coefficient
  (`r`), the second element is used for rescaling `Alpha` and the third
  element is used for re-scaling `Beta`

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- method:

  character, indicating the formula used to compute the variability
  ratio in the Kling-Gupta efficiency. Valid values are:

  -) 2012: the variability is defined as ‘Gamma’, the ratio of the
  coefficient of variation of `sim` values to the coefficient of
  variation of `obs`. See Pizarro and Jorquera (2024) and Kling et al.
  (2012).

  -) 2009: the variability is defined as ‘Alpha’, the ratio of the
  standard deviation of `sim` values to the standard deviation of `obs`.
  This is the default option. See Gupta et al. (2009).

  -) 2021: the bias is defined as ‘Beta’, the ratio of `mean(sim)` minus
  `mean(obs)` to the standard deviation of `obs`. The variability is
  defined as ‘Alpha’, the ratio of the standard deviation of `sim`
  values to the standard deviation of `obs`. See Tang et al. (2021).

- out.type:

  character, indicating the whether the output of the function has to
  include each one of the three terms used in the computation of the
  Kling-Gupta efficiency or not. Valid values are:

  -) single: the output is a numeric with the Kling-Gupta efficiency
  only.

  -) full: the output is a list of two elements: the first one with the
  Kling-Gupta efficiency, and the second is a numeric with 3 elements:
  the Pearson product-moment correlation coefficient (‘r’), the ratio
  between the mean of the simulated values to the mean of observations
  (‘Beta’), and the variability measure (‘Gamma’ or ‘Alpha’, depending
  on the value of `method`).

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the Kling-Gupta
  efficiency.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `fun`.

  It is was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any nummeric value.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the the mean observed values, instead of
  the one hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs`, before applying
  `fun`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs`, before applying
  `fun`.

- epsilon.value:

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values, instead of the one
  hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

## Details

Traditional objective functions, such as Nash-Sutcliffe Efficiency (NSE)
and Kling-Gupta Efficiency (KGE), often make assumptions about data
distribution and are sensitive to outliers. The Kling-Gupta Efficiency
with knowable-moments (KGEkm) goodness-of-fit measure was developed by
Pizarro and Jorquera (2024) to provide a reliable estimation and
effective description of high-order statistics from typical hydrological
samples and, therefore, reducing uncertainty in their estimation and
computation of the KGE.

In the \\KGE\_{km}\\, the dispersion is quantified using knowable
moments (computed over ordered values of the samples in ascending order)
instead of the standard deviation, while retaining the decomposition
into correlation, variability, and bias components.

The general formulation of Kling–Gupta Efficiency with knowable moments
(\\KGE\_{km}\\) is:

\$\$ KGE\_{km} = 1 - \sqrt{ \left\[ s_1 (r - 1) \right\]^2 + \left\[ s_2
(vr - 1) \right\]^2 + \left\[ s_3 (br - 1) \right\]^2 } \$\$

where \\r\\ is the Pearson product–moment correlation coefficient
between simulated (\\Q^{sim}\_t\\) and observed (\\Q^{obs}\_t\\) values,
\\vr\\ is the variability ratio, \\br\\ is the bias ratio, and \\s =
(s_1, s_2, s_3)\\ is a vector of non-negative scaling factors that
control the relative importance of each component.

Dispersion is computed from the second knowable moment. For a sample
\\x_1, x_2, \ldots, x_n\\, the second knowable moment is defined as:

\$\$ K_2 = \frac{1}{n(n-1)} \sum\_{i=1}^{n} 2 (i-1) x\_{(i)} \$\$

where \\x\_{(i)}\\ denotes the **ordered values of the sample in
ascending order**. The corresponding dispersion measure is:

\$\$ \sigma\_{km} = \sqrt{ 2 K_2 } \$\$

The variability ratio depends on the selected `method`:

- For `method = "2009"`, variability is defined as the ratio of
  knowable-moment dispersions:

  \$\$ vr = \alpha = \frac{\sigma^{sim}\_{km}}{\sigma^{obs}\_{km}} \$\$

- For `method = "2012"`, variability is defined as the ratio of
  coefficients of variation:

  \$\$ vr = \gamma = \frac{ \sigma^{sim}\_{km} / \mu^{sim} }{
  \sigma^{obs}\_{km} / \mu^{obs} } \$\$

- For `method = "2021"`, variability is defined as in the 2009
  formulation:

  \$\$ vr = \alpha = \frac{\sigma^{sim}\_{km}}{\sigma^{obs}\_{km}} \$\$

The bias component also depends on the selected `method`:

- For `method = "2009"` and `method = "2012"`:

  \$\$ br = \beta = \frac{\mu^{sim}}{\mu^{obs}} \$\$

- For `method = "2021"`:

  \$\$ br = \beta\_{2021} = \frac{ \mu^{sim} - \mu^{obs} }{
  \sigma^{obs}\_{km} } \$\$

In the same line that the traditional Kling-Gupta efficiency, the
(\\KGE\_{km}\\) ranges from -Inf to 1. Essentially, the closer to 1, the
more similar `sim` and `obs` are.

As with other KGE-type metrics, the statistic integrates information
about correlation, variability, and bias into a single performance
measure while allowing explicit control over the relative contribution
of each component through the scaling factors \\s\\.

## Value

If `out.type=single`: numeric with the Kling-Gupta efficiency between
`sim` and `obs`. If `sim` and `obs` are matrices, the output value is a
vector, with the Kling-Gupta efficiency between each column of `sim` and
`obs`

If `out.type=full`: a list of two elements:

- KGEkm.value:

  numeric with the Kling-Gupta efficiency. If `sim` and `obs` are
  matrices, the output value is a vector, with the Kling-Gupta
  efficiency between each column of `sim` and `obs`

- KGEkm.elements:

  numeric with 3 elements: the Pearson product-moment correlation
  coefficient (‘r’), the ratio between the mean of the simulated values
  to the mean of observations (‘Beta’), and the variability measure
  (‘Gamma’ or ‘Alpha’, depending on the value of `method`). If `sim` and
  `obs` are matrices, the output value is a matrix, with the previous
  three elements computed for each column of `sim` and `obs`  

## References

Pizarro, A.; Jorquera, J. (2024). Advancing objective functions in
hydrological modelling: Integrating knowable moments for improved
simulation accuracy. Journal of Hydrology, 634, 131071.
doi:10.1016/j.jhydrol.2024.131071.

Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper
Danube basin under an ensemble of climate change scenarios. Journal of
Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.

Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009).
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling. Journal of hydrology,
377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.

Tang, G.; Clark, M. P.; Papalexiou, S. M. (2021). SC-earth: a
station-based serially complete earth dataset from 1950 to 2019. Journal
of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.

Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using
log-transformed flows within the KGEkm criterion.
doi:10.5194/hess-22-4583-2018.

Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or
not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores.
Hydrology and Earth System Sciences, 23(10), 4323-4331.
doi:10.5194/hess-23-4323-2019.

Cinkus, G., Mazzilli, N., Jourde, H., Wunsch, A., Liesch, T., Ravbar,
N., Chen, Z., and Goldscheider, N. (2023). When best is the enemy of
good - critical evaluation of performance criteria in hydrological
models. Hydrology and Earth System Sciences 27, 2397-2411,
doi:10.5194/hess-27-2397-2023

## Author

Mauricio Zambrano-Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md),
[`KGElf`](https://hzambran.github.io/hydroGOF/reference/KGElf.md),
[`sKGE`](https://hzambran.github.io/hydroGOF/reference/sKGE.md),
[`KGEnp`](https://hzambran.github.io/hydroGOF/reference/KGEnp.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
KGEkm(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
KGEkm(sim, obs)
#> [1] 0.793454

##################
# Example2: Looking at the difference between 'method=2009' and 'method=2012'

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGEkm 2012 (method="2012" is the default option for KGEkm)
KGEkm(sim=sim, obs=obs, method="2012", out.type="full")
#> $KGEkm.value
#> [1] -0.04201077
#> 
#> $KGEkm.elements
#>         r      Beta     Gamma 
#> 1.0000000 2.0000000 0.7071068 
#> 

# KGEkm 2009
KGEkm(sim=sim, obs=obs, method="2009", out.type="full")
#> $KGEkm.value
#> [1] -0.0823922
#> 
#> $KGEkm.elements
#>        r     Beta    Alpha 
#> 1.000000 2.000000 1.414214 
#> 


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
#> [1] 0.578109

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012")
#> [1] -0.4352039

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)
#> [1] -0.6329255

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021")
#> [1] 0.1010568

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)
#> [1] 0.004794342

# Weighted Nash-Sutcliffe efficiency (Hundecha and Bardossy, 2004)
wNSE(sim=sim, obs=obs)
#> [1] 0.281951

# wsNSE (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs)
#> [1] -0.2213234

# APFB (Mizukami et al., 2019):
APFB(sim=sim, obs=obs)
#> [1] 0.2918397


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
#> [1] 0.8774177

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012")
#> [1] -0.1042023

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs)
#> [1] -0.09772871

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021")
#> [1] 0.912638

# Traditional Nash-Sutcliffe eficiency (Nash and Sutcliffe, 1970)
NSE(sim=sim, obs=obs)
#> [1] 0.9844205

# Weighted seasonal Nash-Sutcliffe efficiency (Zambrano-Bigiarini and Bellin, 2012):
wsNSE(sim=sim, obs=obs, lambda=0.05, j=1/2)
#> [1] 0.6815896

# Refined Index of Agreement (Willmott et al., 2012):
dr(sim=sim, obs=obs)
#> [1] 0.9394451

# Relative Index of Agreement (Krause et al., 2005):
rd(sim=sim, obs=obs)
#> [1] 0.9077374

# Modified Index of Agreement (Krause et al., 2005):
md(sim=sim, obs=obs)
#> [1] 0.9355419

# KGElf (Garcia et al., 2017):
KGElf(sim=sim, obs=obs)
#> [1] 0.00988583


##################
# Example 5: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

KGEkm(sim=sim, obs=obs, fun=log)
#> [1] 0.8341867

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
KGEkm(sim=lsim, obs=lobs)
#> [1] 0.8341867

##################
# Example 6: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.8408504

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)
#> [1] 0.8408504

##################
# Example 7: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.8346255

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)
#> [1] 0.8346255

##################
# Example 8: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
KGEkm(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.8469799

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEkm(sim=lsim, obs=lobs)
#> [1] 0.8469799

##################
# Example 9: KGEkm for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

KGEkm(sim=sim, obs=obs, fun=fun1)
#> [1] 0.8960538

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
KGEkm(sim=sim1, obs=obs1)
#> [1] 0.8960538
```
