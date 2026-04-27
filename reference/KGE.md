# Kling-Gupta Efficiency

Kling-Gupta efficiency between `sim` and `obs`, with treatment of
missing values.

This goodness-of-fit measure was developed by Gupta et al. (2009) to
provide a diagnostically interesting decomposition of the Nash-Sutcliffe
efficiency (and hence MSE), which facilitates the analysis of the
relative importance of its different components (correlation, bias and
variability) in the context of hydrological modelling.

Kling et al. (2012) proposed a revised version of this index (KGE') to
ensure that the bias and variability ratios are not cross-correlated.

Tang et al. (2021) proposed a revised version of this index (KGE”) to
avoid the anomalously negative KGE' or KGE values when the mean value is
close to zero.

For a short description of its three components and the numeric range of
varios, pleae see Details.

## Usage

``` r
KGE(sim, obs, ...)

# Default S3 method
KGE(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'data.frame'
KGE(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'matrix'
KGE(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
             out.type=c("single", "full"), fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)
             
# S3 method for class 'zoo'
KGE(sim, obs, s=c(1,1,1), na.rm=TRUE, method=c("2009", "2012", "2021"), 
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

  -) 2009: the variability is defined as ‘Alpha’, the ratio of the
  standard deviation of `sim` values to the standard deviation of `obs`.
  This is the default option. See Gupta et al. (2009).

  -) 2012: the variability is defined as ‘Gamma’, the ratio of the
  coefficient of variation of `sim` values to the coefficient of
  variation of `obs`. See Kling et al. (2012).

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

In the computation of this index, there are three main components
involved:

1\) r : the Pearson product-moment correlation coefficient. Ideal value
is r=1.

2\) Beta : the ratio between the mean of the simulated values and the
mean of the observed ones. Ideal value is Beta=1.

3\) vr : variability ratio, which could be computed using the standard
deviation (Alpha) or the coefficient of variation (Gamma) of `sim` and
`obs`, depending on the value of `method`:

3.1) Alpha: the ratio between the standard deviation of the simulated
values and the standard deviation of the observed ones. Its ideal value
is Alpha=1.

3.2) Gamma: the ratio between the coefficient of variation (CV) of the
simulated values to the coefficient of variation of the observed ones.
Its ideal value is Gamma=1.

For a full discussion of the Kling-Gupta index, and its advantages over
the Nash-Sutcliffe efficiency
([`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md)) see
Gupta et al. (2009).

Kling-Gupta efficiencies range from -Inf to 1. Essentially, the closer
to 1, the more similar `sim` and `obs` are.

Knoben et al. (2019) showed that KGE values greater than -0.41 indicate
that a model improves upon the mean flow benchmark, even if the model's
KGE value is negative.

\$\$KGE = 1 - ED\$\$ \$\$ ED = \sqrt{ (s\[1\]\*(r-1))^2
+(s\[2\]\*(vr-1))^2 + (s\[3\]\*(\beta-1))^2 } \$\$ \$\$r=Pearson
product-moment correlation coefficient\$\$ \$\$vr= \left\\
\begin{array}{cc} \alpha & , \\ method=2009 \\ \gamma & , \\ method=2012
\end{array} \right.\$\$ \$\$\beta=\mu_s/\mu_o\$\$
\$\$\alpha=\sigma_s/\sigma_o\$\$ \$\$\gamma=\frac{CV_s}{CV_o} =
\frac{\sigma_s/\mu_s}{\sigma_o/\mu_o}\$\$

## Value

If `out.type=single`: numeric with the Kling-Gupta efficiency between
`sim` and `obs`. If `sim` and `obs` are matrices, the output value is a
vector, with the Kling-Gupta efficiency between each column of `sim` and
`obs`

If `out.type=full`: a list of two elements:

- KGE.value:

  numeric with the Kling-Gupta efficiency. If `sim` and `obs` are
  matrices, the output value is a vector, with the Kling-Gupta
  efficiency between each column of `sim` and `obs`

- KGE.elements:

  numeric with 3 elements: the Pearson product-moment correlation
  coefficient (‘r’), the ratio between the mean of the simulated values
  to the mean of observations (‘Beta’), and the variability measure
  (‘Gamma’ or ‘Alpha’, depending on the value of `method`). If `sim` and
  `obs` are matrices, the output value is a matrix, with the previous
  three elements computed for each column of `sim` and `obs`  

## References

Gupta, H.V.; Kling, H.; Yilmaz, K.K.; Martinez, G.F. (2009).
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling. Journal of hydrology,
377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.

Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper
Danube basin under an ensemble of climate change scenarios. Journal of
Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.

Tang, G.; Clark, M.P.; Papalexiou, S.M. (2021). SC-earth: a
station-based serially complete earth dataset from 1950 to 2019. Journal
of Climate, 34(16), 6493-6511. doi:10.1175/JCLI-D-21-0067.1.

Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using
log-transformed flows within the KGE criterion.
doi:10.5194/hess-22-4583-2018.

Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or
not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores.
Hydrology and Earth System Sciences, 23(10), 4323-4331.
doi:10.5194/hess-23-4323-2019.

Mizukami, N.; Rakovec, O.; Newman, A.J.; Clark, M.P.; Wood, A.W.; Gupta,
H.V.; Kumar, R. (2019). On the choice of calibration metrics for
"high-flow" estimation using hydrologic models.
doi:10.5194/hess-23-2601-2019.

Cinkus, G.; Mazzilli, N.; Jourde, H.; Wunsch, A.; Liesch, T.; Ravbar,
N.; Chen, Z.; and Goldscheider, N. (2023). When best is the enemy of
good - critical evaluation of performance criteria in hydrological
models. Hydrology and Earth System Sciences 27, 2397-2411,
doi:10.5194/hess-27-2397-2023.

## Author

Mauricio Zambrano-Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

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
KGE(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
KGE(sim, obs)
#> [1] 0.8181818

##################
# Example2: Looking at the difference between 'method=2009' and 'method=2012'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs, method="2009", out.type="full")
#> $KGE.value
#> [1] -0.4142136
#> 
#> $KGE.elements
#>     r  Beta Alpha 
#>     1     2     2 
#> 

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012", out.type="full")
#> $KGE.value
#> [1] 0
#> 
#> $KGE.elements
#>     r  Beta Gamma 
#>     1     2     1 
#> 

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021", out.type="full")
#> $KGE.value
#> [1] -0.2744084
#> 
#> $KGE.elements
#>         r Beta.2021     Alpha 
#> 1.0000000 0.7900105 2.0000000 
#> 

##################
# Example3: KGE for simulated values equal to observations plus random noise 
#           on the first half of the observed values
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim <- obs 
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)

# Computing the new 'KGE'
KGE(sim=sim, obs=obs)
#> [1] 0.6799691

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Traditional Kling-Gupta eficiency (Gupta and Kling, 2009)
KGE(sim=sim, obs=obs, method="2009", out.type="full")
#> $KGE.value
#> [1] 0.6490295
#> 
#> $KGE.elements
#>         r      Beta     Alpha 
#> 0.9704045 1.3471574 1.0422635 
#> 

# KGE': Kling-Gupta eficiency 2012 (Kling et al.,2012) 
KGE(sim=sim, obs=obs, method="2012", out.type="full")
#> $KGE.value
#> [1] 0.5845285
#> 
#> $KGE.elements
#>         r      Beta     Gamma 
#> 0.9704045 1.3471574 0.7736762 
#> 

# KGE'': Kling-Gupta eficiency 2021 (Tang et al.,2021) 
KGE(sim=sim, obs=obs, method="2021", out.type="full")
#> $KGE.value
#> [1] 0.720931
#> 
#> $KGE.elements
#>         r Beta.2021     Alpha 
#> 0.9704045 0.2742580 1.0422635 
#> 

##################
# Example 4: KGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

KGE(sim=sim, obs=obs, fun=log)
#> [1] 0.7157116

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
KGE(sim=lsim, obs=lobs)
#> [1] 0.7157116

##################
# Example 5: KGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

KGE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.7233712

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGE(sim=lsim, obs=lobs)
#> [1] 0.7233712

##################
# Example 6: KGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
KGE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.7162154

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGE(sim=lsim, obs=lobs)
#> [1] 0.7162154

##################
# Example 7: KGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
KGE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.730441

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGE(sim=lsim, obs=lobs)
#> [1] 0.730441

##################
# Example 8: KGE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

KGE(sim=sim, obs=obs, fun=fun1)
#> [1] 0.7948099

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
KGE(sim=sim1, obs=obs1)
#> [1] 0.7948099
```
