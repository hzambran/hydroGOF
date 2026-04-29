# Non-parametric version of the Kling-Gupta Efficiency

Non-parametric Kling-Gupta efficiency between `sim` and `obs`, with
treatment of missing values.  

This goodness-of-fit measure was developed by Pool et al. (2018), as a
non-parametric alternative to the original Kling-Gupta efficiency (KGE)
proposed by Gupta et al. (2009). See Details.

## Usage

``` r
KGEnp(sim, obs, ...)

# Default S3 method
KGEnp(sim, obs, na.rm=TRUE, out.type=c("single", "full"), fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

# S3 method for class 'data.frame'
KGEnp(sim, obs, na.rm=TRUE, out.type=c("single", "full"), fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)

# S3 method for class 'matrix'
KGEnp(sim, obs, na.rm=TRUE, out.type=c("single", "full"), fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)
             
# S3 method for class 'zoo'
KGEnp(sim, obs, na.rm=TRUE, out.type=c("single", "full"), fun=NULL, ..., 
               epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
               epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  When an 'NA' value is found at the i-th position in `obs` **OR**
  `sim`, the i-th value of `obs` **AND** `sim` are removed before the
  computation.

- out.type:

  character, indicating the whether the output of the function has to
  include each one of the three terms used in the computation of the
  Kling-Gupta efficiency or not. Valid values are:

  -) single: the output is a numeric with the Kling-Gupta efficiency
  only.

  -) full: the output is a list of two elements: the first one with the
  Kling-Gupta efficiency, and the second is a numeric with 3 elements:
  the Spearman rank correlation coefficient (‘rSpearman’), the ratio
  between the mean of the simulated values to the mean of observations
  (‘Beta’), and the variability measure (‘Alpha’).

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing this goodness-of-fit
  index.

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
  any numeric value. This is the default option.

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

This non-paramettric verison of the Kling-Gupta efficiency keeps the
bias term Alpha (mean(sim) / mean(obs)), but for correlation uses the
Spearman rank coefficient instead of the Pearson product-moment
coefficient; and for variability it uses the normalized flow-duration
curve instead of the standard deviation (or coefficient of variation).

The proposed non-parametric based multi-objective function can be seen
as a useful alternative to existing performance measures when aiming at
acceptable simulations of multiple hydrograph aspects (Pool et al.,
2018).

\$\$KGE\_{np} = 1 - ED\$\$ \$\$ ED = \sqrt{ ((\rho-1)^2 + (\alpha-1)^2 +
(\beta-1)^2 } \$\$ \$\$\rho = \textrm{Spearman rank correlation
coefficient}\$\$ \$\$\alpha = 1 - 0.5\*sum( sim(I(k)) / (n\*\mu_s) -
obs(J(k)) / (n\*\mu_o) )\$\$ \$\$\beta = \mu_s/\mu_o\$\$

Traditional Kling-Gupta efficiencies (Gupta et al., 2009; Kling et al.,
2012) range from -Inf to 1, and therefore KGEnp should do so.
Essentially, the closer to 1, the more similar `sim` and `obs` are.  

Knoben et al. (2019) showed that traditional Kling-Gupta (Gupta et al.,
2009; Kling et al., 2012) values greater than -0.41 indicate that a
model improves upon the mean flow benchmark, even if the model's KGE
value is negative.

## Value

numeric with the non-parametric Kling-Gupta efficiency between `sim` and
`obs`.  
If `sim` and `obs` are matrices, the output value is a vector, with the
non-parametric Kling-Gupta efficiency between each column of `sim` and
`obs`

## References

Pool, S.; Vis, M.; Seibert, J. (2018). Evaluating model performance:
towards a non-parametric variant of the Kling-Gupta efficiency.
Hydrological Sciences Journal, 63(13-14), pp.1941-1953.
doi:/10.1080/02626667.2018.1552002.

Garcia, F.; Folton, N.; Oudin, L. (2017). Which objective function to
calibrate rainfall-runoff models for low-flow index simulations?.
Hydrological sciences journal, 62(7), 1149-1166.
doi:10.1080/02626667.2017.1308511.

Gupta, H. V.; Kling, H.; Yilmaz, K. K.; Martinez, G. F. (2009).
Decomposition of the mean squared error and NSE performance criteria:
Implications for improving hydrological modelling. Journal of hydrology,
377(1-2), 80-91. doi:10.1016/j.jhydrol.2009.08.003. ISSN 0022-1694.

Kling, H.; Fuchs, M.; Paulin, M. (2012). Runoff conditions in the upper
Danube basin under an ensemble of climate change scenarios. Journal of
Hydrology, 424, 264-277, doi:10.1016/j.jhydrol.2012.01.011.

Santos, L.; Thirel, G.; Perrin, C. (2018). Pitfalls in using
log-transformed flows within the KGE criterion.
doi:10.5194/hess-22-4583-2018.

Knoben, W.J.; Freer, J.E.; Woods, R.A. (2019). Inherent benchmark or
not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores.
Hydrology and Earth System Sciences, 23(10), 4323-4331.
doi:10.5194/hess-23-4323-2019.

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
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
# Example1: basic ideal case
obs <- 1:10
sim <- 1:10
KGEnp(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
KGEnp(sim, obs)
#> [1] 0.8148503

##################
# Example2: Looking at the difference between 'method=2009' and 'method=2012'
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")
#> $KGE.value
#> [1] -1.236068
#> 
#> $KGE.elements
#>     r  Beta Alpha 
#>     1     2     2 
#> 

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")
#> $KGE.value
#> [1] -1
#> 
#> $KGE.elements
#>     r  Beta Gamma 
#>     1     2     1 
#> 

# KGEnp (Pool et al., 2018):
KGEnp(sim=sim, obs=obs)
#> [1] 0

##################
# Example3: KGEnp for simulated values equal to observations plus random noise 
#           on the first half of the observed values
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim <- obs 
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)

# Computing the new 'KGEnp'
KGEnp(sim=sim, obs=obs)
#> [1] 0.6344234

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'KGEnp'
KGEnp(sim=sim, obs=obs)
#> [1] 0.6129172

##################
# Example 4: KGEnp for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

KGEnp(sim=sim, obs=obs, fun=log)
#> [1] 0.7452

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
KGEnp(sim=lsim, obs=lobs)
#> [1] 0.7452

##################
# Example 5: KGEnp for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

KGEnp(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.7452

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEnp(sim=lsim, obs=lobs)
#> [1] 0.7503355

##################
# Example 6: KGEnp for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
KGEnp(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.7452

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEnp(sim=lsim, obs=lobs)
#> [1] 0.7455385

##################
# Example 7: KGEnp for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
KGEnp(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.7452

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
KGEnp(sim=lsim, obs=lobs)
#> [1] 0.7550453

##################
# Example 8: KGEnp for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

KGEnp(sim=sim, obs=obs, fun=fun1)
#> [1] 0.7533573

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
KGEnp(sim=sim1, obs=obs1)
#> [1] 0.7533573
```
