# Unbiased Root Mean Square Error

unbiased Root Mean Square Error (ubRMSE) between `sim` and `obs`, in the
same units of `sim` and `obs`, with treatment of missing values.  

ubRMSE was introduced by Entekhabi et al. (2010) to improve the
evaluation of the temporal dynamic of volumentric soil moisture, by
removing from the traditional RMSE the mean bias error caused by the
mistmatch between the spatial representativeness of in situ soil
moisture and the corresponding gridded values.  

A smaller value indicates better model performance.

## Usage

``` r
ubRMSE(sim, obs, ...)

# Default S3 method
ubRMSE(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'data.frame'
ubRMSE(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'matrix'
ubRMSE(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'zoo'
ubRMSE(sim, obs, na.rm=TRUE, fun=NULL, ...,
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

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the Root Mean Square
  Error.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `FUN`.

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

  numeric value to be added to both `sim` and `obs` when
  `epsilon.type="otherValue"`.

## Details

The traditional root mean square error (RMSE) is severely compromised if
there are biases in either the mean or the amplitude of fluctuations of
the simulated values. If it can be estimated reliably, the mean-bias
(BIAS) can easily be removed from RMSE, leading to the unbiased RMSE:

\$\$ ubRMSE = \sqrt{ RMSE^2 - BIAS^2 } \$\$

## Value

Unbiased Root mean square error (ubRMSE) between `sim` and `obs`.  

If `sim` and `obs` are matrixes or data.frames, the returned value is a
vector, with the ubRMSE between each column of `sim` and `obs`.

## References

Entekhabi, D.; Reichle, R.H.; Koster, R.D.; Crow, W.T. (2010).
Performance metrics for soil moisture retrievals and application
requirements. Journal of Hydrometeorology, 11(3), 832-840. doi:
10.1175/2010JHM1223.1.

Ling, X.; Huang, Y.; Guo, W.; Wang, Y.; Chen, C.; Qiu, B.; Ge, J.; Qin,
K.; Xue, Y.; Peng, J. (2021). Comprehensive evaluation of
satellite-based and reanalysis soil moisture products using in situ
observations over China. Hydrology and Earth System Sciences, 25(7),
4209-4229. doi:10.5194/hess-25-4209-2021.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md),
[`pbiasfdc`](https://hzambran.github.io/hydroGOF/reference/pbiasfdc.md),
[`mae`](https://hzambran.github.io/hydroGOF/reference/mae.md),
[`mse`](https://hzambran.github.io/hydroGOF/reference/mse.md),
[`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.md),
[`nrmse`](https://hzambran.github.io/hydroGOF/reference/nrmse.md),
[`ssq`](https://hzambran.github.io/hydroGOF/reference/ssq.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
ubRMSE(sim, obs)
#> [1] 0

obs <- 1:10
sim <- 2:11
ubRMSE(sim, obs)
#> [1] 0

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'ubRMSE' for the "best" (unattainable) case
ubRMSE(sim=sim, obs=obs)
#> [1] 0

##################
# Example 3: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


ubRMSE(sim=sim, obs=obs)
#> [1] 5.058106

##################
# Example 4: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

ubRMSE(sim=sim, obs=obs, fun=log)
#> [1] 0.5542572

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
ubRMSE(sim=lsim, obs=lobs)
#> [1] 0.5542572

##################
# Example 5: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.5392559

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)
#> [1] 0.5392559

##################
# Example 6: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.5532724

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)
#> [1] 0.5532724

##################
# Example 7: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
ubRMSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.5253581

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
ubRMSE(sim=lsim, obs=lobs)
#> [1] 0.5253581

##################
# Example 8: ubRMSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

ubRMSE(sim=sim, obs=obs, fun=fun1)
#> [1] 0.7105079

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
ubRMSE(sim=sim1, obs=obs1)
#> [1] 0.7105079
```
