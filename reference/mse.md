# Mean Squared Error

Mean squared error between `sim` and `obs`, in the squared units of
`sim` and `obs`, with treatment of missing values.

## Usage

``` r
mse(sim, obs, ...)

# Default S3 method
mse(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'data.frame'
mse(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'matrix'
mse(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'zoo'
mse(sim, obs, na.rm=TRUE, fun=NULL, ..., 
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

\$\$ mse = \frac{1}{N} \sum\_{i=1}^N { \left( S_i - O_i \right)^2 } \$\$

## Value

Mean squared error between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the mean squared error between each column of `sim` and `obs`.

## References

Yapo P.O.; Gupta H.V.; Sorooshian S. (1996). Automatic calibration of
conceptual rainfall-runoff models: sensitivity to calibration data.
Journal of Hydrology. v181 i1-4. 23-48. doi:10.1016/0022-1694(95)02918-4

Gupta, H.V.; Kling, H. (2011). On typical range, sensitivity, and
normalization of Mean Squared Error and Nash-Sutcliffe Efficiency type
metrics. Water Resources Research, 47(10). doi:10.1029/2011WR010962.

Willmott, C.J.; Matsuura, K.; Robeson, S.M. (2009). Ambiguities inherent
in sums-of-squares-based error statistics, Atmospheric Environment, 43,
749-752, doi:10.1016/j.atmosenv.2008.10.005.

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
[`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.md),
[`ubRMSE`](https://hzambran.github.io/hydroGOF/reference/ubRMSE.md),
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
mse(sim, obs)
#> [1] 0

obs <- 1:10
sim <- 2:11
mse(sim, obs)
#> [1] 1

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'mse' for the "best" (unattainable) case
mse(sim=sim, obs=obs)
#> [1] 0

##################
# Example 3: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


mse(sim=sim, obs=obs)
#> [1] 50.57333

##################
# Example 4: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

mse(sim=sim, obs=obs, fun=log)
#> [1] 0.4836431

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
mse(sim=lsim, obs=lobs)
#> [1] 0.4836431

##################
# Example 5: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

mse(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.4600443

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
mse(sim=lsim, obs=lobs)
#> [1] 0.4600443

##################
# Example 6: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
mse(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.4820793

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
mse(sim=lsim, obs=lobs)
#> [1] 0.4820793

##################
# Example 7: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
mse(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.4386083

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
mse(sim=lsim, obs=lobs)
#> [1] 0.4386083

##################
# Example 8: mse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

mse(sim=sim, obs=obs, fun=fun1)
#> [1] 0.9226073

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
mse(sim=sim1, obs=obs1)
#> [1] 0.9226073
```
