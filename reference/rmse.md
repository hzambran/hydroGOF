# Root Mean Square Error

Root Mean Square Error (RMSE) between `sim` and `obs`, in the same units
of `sim` and `obs`, with treatment of missing values.  
RMSE gives the standard deviation of the model prediction error. A
smaller value indicates better model performance.

## Usage

``` r
rmse(sim, obs, ...)

# Default S3 method
rmse(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'data.frame'
rmse(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'matrix'
rmse(sim, obs, na.rm=TRUE, fun=NULL, ...,
              epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
              epsilon.value=NA)

# S3 method for class 'zoo'
rmse(sim, obs, na.rm=TRUE, fun=NULL, ...,
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

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values, instead of the one
  hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

## Details

\$\$ rmse = \sqrt{ \frac{1}{N} \sum\_{i=1}^N { \left( S_i - O_i
\right)^2 } } \$\$

## Value

Root mean square error (rmse) between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the RMSE between each column of `sim` and `obs`.

## References

<https://en.wikipedia.org/wiki/Root_mean_square_deviation>

Willmott, C.J.; Matsuura, K. (2005). Advantages of the mean absolute
error (MAE) over the root mean square error (RMSE) in assessing average
model performance, Climate Research, 30, 79-82, doi:10.3354/cr030079.

Chai, T.; Draxler, R.R. (2014). Root mean square error (RMSE) or mean
absolute error (MAE)? - Arguments against avoiding RMSE in the
literature, Geoscientific Model Development, 7, 1247-1250.
doi:10.5194/gmd-7-1247-2014.

Hodson, T.O. (2022). Root-mean-square error (RMSE) or mean absolute
error (MAE): when to use them or not, Geoscientific Model Development,
15, 5481-5487, doi:10.5194/gmd-15-5481-2022.

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
rmse(sim, obs)
#> [1] 0

obs <- 1:10
sim <- 2:11
rmse(sim, obs)
#> [1] 1

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'rmse' for the "best" (unattainable) case
rmse(sim=sim, obs=obs)
#> [1] 0

##################
# Example 3: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


rmse(sim=sim, obs=obs)
#> [1] 7.116846

##################
# Example 4: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

rmse(sim=sim, obs=obs, fun=log)
#> [1] 0.6960498

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
rmse(sim=lsim, obs=lobs)
#> [1] 0.6960498

##################
# Example 5: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

rmse(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.6788822

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
rmse(sim=lsim, obs=lobs)
#> [1] 0.6788822

##################
# Example 6: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
rmse(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.6949254

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
rmse(sim=lsim, obs=lobs)
#> [1] 0.6949254

##################
# Example 7: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
rmse(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.6629011

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
rmse(sim=lsim, obs=lobs)
#> [1] 0.6629011

##################
# Example 8: rmse for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

rmse(sim=sim, obs=obs, fun=fun1)
#> [1] 0.9622686

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
rmse(sim=sim1, obs=obs1)
#> [1] 0.9622686
```
