# Weighted Nash-Sutcliffe efficiency

Weighted Nash-Sutcliffe efficiency between `sim` and `obs`, with
treatment of missing values.  

This goodness-of-fit measure was proposed by Hundecha and Bardossy
(2004) to put special focus on high values.

## Usage

``` r
wNSE(sim, obs, ...)

# Default S3 method
wNSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'data.frame'
wNSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'matrix'
wNSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'zoo'
wNSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
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
  transformed values thereof before computing the weighted
  Nash-Sutcliffe efficiency.

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
  argument is used to multiply the mean of the observed values, instead
  of the one hundredth (1/100) described in Pushpalatha et al. (2012).
  The resulting value is then added to both `sim` and `obs`, before
  applying `fun`.

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

\$\$ wNSE = 1 -\frac { \sum\_{i=1}^N O_i \* ( S_i - O_i )^2 } {
\sum\_{i=1}^N O_i \* ( O_i - \bar{O} )^2 } \$\$

## Value

Weighted Nash-Sutcliffe efficiency between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the relative Nash-Sutcliffe efficiency between each column of `sim` and
`obs`.

## References

Nash, J.E. and J.V. Sutcliffe, River flow forecasting through conceptual
models. Part 1: A discussion of principles, J. Hydrol. 10 (1970), pp.
282-290. doi:10.1016/0022-1694(70)90255-6.  

Hundecha, Y., Bardossy, A. (2004). Modeling of the effect of land use
changes on the runoff generation of a river basin through parameter
regionalization of a watershed model. Journal of hydrology, 292(1-4),
281-295. doi:10.1016/j.jhydrol.2004.01.002.

Hundecha, Y., Ouarda, T. B., Bardossy, A. (2008). Regional estimation of
parameters of a rainfall-runoff model at ungauged watersheds using the
'spatial' structures of the parameters within a canonical
physiographic-climatic space. Water Resources Research, 44(1).
doi:10.1029/2006WR005439.

Hundecha, Y. and Merz, B. (2012), Exploring the Relationship between
Changes in Climate and Floods Using a Model-Based Analysis, Water
Resour. Res., 48(4), 1-21, doi:10.1029/2011WR010527..

## Author

sluedtke (github user)

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation  

If some of the observed values are equal to zero (at least one of them),
this index can not be computed.

## See also

[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`rNSE`](https://hzambran.github.io/hydroGOF/reference/rNSE.md),
[`mNSE`](https://hzambran.github.io/hydroGOF/reference/mNSE.md),
[`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
wNSE(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
wNSE(sim, obs)
#> [1] 0.8787879

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'wNSE' for the "best" (unattainable) case
wNSE(sim=sim, obs=obs)
#> [1] 1

##################
# Example 3: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


wNSE(sim=sim, obs=obs)
#> [1] 0.9766039

##################
# Example 4: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

wNSE(sim=sim, obs=obs, fun=log)
#> [1] 0.7428831

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
wNSE(sim=lsim, obs=lobs)
#> [1] 0.7428831

##################
# Example 5: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

wNSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.7390918

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)
#> [1] 0.7390918

##################
# Example 6: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
wNSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.7426189

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)
#> [1] 0.7426189

##################
# Example 7: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
wNSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.7360056

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
wNSE(sim=lsim, obs=lobs)
#> [1] 0.7360056

##################
# Example 8: wNSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

wNSE(sim=sim, obs=obs, fun=fun1)
#> [1] 0.8875357

# Verifying the previous value
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
wNSE(sim=sim1, obs=obs1)
#> [1] 0.8875357
```
