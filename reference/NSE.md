# Nash-Sutcliffe Efficiency

Nash-Sutcliffe efficiency between `sim` and `obs`, with treatment of
missing values.

## Usage

``` r
NSE(sim, obs, ...)

# Default S3 method
NSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'data.frame'
NSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'matrix'
NSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
             epsilon.value=NA)

# S3 method for class 'zoo'
NSE(sim, obs, na.rm=TRUE, fun=NULL, ..., 
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
  transformed values thereof before computing the Nash-Sutcliffe
  efficiency.

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

\$\$ NSE = 1 -\frac { \sum\_{i=1}^N { \left( S_i - O_i \right)^2 } } {
\sum\_{i=1}^N { \left( O_i - \bar{O} \right)^2 } } \$\$

The Nash-Sutcliffe efficiency (NSE) is a normalized statistic that
determines the relative magnitude of the residual variance ("noise")
compared to the measured data variance ("information") (Nash and
Sutcliffe, 1970).  

NSE indicates how well the plot of observed versus simulated data fits
the 1:1 line.  

Nash-Sutcliffe efficiencies range from -Inf to 1. Essentially, the
closer to 1, the more accurate the model is.  
-) NSE = 1, corresponds to a perfect match of modelled to the observed
data.  
-) NSE = 0, indicates that the model predictions are as accurate as the
mean of the observed data,  
-) -Inf \< NSE \< 0, indicates that the observed mean is better
predictor than the model.

## Value

Nash-Sutcliffe efficiency between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the Nash-Sutcliffe efficiency between each column of `sim` and `obs`.

## References

<https://en.wikipedia.org/wiki/Nash%E2%80%93Sutcliffe_model_efficiency_coefficient>

Nash, J.E. and Sutcliffe, J.V. (1970). River flow forecasting through
conceptual models. Part 1: a discussion of principles, Journal of
Hydrology 10, pp. 282-290. doi:10.1016/0022-1694(70)90255-6.

Garrick, M.; Cunnane, C.; Nash, J.E. (1978). A criterion of efficiency
for rainfall-runoff models. Journal of Hydrology 36, 375-381.
doi:10.1016/0022-1694(78)90155-5.

Schaefli, B., Gupta, H. (2007). Do Nash values have value?. Hydrological
Processes 21, 2075-2080. doi:10.1002/hyp.6825.

Criss, R. E.; Winston, W. E. (2008), Do Nash values have value?
Discussion and alternate proposals. Hydrological Processes, 22:
2723-2725. doi:10.1002/hyp.7072.

Gupta, H.V.; Kling, H. (2011). On typical range, sensitivity, and
normalization of Mean Squared Error and Nash-Sutcliffe Efficiency type
metrics. Water Resources Research, 47(10). doi:10.1029/2011WR010962.

Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A
review of efficiency criteria suitable for evaluating low-flow
simulations. Journal of Hydrology, 420, 171-182.
doi:10.1016/j.jhydrol.2011.11.055.

Knoben, W. J.; Freer, J. E.; Woods, R. A. (2019). Inherent benchmark or
not? Comparing Nash-Sutcliffe and Kling-Gupta efficiency scores.
Hydrology and Earth System Sciences, 23(10), 4323-4331.
doi:10.5194/hess-23-4323-2019.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`mNSE`](https://hzambran.github.io/hydroGOF/reference/mNSE.md),
[`rNSE`](https://hzambran.github.io/hydroGOF/reference/rNSE.md),
[`wNSE`](https://hzambran.github.io/hydroGOF/reference/wNSE.md),
[`KGE`](https://hzambran.github.io/hydroGOF/reference/KGE.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
NSE(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
NSE(sim, obs)
#> [1] 0.8787879

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
NSE(sim=sim, obs=obs)
#> [1] 1

##################
# Example 3: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


NSE(sim=sim, obs=obs)
#> [1] 0.8737763

##################
# Example 4: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

NSE(sim=sim, obs=obs, fun=log)
#> [1] 0.478635

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
NSE(sim=lsim, obs=lobs)
#> [1] 0.478635

##################
# Example 5: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

NSE(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.4857823

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)
#> [1] 0.4857823

##################
# Example 6: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
NSE(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.4791024

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)
#> [1] 0.4791024

##################
# Example 7: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
NSE(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.4924612

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
NSE(sim=lsim, obs=lobs)
#> [1] 0.4924612

##################
# Example 8: NSE for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

NSE(sim=sim, obs=obs, fun=fun1)
#> [1] 0.7256845

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
NSE(sim=sim1, obs=obs1)
#> [1] 0.7256845
```
