# Percent Bias in the Slope of the Midsegment of the Flow Duration Curve

Percent Bias in the slope of the midsegment of the flow duration curve
(FDC) \[%\]. It is related to the vertical soil moisture
redistribution.  

## Usage

``` r
pbiasfdc(sim, obs, ...)

# Default S3 method
pbiasfdc(sim, obs, lQ.thr=0.6, hQ.thr=0.1, na.rm=TRUE, 
       plot=TRUE, verbose=FALSE, fun=NULL, ..., 
       epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
       epsilon.value=NA)

# S3 method for class 'data.frame'
pbiasfdc(sim, obs, lQ.thr=0.6, hQ.thr=0.1, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
        epsilon.value=NA)

# S3 method for class 'matrix'
pbiasfdc(sim, obs, lQ.thr=0.6, hQ.thr=0.1, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
        epsilon.value=NA)
       
# S3 method for class 'zoo'
pbiasfdc(sim, obs, lQ.thr=0.6, hQ.thr=0.1, na.rm=TRUE, 
        plot=TRUE, verbose=FALSE, fun=NULL, ..., 
        epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
        epsilon.value=NA)
```

## Arguments

- sim:

  numeric, zoo, matrix or data.frame with simulated values

- obs:

  numeric, zoo, matrix or data.frame with observed values

- lQ.thr:

  numeric, used to classify low flows. All the streamflows with a
  probability of exceedence larger or equal to `lQ.thr` are classified
  as low flows

- hQ.thr:

  numeric, used to classify high flows. All the streamflows with a
  probability of exceedence larger or equal to `hQ.thr` are classified
  as high flows

- na.rm:

  a logical value indicating whether 'NA' values should be stripped
  before the computation proceeds.

- plot:

  a logical value indicating if the flow duration curves corresponding
  to `obs` and `sim` have to be plotted or not.

- verbose:

  logical; if TRUE, progress messages are printed

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

## Value

Percent Bias in the slope of the midsegment of the flow duration curve,
between `sim` and `obs`.  

If `sim` and `obs` are matrixes, the returned value is a vector, with
the Percent Bias in the slope of the midsegment of the flow duration
curve, between each column of `sim` and `obs`.

## References

Yilmaz, K.K., Gupta, H.V. ; Wagener, T. (2008), A process-based
diagnostic approach to model evaluation: Application to the NWS
distributed hydrologic model, Water Resources Research, 44, W09417,
doi:10.1029/2007WR006716.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

The result is given in percentage (%).  

It requires the hydroTSM package.

## See also

[fdc](https://rdrr.io/pkg/hydroTSM/man/fdc.html),
[`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md),
[`mae`](https://hzambran.github.io/hydroGOF/reference/mae.md),
[`mse`](https://hzambran.github.io/hydroGOF/reference/mse.md),
[`rmse`](https://hzambran.github.io/hydroGOF/reference/rmse.md),
[`ubRMSE`](https://hzambran.github.io/hydroGOF/reference/ubRMSE.md),
[`nrmse`](https://hzambran.github.io/hydroGOF/reference/nrmse.md),
[`ssq`](https://hzambran.github.io/hydroGOF/reference/ssq.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
if (FALSE) { # \dontrun{
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
pbiasfdc(sim, obs)

obs <- 1:10
sim <- 2:11
pbiasfdc(sim, obs)

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'pbiasfdc' for the "best" (unattainable) case
pbiasfdc(sim=sim, obs=obs)

##################
# Example 3: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)

pbiasfdc(sim=sim, obs=obs)

##################
# Example 4: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

pbiasfdc(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 5: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 6: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 7: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
pbiasfdc(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
pbiasfdc(sim=lsim, obs=lobs)

##################
# Example 8: pbiasfdc for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

pbiasfdc(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
pbiasfdc(sim=sim1, obs=obs1)
} # }
```
