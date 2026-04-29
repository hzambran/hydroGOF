# Index of Agreement

Index of Agreement between `sim` and `obs`, with treatment of missing
values.

## Usage

``` r
d(sim, obs, ...)

# Default S3 method
d(sim, obs, na.rm=TRUE, fun=NULL, ...,
           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
           epsilon.value=NA)

# S3 method for class 'data.frame'
d(sim, obs, na.rm=TRUE, fun=NULL, ...,
           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
           epsilon.value=NA)

# S3 method for class 'matrix'
d(sim, obs, na.rm=TRUE, fun=NULL, ...,
           epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"), 
           epsilon.value=NA)

# S3 method for class 'zoo'
d(sim, obs, na.rm=TRUE, fun=NULL, ...,
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

  1\) "none": `sim` and `obs` are used by `FUN` without the addition of
  any nummeric value.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `FUN`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the the mean observed values, instead of
  the one hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs`, before applying
  `FUN`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs`, before applying
  `FUN`.

- epsilon.value:

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values, instead of the one
  hundredth (1/100) described in Pushpalatha et al. (2012). The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

## Details

\$\$d = 1 - \frac{\sum\_{i=1}^N {(O_i - S_i)^2} } { \sum\_{i=1}^N { (
\left\| S_i - \bar{O} \right\| + \left\| O_i - \bar{O} \right\| } )^2 }
\$\$

The Index of Agreement (d) developed by Willmott (1981) as a
standardized measure of the degree of model prediction error.

It is is dimensionless and varies between 0 and 1. A value of 1
indicates a perfect match, and 0 indicates no agreement at all
(Willmott, 1981).

The index of agreement can detect additive and proportional differences
in the observed and simulated means and variances; however, it is overly
sensitive to extreme values due to the squared differences (Legates and
McCabe, 1999).

## Value

Index of agreement between `sim` and `obs`.  

If `sim` and `obs` are matrixes or data.frames, the returned value is a
vector, with the index of agreement between each column of `sim` and
`obs`.

## References

Willmott, C.J. (1981). On the validation of models. Physical Geography,
2, 184–194. doi:10.1080/02723646.1981.10642213.

Willmott, C.J. (1984). On the evaluation of model performance in
physical geography. Spatial Statistics and Models, G. L. Gaile and C. J.
Willmott, eds., 443-460. doi:10.1007/978-94-017-3048-8_23.

Willmott, C.J.; Ackleson, S.G. Davis, R.E.; Feddema, J.J.; Klink, K.M.;
Legates, D.R.; O'Donnell, J.; Rowe, C.M. (1985), Statistics for the
Evaluation and Comparison of Models, J. Geophys. Res., 90(C5),
8995-9005. doi:10.1029/JC090iC05p08995.

Legates, D.R.; McCabe, G. J. Jr. (1999), Evaluating the Use of
"Goodness-of-Fit" Measures in Hydrologic and Hydroclimatic Model
Validation, Water Resour. Res., 35(1), 233-241.
doi:10.1029/1998WR900018.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` has to have the same length/dimension  

The missing values in `obs` and `sim` are removed before the computation
proceeds, and only those positions with non-missing values in `obs` and
`sim` are considered in the computation

## See also

[`md`](https://hzambran.github.io/hydroGOF/reference/md.md),
[`rd`](https://hzambran.github.io/hydroGOF/reference/rd.md),
[`dr`](https://hzambran.github.io/hydroGOF/reference/dr.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r
##################
# Example 1: basic ideal case
obs <- 1:10
sim <- 1:10
d(sim, obs)
#> [1] 1

obs <- 1:10
sim <- 2:11
d(sim, obs)
#> [1] 0.9706745

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'd' for the "best" (unattainable) case
d(sim=sim, obs=obs)
#> [1] 1

##################
# Example 3: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for ow flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


d(sim=sim, obs=obs)
#> [1] 0.9696183

##################
# Example 4: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

d(sim=sim, obs=obs, fun=log)
#> [1] 0.8605263

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
d(sim=lsim, obs=lobs)
#> [1] 0.8605263

##################
# Example 5: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

d(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")
#> [1] 0.8628634

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
d(sim=lsim, obs=lobs)
#> [1] 0.8628634

##################
# Example 6: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
d(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)
#> [1] 0.8606803

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
d(sim=lsim, obs=lobs)
#> [1] 0.8606803

##################
# Example 7: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
d(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)
#> [1] 0.8650161

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
d(sim=lsim, obs=lobs)
#> [1] 0.8650161

##################
# Example 8: d for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

d(sim=sim, obs=obs, fun=fun1)
#> [1] 0.9308643

# Verifying the previous value, with the epsilon value following Pushpalatha2012
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
d(sim=sim1, obs=obs1)
#> [1] 0.9308643
```
