# Proxy for Model Robustness

Proxy for model robustness (PMR) between `sim` and `obs`, with treatment
of missing values.

## Usage

``` r
PMR(sim, obs, ...)

# Default S3 method
PMR(sim, obs, na.rm=TRUE, k=NULL, min.years=5, 
             days.per.year=365, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'data.frame'
PMR(sim, obs, na.rm=TRUE, k=NULL, min.years=5, 
             days.per.year=365, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'matrix'
PMR(sim, obs, na.rm=TRUE, k=NULL, min.years=5, 
             days.per.year=365, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)

# S3 method for class 'zoo'
PMR(sim, obs, na.rm=TRUE, k=NULL, min.years=5, 
             days.per.year=365, fun=NULL, ...,
             epsilon.type=c("none", "Pushpalatha2012", "otherFactor", "otherValue"),
             epsilon.value=NA)
```

## Arguments

- sim:

  zoo object with simulated values. Multicolumn `zoo` objects are
  allowed.

- obs:

  zoo object with observed values. Multicolumn `zoo` objects are
  allowed.

- na.rm:

  a logical value indicating whether 'NA' should be stripped before the
  computation proceeds.  
  For the full-period bias, only positions with valid paired values in
  `obs` and `sim` are used. For the moving-window biases, each
  fixed-length window is selected first, and invalid pairs are then
  removed inside that window.

- k:

  integer value representing the length of the moving window (number of
  time steps) used to compute the bias over sub-periods.  

  By default `k=NULL`, which means that its value is automatically
  computed based on the minimum numbers of years defined by `min.years`.

  The k argument should reflect the temporal scale at which robustness
  is intended to be evaluated, and therefore depends primarily on the
  time resolution of the data. Royer-Gaspard et al. (2021) recommended
  to use multi-year windows, typically in the range of 3 to 5 years, to
  ensure that each sub-period captures meaningful hydroclimatic
  variability while still allowing enough windows for comparison.

- min.years:

  Numeric, only used when the user does not explicitly define the value
  of `k`, i.e., when `k=NULL`.  

  Minimum numbers of years used to ensure that each sub-period used int
  eh computation of PMR captures meaningful hydroclimatic variability
  while still allowing enough windows for comparison. By default,
  `min.years=5`.

- days.per.year:

  Numeric, only used when the user does not explicitly define the value
  of `k`, i.e., when `k=NULL`.  

  Number of days in a year. A value of Use 365.25 is recoomended instead
  of the default value of 365 when `sim` and `obs` are long
  climatological series.

- fun:

  function to be applied to `sim` and `obs` in order to obtain
  transformed values thereof before computing the proxy for model
  robustness.

  The first argument MUST BE a numeric vector with any name (e.g., `x`),
  and additional arguments are passed using `...`.

- ...:

  arguments passed to `fun`, in addition to the mandatory first numeric
  vector.

- epsilon.type:

  argument used to define a numeric value to be added to both `sim` and
  `obs` before applying `FUN`.

  It was designed to allow the use of logarithm and other similar
  functions that do not work with zero values.

  Valid values of `epsilon.type` are:

  1\) "none": `sim` and `obs` are used by `fun` without the addition of
  any numeric value. This is the default option.

  2\) "Pushpalatha2012": one hundredth (1/100) of the mean observed
  values is added to both `sim` and `obs` before applying `fun`, as
  described in Pushpalatha et al. (2012).

  3\) "otherFactor": the numeric value defined in the `epsilon.value`
  argument is used to multiply the mean of the observed values. The
  resulting value is then added to both `sim` and `obs` before applying
  `fun`.

  4\) "otherValue": the numeric value defined in the `epsilon.value`
  argument is directly added to both `sim` and `obs` before applying
  `fun`.

- epsilon.value:

  -) when `epsilon.type="otherValue"` it represents the numeric value to
  be added to both `sim` and `obs` before applying `fun`.  
  -) when `epsilon.type="otherFactor"` it represents the numeric factor
  used to multiply the mean of the observed values to obtain the
  constant to be added to both `sim` and `obs` before applying `fun`.

## Details

\$\$ PMR = 2 \times \frac{1}{N} \sum\_{i=1}^{N} \left\| (\bar{S}\_i -
\bar{O}\_i) - (\bar{S} - \bar{O}) \right\| \frac{1}{\bar{O}} \$\$

where:

- \\\bar{S}\_i\\ and \\\bar{O}\_i\\ are the mean simulated and observed
  values computed over the i-th moving window of length `k`,

- \\\bar{S}\\ and \\\bar{O}\\ are the overall mean simulated and
  observed values,

- \\N\\ is the number of moving windows.

The proxy for model robustness (PMR) is a dimensionless statistic that
quantifies the temporal stability of model bias by measuring the average
deviation of sub-period bias from the overall bias.

PMR indicates how consistent the model performance is across different
time periods or hydrological conditions.

The proxy for model robustness ranges from 0 to positive infinity.
Essentially, the closer to 0, the more temporally robust the model is.  

-) PMR = 0 corresponds to a perfectly robust model, in which the model
bias is identical across all sub-periods.  

-) 0 \< PMR \< 1 indicates relatively stable model performance with
moderate temporal variability in bias.  

-) PMR \> 1 indicates increasing variability in model bias across time
periods, suggesting reduced robustness of model performance.

## Value

Proxy for model robustness between `sim` and `obs`.  

If `sim` and `obs` are multicolumn `zoo` objects, the returned value is
a vector with the proxy for model robustness between each column of
`sim` and `obs`.

## References

Royer-Gaspard, P., Andreassian, V., and Thirel, G. (2021). Technical
note: PMR - a proxy metric to assess hydrological model robustness in a
changing climate. Hydrology and Earth System Sciences, 25, 5703–5716.
doi:10.5194/hess-25-5703-2021.

Pushpalatha, R.; Perrin, C.; Le Moine, N.; Andreassian, V. (2012). A
review of efficiency criteria suitable for evaluating low-flow
simulations. Journal of Hydrology, 420, 171–182.
doi:10.1016/j.jhydrol.2011.11.055.

## Author

Mauricio Zambrano Bigiarini \<mzb.devel@gmail.com\>

## Note

`obs` and `sim` have to have the same length/dimension.  

For the moving-window biases, the fixed-length window is selected before
invalid pairs are removed inside that window.

The choice of window length `k` influences the temporal scale at which
robustness is evaluated.

## See also

[`pbias`](https://hzambran.github.io/hydroGOF/reference/pbias.md),
[`NSE`](https://hzambran.github.io/hydroGOF/reference/NSE.md),
[`VE`](https://hzambran.github.io/hydroGOF/reference/VE.md),
[`JDKGE`](https://hzambran.github.io/hydroGOF/reference/JDKGE.md),
[`gof`](https://hzambran.github.io/hydroGOF/reference/gof.md),
[`ggof`](https://hzambran.github.io/hydroGOF/reference/ggof.md)

## Examples

``` r

##################
# Example 1: Looking at the difference between PMR and KGE, both with 'method=2009' 
#            and 'method=2012'

# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Simulated daily time series, initially equal to twice the observed values
sim <- 2*obs 

# KGE 2009
KGE(sim=sim, obs=obs, method="2009", out.type="full")
#> $KGE.value
#> [1] -0.4142136
#> 
#> $KGE.elements
#>     r  Beta Alpha 
#>     1     2     2 
#> 

# KGE 2012
KGE(sim=sim, obs=obs, method="2012", out.type="full")
#> $KGE.value
#> [1] 0
#> 
#> $KGE.elements
#>     r  Beta Gamma 
#>     1     2     1 
#> 

# PMR (Royer-Gaspard et al., 2021):
PMR(sim=sim, obs=obs)
#> [1] 0.05886164

##################
# Example 2: 
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'PMR' for the "best" (unattainable) case
PMR(sim=sim, obs=obs)
#> [1] 0

##################
# Example 3: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values. 
#            This random noise has more relative importance for low flows than 
#            for medium and high flows.
  
# Randomly changing the first 1826 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:1826] <- obs[1:1826] + rnorm(1826, mean=10)
ggof(sim, obs)


PMR(sim=sim, obs=obs)
#> [1] 0.3162052

if (FALSE) { # \dontrun{ 
##################
# Example 4: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' during computations.

PMR(sim=sim, obs=obs, fun=log)

# Verifying the previous value:
lsim <- log(sim)
lobs <- log(obs)
PMR(sim=lsim, obs=lobs)

##################
# Example 5: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding the Pushpalatha2012 constant
#            during computations

PMR(sim=sim, obs=obs, fun=log, epsilon.type="Pushpalatha2012")

# Verifying the previous value, with the epsilon value following Pushpalatha2012
eps  <- mean(obs, na.rm=TRUE)/100
lsim <- log(sim+eps)
lobs <- log(obs+eps)
PMR(sim=lsim, obs=lobs)

##################
# Example 6: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and adding a user-defined constant
#            during computations

eps <- 0.01
PMR(sim=sim, obs=obs, fun=log, epsilon.type="otherValue", epsilon.value=eps)

# Verifying the previous value:
lsim <- log(sim+eps)
lobs <- log(obs+eps)
PMR(sim=lsim, obs=lobs)

##################
# Example 7: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying (natural) 
#            logarithm to 'sim' and 'obs' and using a user-defined factor
#            to multiply the mean of the observed values to obtain the constant
#            to be added to 'sim' and 'obs' during computations

fact <- 1/50
PMR(sim=sim, obs=obs, fun=log, epsilon.type="otherFactor", epsilon.value=fact)

# Verifying the previous value:
eps  <- fact*mean(obs, na.rm=TRUE)
lsim <- log(sim+eps)
lobs <- log(obs+eps)
PMR(sim=lsim, obs=lobs)

##################
# Example 8: PMR for simulated values equal to observations plus random noise 
#            on the first half of the observed values and applying a 
#            user-defined function to 'sim' and 'obs' during computations

fun1 <- function(x) {sqrt(x+1)}

PMR(sim=sim, obs=obs, fun=fun1)

# Verifying the previous value
sim1 <- sqrt(sim+1)
obs1 <- sqrt(obs+1)
PMR(sim=sim1, obs=obs1)
##################
# Example 9: PMR for a two-column data frame where simulated values are equal to 
#            observations plus random noise on the first half of the observed values 

SIM <- cbind(sim, sim)
OBS <- cbind(obs, obs)

PMR(sim=SIM, obs=OBS)
} # }
```
